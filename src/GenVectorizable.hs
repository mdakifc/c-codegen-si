module GenVectorizable where

import Common
import CommonGen
import Control.Monad
import Control.Monad.Trans.State
import Data.IntMap               qualified as IntMap
import Data.Vector               qualified as V
import Language.C.Data.Ident
import Language.C.Data.Node      (undefNode)
import Language.C.Data.Position  (nopos)
import Language.C.Syntax.AST
import Selectors

genFor :: Int -> GState CStat
genFor 1 = genVectorizableForForward
genFor nest = do
  hoistedIdent <- gets (head . hoistedVars)
  modify' (\s -> s { hoistedVars = tail . hoistedVars $ s})
  -- 0. Create scope for indexes
  modify' (\s -> s {immediateLoopIndexes = IntMap.empty : immediateLoopIndexes s})
  -- 1. Activate an index variable
  (key, _) <- activateIndexVar Nothing
  -- 2. Generate 0 or more assign statements pre- and post- assign stats
  -- preNoAssignStats <- gets loopDepthRange >>= execRandGen
  -- postNoAssignStats <- gets loopDepthRange >>= execRandGen
  -- preAssignStats :: [CStat] <- replicateM preNoAssignStats $ do
  --   dtype <- gets targetDTypes >>= chooseFromList
  --   flip CExpr undefNode . Just <$> genAssignExpr dtype
  -- postAssignStats :: [CStat] <- replicateM postNoAssignStats $ do
  --   dtype <- gets targetDTypes >>= chooseFromList
  --   flip CExpr undefNode . Just <$> genAssignExpr dtype
  -- 3. Generate the next nested loop
  nestedForStat :: CStat <- genFor (nest-1)
  -- 4. Generate the for statement
  loopStat :: CStat <- do
    indexes <- gets (IntMap.keys . head . immediateLoopIndexes) >>= (\indexes -> gets ((\m -> fmap (m IntMap.!) indexes) . activeIndexes))
    modify' $ \s -> s
      { immediateLoopIndexes = tail $ immediateLoopIndexes s}
    modify' (\s -> s { hoistedVars = hoistedIdent:hoistedVars s})
    pure $ constructFor hoistedIdent indexes (CCompound [] (CBlockStmt <$> [nestedForStat]) undefNode)
  -- 6. Deactivate Index var
  deactiveIndexVar key
  pure loopStat


{-
Goal:
  - Create a loops with arbitrary index variables.
  - Use modulo to keep access within bounds.
-}

-- (1) Loops with no nested control statements
genVectorizableForForward :: GState CStat
genVectorizableForForward = do
  -- 0. Create scope for indexes
  hoistedIdent <- gets (head . hoistedVars)
  modify' (\s -> s { hoistedVars = tail . hoistedVars $ s})
  modify' (\s -> s {immediateLoopIndexes = IntMap.empty : immediateLoopIndexes s})
  -- 1. Activate an index variable
  (key, _) <- gets strideRange >>= activateIndexVar . Just
  -- 2. Create a vectorizable loop body
  --    -- Since the state is updated with the index variable we can create the loop body
  body <- genVectorizableBlock
  -- 4. Create a label that will be replaced with the pragma call
  indexes <- gets (IntMap.keys . head . immediateLoopIndexes) >>= (\indexes -> gets ((\m -> fmap (m IntMap.!) indexes) . activeIndexes))
  stat <- constructPragmaLabel $ constructFor hoistedIdent indexes body
  -- 5. Pop scope
  modify' (\s -> s { hoistedVars = hoistedIdent:hoistedVars s})
  modify' (\s -> s {immediateLoopIndexes = tail $ immediateLoopIndexes s})
  -- 6. Deactivate Index var
  deactiveIndexVar key
  pure stat

-- Constructs a for loop
-- If the number of activeIndexes is 1, then it hoists it out of the loop body
constructFor :: Ident -> [ActiveIndexVar] -> CStat -> CStat
constructFor hoistedVar activeIndexes body =
  let
    indexExpr :: ActiveIndexVar -> CExpr
    indexExpr activeIndex = CVar (activeIndexIdent activeIndex) undefNode
    initExpr :: ActiveIndexVar -> CExpr
    initExpr activeIndex = CAssign CAssignOp (indexExpr activeIndex) (constructExprFromEither $ activeIndexStart activeIndex) undefNode
    listOfEitherToExpr :: [Either Int CExpr] -> [CExpr]
    listOfEitherToExpr xs =
      (<$> xs) $ \case
          Left intLiteral -> constructConstExpr $ fromIntegral intLiteral
          Right expr      -> expr
    minExpr :: ActiveIndexVar -> CExpr
    minExpr activeIndex =
        constructMinMaxCall stdFuncIdents True . listOfEitherToExpr $ activeIndexEnd activeIndex
    conditionExpr :: ActiveIndexVar -> CExpr -> CExpr
    conditionExpr activeIndex endExpr =
      CBinary CLeOp
        (indexExpr activeIndex)
        endExpr
        undefNode
    updateExpr :: ActiveIndexVar -> CExpr
    updateExpr activeIndex = CAssign CAddAssOp (indexExpr activeIndex) (constructExprFromEither $ activeIndexStride activeIndex) undefNode
    loopStat endExprs =
      CFor
        (Left . Just . flip CComma undefNode $ fmap initExpr activeIndexes)
        (Just . constructBinaryExprTree (repeat CLndOp) $ endExprs)
        (Just . flip CComma undefNode $ fmap updateExpr activeIndexes)
        body
        undefNode
  in case activeIndexes of
       [activeIndex] ->
          let
            hoistedVarExpr :: CExpr = CVar hoistedVar undefNode
            hoisted :: CStat = flip CExpr undefNode . Just $
              CAssign CAssignOp
                hoistedVarExpr
                (minExpr activeIndex)
                undefNode
            wrapped = CCompound [] [CBlockStmt hoisted, CBlockStmt $ loopStat [conditionExpr activeIndex hoistedVarExpr]] undefNode
           in wrapped
       _ -> loopStat $ fmap (\i -> conditionExpr i (minExpr i)) activeIndexes


-- The corresponding statement will be repeated by the `repeatFactor`
-- Effectful, since it generates an index variable but doesn't update the singleton list
genRepeatedStatement :: Int -> CStat -> GState CStat
genRepeatedStatement repeatFactor stat = do
  cNameId <- getId
  let
      name :: String = "i"
      ident :: Ident = mkIdent nopos name cNameId
      decl :: CDecl = constructSingleton ident DInt (Just constructInitializerZero)
      indexExpr :: CExpr = CVar ident undefNode
      conditionExpr :: CExpr = CBinary CLeOp indexExpr (constructConstExpr $ fromIntegral repeatFactor) undefNode
      updateExpr :: CExpr = CUnary CPostIncOp indexExpr undefNode
  pure $
    CFor
      (Right decl)
      (Just conditionExpr)
      (Just updateExpr)
      stat
      undefNode


constructPragmaLabel :: CStat -> GState CStat
constructPragmaLabel targetStat = do
  cNameId <- getId
  let name = "pragma"
      ident = mkIdent nopos name cNameId
  pure $ CLabel ident targetStat [] undefNode


{-
    Generates a block of assignments
-}
genVectorizableBlock :: GState CStat
genVectorizableBlock = do
  modify' (\s -> s
    { modAccess = False:modAccess s
    , allowDiagonalAccess = False:allowDiagonalAccess s
    })
  noOfStats <- gets loopDepthRange >>= execRandGen
  res <- ((flip (CCompound []) undefNode . fmap CBlockStmt)  <$>) . replicateM noOfStats $ do
    dtype <- gets targetDTypes >>= chooseFromList Nothing
    flip CExpr undefNode . Just <$> genAssignExpr dtype
  -- Reset lValueSingletons
  modify' (\s -> s
    { lValueSingletons = V.replicate (fromEnum (maxBound :: DType) + 1) mempty
    , modAccess = tail $ modAccess s
    , allowDiagonalAccess = tail $ allowDiagonalAccess s
    })
  return res

constructMinMaxCall :: StdFunctions -> Bool -> [CExpr] -> CExpr
constructMinMaxCall _ _ [] = error "constructMinMaxCall: number of expressions is 0."
constructMinMaxCall _ _ [expr] = expr
constructMinMaxCall stdFunctionIdents isMin args =
  let fIdent = flip CVar undefNode . (stdFunctionIdents V.!) . fromEnum $ if isMin then CMin else CMax
      -- nargs = constructConstExpr . fromIntegral $ length args
  -- in CCall (CVar (stdFunctionIdents V.! fromEnum CAp) undefNode) args undefNode
  in CCall fIdent args undefNode
