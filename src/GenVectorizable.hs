module GenVectorizable where

import Common
import CommonGen
import Control.Monad
import Control.Monad.Trans.State
import Data.IntMap                 qualified as IntMap
import Data.Vector                 qualified as V
import Language.C.Data.Ident
import Language.C.Data.Node        (undefNode)
import Language.C.Data.Position    (nopos)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (cFloat)
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
    pure $ constructFor Nothing hoistedIdent indexes (CCompound [] (CBlockStmt <$> [nestedForStat]) undefNode)
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
  stat <- (\mLabel -> constructFor mLabel hoistedIdent indexes body) . Just <$> genPragmaLabel
  -- 5. Pop scope
  modify' (\s -> s { hoistedVars = hoistedIdent:hoistedVars s})
  modify' (\s -> s {immediateLoopIndexes = tail $ immediateLoopIndexes s})
  -- 6. Deactivate Index var
  deactiveIndexVar key
  pure stat

-- Constructs a for loop
-- If the number of activeIndexes is 1, then it hoists it out of the loop body
constructFor :: Maybe Ident -> Ident -> [ActiveIndexVar] -> CStat -> CStat
constructFor mLabel hoistedVar activeIndexes body =
  let
    wrapLabel stat =
      case mLabel of
        Nothing    -> stat
        Just label -> constructPragmaLabel label stat
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
            wrapped = CCompound [] [CBlockStmt hoisted, CBlockStmt . wrapLabel $ loopStat [conditionExpr activeIndex hoistedVarExpr]] undefNode
           in wrapped
       _ -> wrapLabel . loopStat $ fmap (\i -> conditionExpr i (minExpr i)) activeIndexes


-- The corresponding statement will be repeated by the `repeatFactor`
-- Effectful, since it generates an index variable but doesn't update the singleton list
genRepeatedStatement :: Int -> String -> CStat ->  GState [CBlockItem]
genRepeatedStatement repeatFactor formatString stat = do
  limitStartIdent <- gets nId >>= \n -> createIdent ("limit_start" ++ show n)
  timeLimitExpr :: CExpr <- gets timeLimit >>= (\num -> pure $ CConst (CFloatConst (cFloat num) undefNode))
  let
      limitStartDecl :: CDecl = constructClockTDecl stdFuncIdents limitStartIdent
      clockLimitStartAssign :: CStat = constructClockCallStat stdFuncIdents limitStartIdent
      execTimeIdentExpr :: CExpr = CVar (stdFuncIdents V.! fromEnum IExecTime) undefNode
      execTimeAssign :: CStat =
        let
          execTimeZeroValExpr :: CExpr = CConst (flip CFloatConst undefNode $ cFloat 0)
        in flip CExpr undefNode . Just $ CAssign CAssignOp execTimeIdentExpr execTimeZeroValExpr undefNode
      ident :: Ident = stdFuncIdents  V.! fromEnum Ii
      decl :: CDecl = constructSingleton ident DInt (Just constructInitializerZero)
      indexExpr :: CExpr = CVar ident undefNode
      repeatFactorCond :: CExpr =
        CBinary CLeOp indexExpr (constructConstExpr $ fromIntegral repeatFactor) undefNode
      currentClockExpr :: CExpr = CCall (CVar (stdFuncIdents V.! fromEnum CClock) undefNode) [] undefNode
      currentClockCycles :: CExpr = CBinary CSubOp currentClockExpr (CVar limitStartIdent undefNode) undefNode
      maxClockCycles :: CExpr = CBinary CMulOp timeLimitExpr (CVar (stdFuncIdents V.! fromEnum CCLOCKS_PER_SEC) undefNode) undefNode
      timeLimitCond :: CExpr =
        CBinary CLeOp currentClockCycles maxClockCycles undefNode
      conditionExpr :: CExpr =
        -- i < repeatFactor || (clock() - limit_start) < <time-limit>
        CBinary CLorOp repeatFactorCond timeLimitCond undefNode
      updateExpr :: CExpr = CUnary CPostIncOp indexExpr undefNode
      forStat =
        CFor
          (Right decl)
          (Just conditionExpr)
          (Just updateExpr)
          stat
          undefNode
  pure [ CBlockDecl limitStartDecl
       , CBlockStmt clockLimitStartAssign
       , CBlockStmt execTimeAssign
       , CBlockStmt forStat
       , CBlockStmt . flip CExpr undefNode . Just $
         constructFprintf
           (stdFuncIdents V.! fromEnum CStderr)
           stdFuncIdents
           formatString
           [execTimeIdentExpr]
       ]

constructPragmaLabel :: Ident -> CStat -> CStat
constructPragmaLabel ident targetStat = CLabel ident targetStat [] undefNode

genPragmaLabel :: GState Ident
genPragmaLabel = do
  cNameId <- getId
  let name = "pragma"
      ident = mkIdent nopos name cNameId
  pure ident


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
