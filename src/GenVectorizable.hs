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
  -- 1. Activate an index variable
  (key, _) <- activateIndexVar
  -- 2. Generate 0 or more assign statements pre- and post- assign stats
  preNoAssignStats <- gets loopDepthRange >>= execRandGen
  postNoAssignStats <- gets loopDepthRange >>= execRandGen
  preAssignStats :: [CStat] <- replicateM preNoAssignStats $ do
    dtype <- gets targetDTypes >>= chooseFromList
    flip CExpr undefNode . Just <$> genAssignExpr dtype
  postAssignStats :: [CStat] <- replicateM postNoAssignStats $ do
    dtype <- gets targetDTypes >>= chooseFromList
    flip CExpr undefNode . Just <$> genAssignExpr dtype
  -- 3. Generate the next nested loop
  nestedForStat :: CStat <- genFor (nest-1)
  -- 4. Generate the for statement
  loopStat :: CStat <-
    flip constructFor (CCompound [] (CBlockStmt <$> (preAssignStats ++ [nestedForStat] ++ postAssignStats)) undefNode)
    <$> gets ((IntMap.! key) . activeIndexes)
  let
    -- 5. Create the compount statement
    resultingStat :: CStat = CCompound [] [CBlockStmt loopStat] undefNode
  -- 6. Deactivate Index var
  deactiveIndexVar key
  pure resultingStat


{-
Goal:
  - Create a loops with arbitrary index variables.
  - Use modulo to keep access within bounds.
-}

-- (1) Loops with no nested control statements
genVectorizableForForward :: GState CStat
genVectorizableForForward = do
  -- 1. Activate an index variable
  (key, _) <- activateIndexVar
  -- 2. Create a vectorizable loop body
  --    -- Since the state is updated with the index variable we can create the loop body
  body <- genVectorizableBlock
  -- 3. Create the for loop
  -- 4. Create a label that will be replaced with the pragma call
  stat <- gets ((IntMap.! key) . activeIndexes) >>= constructPragmaLabel . flip constructFor body
  -- 6. Deactivate Index var
  deactiveIndexVar key
  pure stat

constructFor :: ActiveIndexVar -> CStat -> CStat
constructFor activeIndex body =
  let
    indexExpr :: CExpr = CVar (activeIndexIdent activeIndex) undefNode
    initExpr :: CExpr = CAssign CAssignOp indexExpr (constructExprFromEither $ activeIndexStart activeIndex) undefNode
    conditionExpr :: CExpr =
      let
        listOfEitherToExpr :: [Either Int CExpr] -> [CExpr]
        listOfEitherToExpr xs =
          (<$> xs) $ \case
              Left intLiteral -> constructConstExpr $ fromIntegral intLiteral
              Right expr      -> expr

      in CBinary CLeOp indexExpr (constructMinMaxCall stdFuncIdents True . listOfEitherToExpr $ activeIndexEnd activeIndex) undefNode
    updateExpr :: CExpr = CAssign CAddAssOp indexExpr (constructExprFromEither $ activeIndexStride activeIndex) undefNode
    loopStat =
      CFor
        (Left $ Just initExpr)
        (Just conditionExpr)
        (Just updateExpr)
        body
        undefNode
  in loopStat

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
  modify' (\s -> s {modAccess = False:modAccess s})
  noOfStats <- gets loopDepthRange >>= execRandGen
  res <- ((flip (CCompound []) undefNode . fmap CBlockStmt)  <$>) . replicateM noOfStats $ do
    dtype <- gets targetDTypes >>= chooseFromList
    flip CExpr undefNode . Just <$> genAssignExpr dtype
  -- Reset lValueSingletons
  modify' (\s -> s {lValueSingletons = V.replicate (fromEnum (maxBound :: DType) + 1) mempty, modAccess = tail $ modAccess s})
  return res

constructMinMaxCall :: StdFunctions -> Bool -> [CExpr] -> CExpr
constructMinMaxCall _ _ [] = error "constructMinMaxCall: number of expressions is 0."
constructMinMaxCall _ _ [expr] = expr
constructMinMaxCall stdFunctionIdents isMin args =
  let fIdent = flip CVar undefNode . (stdFunctionIdents V.!) . fromEnum $ if isMin then CMin else CMax
      -- nargs = constructConstExpr . fromIntegral $ length args
  -- in CCall (CVar (stdFunctionIdents V.! fromEnum CAp) undefNode) args undefNode
  in CCall fIdent args undefNode
