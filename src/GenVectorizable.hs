module GenVectorizable where

import Common
import CommonGen
import Control.Monad
import Control.Monad.Trans.State
import Language.C.Data.Ident
import Language.C.Data.Node      (undefNode)
import Language.C.Data.Position  (nopos)
import Language.C.Syntax.AST
import Selectors

genFor :: Int -> GState CStat
genFor 1 = genVectorizableForForward
genFor nest = do
  -- 1. Activate an index variable
  (key, activeIndex) <- activateIndexVar
  -- 2. Generate 0 or more assign statements pre- and post- assign stats
  preNoAssignStats <- execRandGen (0, 2) -- TODO: Change hard-coded constant
  postNoAssignStats <- execRandGen (0, 2) -- TODO: Change hard-coded constant
  preAssignStats :: [CStat] <- replicateM preNoAssignStats $ do
    dtype <- gets targetDTypes >>= chooseFromList
    flip CExpr undefNode . Just <$> genAssignExpr dtype
  postAssignStats :: [CStat] <- replicateM postNoAssignStats $ do
    dtype <- gets targetDTypes >>= chooseFromList
    flip CExpr undefNode . Just <$> genAssignExpr dtype
  -- 3. Generate the next nested loop
  nestedForStat :: CStat <- genFor (nest-1)
  let
    -- 4. Generate the for statement
    loopStat :: CStat = constructFor activeIndex nestedForStat
    -- 5. Create the compount statement
    resultingStat :: CStat = CCompound [] (CBlockStmt <$> (preAssignStats ++ [loopStat] ++ postAssignStats)) undefNode
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
  (key, activeIndex) <- activateIndexVar
  -- 2. TODO: Create a vectorizable loop body
  --    -- Since the state is updated with the index variable we can create the loop body
  body <- genVectorizableBlock
  -- 3. Create the for loop
  -- 4. Create a label that will be replaced with the pragma call
  stat <- constructPragmaLabel $ constructFor activeIndex body
  -- 6. Deactivate Index var
  deactiveIndexVar key
  pure stat

constructFor :: ActiveIndexVar -> CStat -> CStat
constructFor activeIndex body =
  let
    indexExpr :: CExpr = CVar (activeIndexIdent activeIndex) undefNode
    initExpr :: CExpr = CAssign CAssignOp indexExpr (constructExprFromEither $ activeIndexStart activeIndex) undefNode
    conditionExpr :: CExpr = CBinary CLeOp indexExpr (constructExprFromEither $ activeIndexEnd activeIndex) undefNode
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
  noOfStats <- execRandGen (1, 5) -- TODO: Change hard-coded constant
  ((flip (CCompound []) undefNode . fmap CBlockStmt)  <$>) . replicateM noOfStats $ do
    dtype <- gets targetDTypes >>= chooseFromList
    flip CExpr undefNode . Just <$> genAssignExpr dtype
