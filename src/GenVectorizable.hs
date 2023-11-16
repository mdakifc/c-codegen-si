module GenVectorizable where

import           Common
import           CommonGen
import           Control.Monad
import           Control.Monad.Trans.State
import           Language.C.Data.Ident
import           Language.C.Data.Node      (undefNode)
import           Language.C.Data.Position  (nopos)
import           Language.C.Syntax.AST
import           Selectors

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
  -- 4. Create a label that will be replaced with the pragma call
  stat <- constructPragmaLabel loopStat
  -- 6. Deactivate Index var
  deactiveIndexVar key
  pure stat

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
