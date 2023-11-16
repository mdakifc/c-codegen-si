{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, NumericUnderscores, ScopedTypeVariables #-}
-- CFor 
--      (Either (Maybe (CExpression a)) (CDeclaration a)) 
--      (Maybe (CExpression a))
--      (Maybe (CExpression a)) 
--      (CStatement a) 
--      a
module GenFor where

import Data.ByteString.Char8 qualified as BS
import Common
import Data.Map qualified as Map
import Language.C.Syntax.AST
import Language.C.Data
import Control.Monad.Trans.State
import Language.C.Syntax.Constants

-- A[i]
exprAccess :: GState CExpr
exprAccess = do
    -- Choose a random array
    -- Choose a random index variable
    -- Emit
    
    arr <- chooseFromVMap mDimArrs
    i <- chooseFromVMap indexVars
    pure $ CIndex (CVar arr undefNode) (CVar i undefNode) undefNode

exprLvalue :: GState CExpr
exprLvalue = do
    p :: Int <- execRandGen(0,1)
    case p of
        -- Access
        0 -> exprAccess
        -- Reduction
        1 -> chooseFromVMap singletons >>= (pure . flip CVar undefNode)
        _ -> undefined

exprRvalue :: Int -> GState CExpr
exprRvalue depth = do
    p :: Int <- execRandGen(0, max 1 (min 2 depth))
    case p of
        -- Access
        0 -> exprAccess
        -- Singletons
        1 -> chooseFromVMap singletons >>= (pure . flip CVar undefNode)
        -- Binary Op
        2 -> exprBinOp (depth - 1)
        _ -> undefined


-- CAssign CAssignOp (CExpression a) (CExpression a) a
-- Note: CAssignOp type has a data constructor CAssignOP which generates (=)
exprAssign :: GState CExpr
exprAssign = do
    lhs <- exprLvalue
    rhs <- exprRvalue 2
    pure $ CAssign CAssignOp lhs rhs undefNode

exprBinOp :: Int -> GState CExpr
exprBinOp depth = do
    op <- execRandGen(0, 2) >>= (pure . ((!!) [CAddOp, CSubOp, CMulOp]))
    expr1 <- exprRvalue (depth - 1)
    expr2 <- exprRvalue (depth - 1)
    pure $ CBinary op expr1 expr2 undefNode


loopCondition :: Ident -> Integer -> CExpr
loopCondition i n = CBinary CLeOp (CVar i undefNode) (CConst (CIntConst (cInteger n) undefNode)) undefNode

increment :: Ident -> CExpr
increment i = CUnary CPostIncOp (CVar i undefNode) undefNode

genStat :: Int -> GState CStat
genStat nest = do
    p <- execRandGen (0, max 0 (min 1 nest))
    case p of
      -- Assign
      0 -> exprAssign >>= (pure . flip CExpr undefNode . Just)
      -- For
      1 -> genFor (nest - 1)
      _ -> undefined

genFor :: Int -> GState CStat
genFor nest = do
    ((name, ident), initDecl) <- genIndexVar
    n <- get >>= (pure . maxSize)
    stat <- genStat (nest - 1)
    deleteIndexVar name
    pure $ 
        CFor (Right initDecl) 
         (Just $ loopCondition ident $ fromIntegral n) 
         (Just $ increment ident) 
         (stat)
         undefNode
    
