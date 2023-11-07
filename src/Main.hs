{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Main where

import Control.Monad (void)
import System.Random
import Language.C.Data.Name
import Language.C.Data.Ident (mkIdent)
import Language.C.Syntax.AST 
import Language.C.Syntax.Ops
import Language.C.Parser (parseC)
import Language.C.Data.Node 
import Language.C.Data.Position (nopos, initPos)
import Text.Pretty.Simple (pPrint)
import Language.C.Pretty
import Data.ByteString qualified as BS
import System.Environment (getArgs)
import Gen (runGState)

main :: IO ()
main = do
    args <- getArgs
    let filepath = head args
    inp <- BS.readFile filepath
    -- pPrint $ parseC inp nopos
    case (parseC inp (initPos filepath)) of
        Right a -> pPrint $ a
        Left v -> pPrint v
    -- pPrint genFor
    -- print . pretty $ genFor
    -- g <- initStdGen
    -- void . sequence $ fmap (print . pretty) (runGState g)


-- CFor 
--      (Either (Maybe (CExpression a)) (CDeclaration a)) 
--      (Maybe (CExpression a))
--      (Maybe (CExpression a)) 
--      (CStatement a) 
--      a

placeholder :: NodeInfo -- TODO 
placeholder = undefNode

exprAssign :: CExpr
exprAssign = CAssign CAssignOp (CVar a placeholder) (CVar b placeholder) placeholder
    where
        a = mkIdent nopos "a" (Name 1)
        b = mkIdent nopos "b" (Name 2)


