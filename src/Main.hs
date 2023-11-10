{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Main where

import Control.Monad.Trans.State
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
import GenMain
import Common

-- GenC
main :: IO ()
main = do
    initStdGen >>= (print . pretty) . runGState

-- ParseC
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let filepath = head args
--     inp <- BS.readFile filepath
--     case (parseC inp (initPos filepath)) of
--       Right a -> pPrint a
--       Left v -> pPrint v


runGState :: StdGen -> CTranslUnit
runGState g = evalState buildAST $ config g

