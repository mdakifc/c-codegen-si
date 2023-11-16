module Main where

import Control.Monad.Trans.State
import Language.C.Syntax.AST
import System.Random
-- import Text.Pretty.Simple (pPrint)
import Language.C.Pretty
-- import Data.ByteString qualified as BS
-- import System.Environment (getArgs)
import Common
import GenMain

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

