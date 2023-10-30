{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Main where

import Language.C.Syntax.AST
import Language.C.Parser (parseC)
import Language.C.Data.Position (nopos, initPos)
import Text.Pretty.Simple (pPrint)
import Language.C.Pretty (prettyUsingInclude)
import Data.ByteString qualified as BS
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filepath = head args
    inp <- BS.readFile filepath
    -- pPrint $ parseC inp nopos
    case (parseC inp (initPos filepath)) of
        Right a -> pPrint . fmap (const 0) $ a
        Left v -> pPrint v
