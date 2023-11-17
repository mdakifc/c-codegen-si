module Main where

import Control.Monad.Trans.State
import Language.C.Syntax.AST
import System.Random
-- import Text.Pretty.Simple (pPrint)
import Data.ByteString.Char8     qualified as BS
import Language.C.Pretty
-- import System.Environment (getArgs)
import Common
import GenMain

-- GenC
main :: IO ()
main = do
    initStdGen >>= (BS.putStrLn . topHeader . BS.concat . replacePragma . BS.pack . show . pretty) . runGState


-- Adds global header
topHeader :: BS.ByteString -> BS.ByteString
topHeader prog = "#include \"global.h\"\n" <> prog

-- Can be expensive
replacePragma :: BS.ByteString -> [BS.ByteString]
replacePragma haystack =
  let
    scalarInterpolationPragma = BS.unlines
        [ "\n#ifdef SI_COUNT"
        , "#pragma clang loop scalar_interpolation_count(SI_COUNT)"
        , "#endif"
        ]
    pat = "pragma:"
    patLen = BS.length pat
    (a, b) = BS.breakSubstring pat haystack

  in a:if BS.null b then [] else scalarInterpolationPragma:replacePragma (BS.drop patLen b)

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

