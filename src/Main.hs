module Main where

import Common                    (DType (..), SProg (..), stdFuncIdents)
import Control.Monad.Trans.State
import Data.ByteString.Char8     qualified as BS
import Data.Vector               qualified as V
import GenMain
import Knobs
import Language.C.Pretty
import Language.C.Syntax.AST
import System.Random


main :: IO ()
main = do
  mKnobs <- loadKnobs "config.json"
  case mKnobs of
    Nothing -> putStrLn "Invalid Config File"
    Just knobs ->
      initStdGen >>= (BS.putStrLn . topHeader . BS.concat . replacePragma . BS.pack . show . pretty) . runGState knobs


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

runGState :: Knobs -> StdGen -> CTranslUnit
runGState = (evalState buildAST .) . initState

initState :: Knobs -> StdGen -> SProg
initState Knobs {..} g =
  -- Generate the identifiers for the standard library functions that we need
  SProg
    { maxDims = knobMaxDims
    , sizeRange = knobSizeRange
    , loopDepthRange = knobLoopDepthRange -- Vectorizable loops
    , nestedLoopRange = knobNestedLoopRange
    , noLoopRange = knobNoLoopRange
    , noOfFunctions = knobNoOfFunctions
    , expressionDepthRange = knobExpressionDepthRange -- Potentially Exponential
    , targetDTypes = knobTargetDTypes -- Target DTypes
    , stdFunctions = stdFuncIdents
      -- Variable
    , functions = mempty
    , generator = g
    , mDimArrs = V.replicate (fromEnum (maxBound :: DType) + 1) mempty
    , singletons = V.replicate (fromEnum (maxBound :: DType) + 1) mempty
    , indexVars = mempty
    , parameters = mempty
    , activeIndexes = mempty
    , lValueSingletons = V.replicate (fromEnum (maxBound :: DType) + 1) mempty
    , nId = V.length stdFuncIdents
    }
