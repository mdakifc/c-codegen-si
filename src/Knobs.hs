module Knobs where

import Common          (DType)
import Data.Aeson      (FromJSON (..), withObject, (.:))
import Data.Aeson      qualified as Ae
import Data.ByteString qualified as BS

data Knobs = Knobs
  {
    -- Potentially Constant
    knobMaxDims              :: Int
  , knobSizeRange            :: (Int, Int)
  , knobLoopDepthRange       :: (Int, Int)
  , knobNestedLoopRange      :: (Int, Int)
  , knobNoLoopRange          :: (Int, Int)
  , knobExpressionDepthRange :: (Int, Int) -- Potentially Exponential
  , knobNoOfFunctions        :: Int
  , knobTargetDTypes         :: [DType]
  }
  deriving (Eq, Show)

instance FromJSON Knobs where
  parseJSON = withObject "Knobs" $ \v1 -> do
      val <- v1 .: "knobs"
      flip (withObject "Knobs") val $ \v -> Knobs
        <$> (v .: "maxDims")
        <*> v .: "sizeRange"
        <*> v .: "loopDepthRange"
        <*> v .: "nestedLoopRange"
        <*> v .: "noLoopRange"
        <*> v .: "expressionDepthRange"
        <*> v .: "noOfFunctions"
        <*> v .: "targetDTypes"

loadKnobs :: FilePath -> IO (Maybe Knobs)
loadKnobs = (Ae.decodeStrict <$>) . BS.readFile

