{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, NumericUnderscores #-}
module Common where

import System.Random
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Control.Monad.Trans.State
import Data.Vector qualified as V

-- TODO: Change the storage
data CType = CInt
    deriving (Eq, Show)

type MultiDimensionalArrays = Map.Map BS.ByteString (V.Vector Int)
type IndexVars = Map.Map BS.ByteString CType
type Singletons = Map.Map BS.ByteString CType -- TODO: Ints for now

data Meta = Meta 
  { maxGlobals :: Int -- Currently, only arrays are generated globally
  , minSize :: Int
  , maxSize :: Int
  , maxDims :: Int
  , maxFuncDepth :: Int
  , maxLoopDepth :: Int
  }
  deriving (Eq, Show)

-- TODO: Potentially change it to ReaderT and State with Meta and Scope data types
--
--
--
data ScopeMeta = ScopeMeta 
  { variables :: (MultiDimensionalArrays, IndexVars, Singletons) -- Can be modified
  , meta :: Meta -- Should be a Reader
  , generator :: StdGen
  }
 deriving (Show, Eq)

type GState a = State ScopeMeta a


execRandGen :: UniformRange a => (a,a) -> GState a
execRandGen r = do
    s <- get
    let g = generator s
        (a, g') = uniformR r g
    put $ ScopeMeta (variables s) (meta s) g'
    pure a

updateArrs :: MultiDimensionalArrays -> (MultiDimensionalArrays, IndexVars, Singletons) -> (MultiDimensionalArrays, IndexVars, Singletons)
updateArrs m1 (m2, i, s) = (Map.union m1 m2, i, s)
updateIndexVars :: IndexVars -> (MultiDimensionalArrays, IndexVars, Singletons) -> (MultiDimensionalArrays, IndexVars, Singletons)
updateIndexVars i1 (m, i2, s) = (m, Map.union i1 i2, s)
updateSingletons :: Singletons -> (MultiDimensionalArrays, IndexVars, Singletons) -> (MultiDimensionalArrays, IndexVars, Singletons)
updateSingletons s1 (m, i, s2) = (m, i, Map.union s1 s2)

