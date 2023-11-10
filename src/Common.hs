{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, NumericUnderscores #-}
module Common where

import System.Random
import Data.Map qualified as Map
import Data.ByteString qualified as BS
import Control.Monad.Trans.State
import Data.Vector qualified as V
import Language.C.Data (Name (..), Ident)

-- TODO: Change the storage
data CType = CInt
    deriving (Eq, Show)

type MultiDimensionalArrays = Map.Map BS.ByteString (Ident, (V.Vector Int))
type IndexVars = Map.Map BS.ByteString (Ident, CType)
type Singletons = Map.Map BS.ByteString (Ident, CType) -- TODO: Ints for now

data SProg = SProg
  { 
    -- Potentially Constant
    maxGlobals :: Int -- Currently, arrays are generated globally
  , maxDims :: Int
  , sizeRange :: (Int, Int)
  , maxSize :: Int
  , maxFuncDepth :: Int
  , maxLoopDepth :: Int
    -- Variable
  , generator :: StdGen
  , mDimArrs :: MultiDimensionalArrays
  , indexVars :: IndexVars
  , singletons :: Singletons
  , nId :: Int
  }
  deriving (Eq, Show)

type GState a = State SProg a

execRandGen :: UniformRange a => (a,a) -> GState a
execRandGen r = do
  sprog <- get
  let g = generator sprog
      (a, g') = uniformR r g
  put $ sprog { generator = g' }
  pure a

updateGen :: StdGen -> SProg -> SProg
updateGen g' prev = prev { generator = g' }

updateArrs :: MultiDimensionalArrays -> GState ()
updateArrs m' = do
  prev <- get
  put $ prev { mDimArrs = Map.union m' (mDimArrs prev) }

updateSingletons :: Singletons -> GState ()
updateSingletons m' = do
  prev <- get
  put $ prev { singletons = Map.union m' (singletons prev) }

updateIndexVars :: IndexVars -> GState ()
updateIndexVars m' = do
  prev <- get
  put $ prev { indexVars = Map.union m' (indexVars prev) }

deleteIndexVar :: BS.ByteString -> GState ()
deleteIndexVar name = do
  prev <- get
  put $ prev { indexVars = Map.delete name (indexVars prev) }

getId :: GState Name
getId = do
  sprog <- get
  let n = nId sprog
  put $ sprog { nId = n+1 }
  pure $ Name n
    

chooseFromVMap :: (SProg -> Map.Map k (a,b)) -> GState a
chooseFromVMap f = do
  mp <- get >>= (pure . f)
  let n = Map.size mp
  i <- execRandGen (0, n-1)
  pure . fst . snd $ Map.elemAt i mp

--------------------------------------------------------------------------------
------------------------------------TEST----------------------------------------
--------------------------------------------------------------------------------

config :: StdGen -> SProg
config g = 
  SProg { 
    maxGlobals = 5
  , maxDims = 2
  , sizeRange = (100, 1000)
  , maxSize = 1000
  , maxFuncDepth = 500
  , maxLoopDepth = 5
    -- Variable
  , generator = g
  , mDimArrs = mempty
  , indexVars = mempty
  , singletons = mempty
  , nId = 0
  }
