module Common where

import System.Random
import Control.Monad.Trans.State
import Data.Vector qualified as V
import Data.IntMap qualified as IntMap
import Language.C.Data (Name (..), Ident)
import Language.C.Data.Ident (mkIdent)
import Language.C.Data.Position (nopos)

-- Standard Library Functions
data StdFunc = CMalloc | CPrintf | CRand
    deriving (Eq, Show, Enum, Bounded)

-- Defined types
data DType = DInt | DChar | DFloat | DDouble
    deriving (Eq, Show, Enum, Bounded)

-- Id of Array, Dimensions of array with either a constant size or an identifier to an integer
type DimSpec = Either Int (Maybe Ident)
type ArrSpec = (Ident, [DimSpec])

-- dtype -> [identifier (, Associated data)?]
type MultiDimensionalArrays = V.Vector (IntMap.IntMap ArrSpec)
type Singletons = V.Vector (IntMap.IntMap Ident)
type IndexVars = IntMap.IntMap Ident
type Parameters = IntMap.IntMap Ident
type StdFunctions = V.Vector Ident

data SProg = SProg
  { 
    -- Potentially Constant
    maxGlobals :: Int -- Currently, arrays are generated globally
  , maxDims :: Int
  , sizeRange :: (Int, Int)
  , maxSize :: Int
  , maxScalars :: Int
  , maxFuncDepth :: Int
  , maxLoopDepth :: Int
  , noOfFunctions :: Int
  , stdFunctions :: StdFunctions
    -- Variable
  , nFunctions :: Int
  , generator :: StdGen
  , mDimArrs :: MultiDimensionalArrays
  , indexVars :: IndexVars
  , singletons :: Singletons
  , parameters :: Parameters
  , activeIndexes :: IndexVars
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

updateSingletons :: DType -> Int -> Ident -> GState ()
updateSingletons dtype key sing = do
  modify' (\s -> s { singletons = V.accum (flip $ IntMap.insert key) (singletons s) [(fromEnum dtype, sing)] } )

updateArrs :: DType -> Int -> ArrSpec -> GState ()
updateArrs dtype key arr = do
  modify' (\s -> s { mDimArrs = V.accum (flip $ IntMap.insert key) (mDimArrs s) [(fromEnum dtype, arr)] })

updateIndexes :: Int -> Ident -> GState ()
updateIndexes key index = do
  modify' (\s -> s { indexVars = IntMap.insert key index $ indexVars s } )

updateParams :: Int -> Ident -> GState ()
updateParams key param = do
  modify' (\s -> s { parameters = IntMap.insert key param $ parameters s })


getId :: GState Name
getId = do
  sprog <- get
  let n = nId sprog
  put $ sprog { nId = n+1 }
  pure $ Name n

-- chooseFromVMap :: (SProg -> Map.Map k (a,b)) -> GState a
-- chooseFromVMap f = do
--   mp <- f <$> get
--   let n = Map.size mp
--   i <- execRandGen (0, n-1)
--   pure . fst . snd $ Map.elemAt i mp

stdFuncName :: StdFunc -> String
stdFuncName v = 
  case v of
    CMalloc -> "malloc"
    CPrintf -> "printf"
    CRand -> "rand"

--------------------------------------------------------------------------------
------------------------------------TEST----------------------------------------
--------------------------------------------------------------------------------

config :: StdGen -> SProg
config g =
  -- Generate the identifiers for the standard library functions that we need
  let standardFunctions' = V.generate (fromEnum (maxBound :: StdFunc) + 1) (\i -> mkIdent nopos (stdFuncName $ toEnum i) (Name i))
  in SProg 
    { maxGlobals = 5
    , maxDims = 2
    , sizeRange = (100, 1000)
    , maxSize = 1000
    , maxScalars = 10
    , maxFuncDepth = 500
    , maxLoopDepth = 5
    , noOfFunctions = 10
    , stdFunctions = standardFunctions'
      -- Variable
    , nFunctions = 0
    , generator = g
    , mDimArrs = V.replicate (fromEnum (maxBound :: DType) + 1) mempty
    , singletons = V.replicate (fromEnum (maxBound :: DType) + 1) mempty
    , indexVars = mempty
    , parameters = mempty
    , activeIndexes = mempty
    , nId = V.length standardFunctions'
    }
