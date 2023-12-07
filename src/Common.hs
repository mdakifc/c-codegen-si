module Common where

import Control.Monad.Trans.State
import Data.Aeson                (FromJSON (..))
import Data.Aeson                qualified as Ae
import Data.Aeson.Types          (prependFailure, typeMismatch)
import Data.IntMap               qualified as IntMap
import Data.Vector               qualified as V
import Language.C.Data           (Ident, Name (..))
import Language.C.Data.Ident     (mkIdent)
import Language.C.Data.Position  (nopos)
import Language.C.Syntax.AST     (CExpr)
import System.Random

-- Standard Library Functions
data StdFunc = CMalloc | CPrintf | CRand | CScanf | CGetTimeOfDay | CStructTimeVal
             | CMin | CMax | CAp
    deriving (Eq, Show, Enum, Bounded)

-- Defined types
data DType = DInt | DUInt | DLong | DULong | DChar
    deriving (Eq, Show, Enum, Bounded)

instance FromJSON DType where
  parseJSON (Ae.String s) =
    case s of
      "int"   -> pure DInt
      "uint"  -> pure DUInt
      "long"  -> pure DLong
      "ulong" -> pure DULong
      "char"  -> pure DChar
      _       -> fail "Invalid type value."
  parseJSON invalid =
    prependFailure "parsing DType failed, " (typeMismatch "String" invalid)


data ActiveIndexVar = ActiveIndexVar
  { activeIndexIdent  :: Ident
  , activeIndexStart  :: Either Int Ident
  , activeIndexEnd    :: [Either Int CExpr]
  , activeIndexStride :: Either Int Ident
  }
  deriving (Show)

-- Id of Array, Dimensions of array with either a constant size or an identifier to an integer
type DimSpec = Either Int (Maybe Ident)
type ArrSpec = (Ident, [DimSpec])

-- dtype -> [identifier (, Associated data)?]
type MultiDimensionalArrays = V.Vector (IntMap.IntMap ArrSpec)
type Singletons = V.Vector (IntMap.IntMap Ident)

type IndexVars = IntMap.IntMap Ident
type ActiveIndexVars = IntMap.IntMap ActiveIndexVar
type Parameters = IntMap.IntMap Ident
type Functions = IntMap.IntMap (Ident, Parameters)

-- StdFunc -> Ident
type StdFunctions = V.Vector Ident

data SProg = SProg
  {
    -- Potentially Constant
    maxDims              :: Int
  , sizeRange            :: (Int, Int)
  , loopDepthRange       :: (Int, Int)
  , nestedLoopRange      :: (Int, Int)
  , noLoopRange          :: (Int, Int)
  , expressionDepthRange :: (Int, Int) -- Potentially Exponential
  , noOfFunctions        :: Int
  , targetDTypes         :: V.Vector DType
  , stdFunctions         :: StdFunctions
  , allowReduction       :: Bool
    -- Variable
  , functions            :: Functions
  , generator            :: StdGen
  , mDimArrs             :: MultiDimensionalArrays
  , indexVars            :: IndexVars
  , singletons           :: Singletons
  , parameters           :: Parameters
  , activeIndexes        :: ActiveIndexVars
  , lValueSingletons     :: Singletons
  , nId                  :: Int
  , repeatFactor         :: Int
  -- Dynamic Gen states
  , modAccess            :: [Bool]
  , allowDiagonalAccess  :: [Bool]
  , immediateLoopIndexes :: [ActiveIndexVars] -- TODO: Change it to NonEmpty List
  , isReduction          :: Bool
  , expressionBucket     :: [CExpr]
  }
  deriving (Show)

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

-- Saves the function in functions
-- And clears out the other variables
popFunctionScope :: Int -> Ident -> GState ()
popFunctionScope key ident = do
  -- Save the function in functions
  modify' $ \sProg ->
    sProg { functions = IntMap.insert key (ident, parameters sProg) (functions sProg)
          }
  -- Clear out the other variables
  modify' $ \sProg ->
    sProg { mDimArrs = mempty
          , indexVars = mempty
          , singletons = mempty
          , parameters = mempty
          }

createIdent :: String -> GState Ident
createIdent idName = mkIdent nopos idName <$> getId


getId :: GState Name
getId = do
  sprog <- get
  let n = nId sprog
  put $ sprog { nId = n+1 }
  pure $ Name n

stdFuncIdents :: StdFunctions
stdFuncIdents =
    V.generate
      (fromEnum (maxBound :: StdFunc) + 1)
      (\i -> mkIdent nopos (stdFuncName $ toEnum i) (Name i))

stdFuncName :: StdFunc -> String
stdFuncName v =
  case v of
    CMalloc        -> "bumpAllocate"
    CPrintf        -> "printf"
    CRand          -> "rand"
    CScanf         -> "scanf"
    CGetTimeOfDay  -> "gettimeofday"
    CStructTimeVal -> "timeval"
    CMin           -> "MIN"
    CMax           -> "MAX"
    CAp            -> "ap"
