module Selectors where

import Common
import Control.Monad.Trans.State
import Data.IntMap               qualified as IntMap
import Data.Maybe
import Data.Vector               qualified as V
import Language.C.Data.Ident

{-
   Activates an index variable from the IndexVars list
    - Activates = assigns (start, end, stride)
    - The values of the parameter is either:
        - (1) Integer Literal
              - start: [0, 10] -- Can be Uniform
              - end: [10, 10_000] -- TODO: Would want to use a skewed-left RV
              - stride: [1, 5] -- Can be Uniform
        - (2) Scalar Identifier (TODO)
              - start: [Index, ?]
              - end: [dimension sizes]
              - stride: [Index, ?]
-}
activateIndexVar :: GState (Int, ActiveIndexVar)
activateIndexVar = do
    (key, ident) <- gets indexVars >>= chooseKeyFromMap
    -- Remove index from index variable list
    modify' (\s -> s { indexVars = IntMap.delete key (indexVars s) } )
    -- TODO: Currently just doing (1)
    startVal <- execRandGen (0, 10)
    endVal <- execRandGen (1000, 50_000)
    strideVal <- execRandGen (1, 5)
    let activeIndexVar = ActiveIndexVar ident (Left startVal) [Left endVal] (Left strideVal)
    -- Add it to the active index variable list
    modify' (\s -> s
        { activeIndexes = IntMap.insert key activeIndexVar (activeIndexes s)
        , immediateLoopIndexes =
            let f []     = error "activateIndexVar: Scope is empty"
                f (x:xs) = IntMap.insert key activeIndexVar x:xs
            in f $ immediateLoopIndexes s
        } )
    pure (key, activeIndexVar)


deactiveIndexVar :: Int -> GState ()
deactiveIndexVar key = do
    ident <- gets (activeIndexIdent . (IntMap.! key) . activeIndexes)
    -- Remove the active index variable from the active list
    modify' (\s -> s { activeIndexes = IntMap.delete key (activeIndexes s) } )
    -- Add it back to the index variable list
    updateIndexes key ident


chooseKeyFromMap :: IntMap.IntMap a -> GState (Int, a)
chooseKeyFromMap m = do
    let keys :: V.Vector Int = V.fromList $ IntMap.keys m
    k <- chooseFromList Nothing keys
    pure (k, m IntMap.! k)

chooseSingleton :: DType -> Bool -> GState (Maybe (Int, Ident))
chooseSingleton dtype skipExistingRValues = do
    singletons' <- gets ((V.! fromEnum dtype) . singletons)
    if skipExistingRValues
        then do
            lValueSingletons' <- gets ((V.! fromEnum dtype) . lValueSingletons)
            let w =  IntMap.difference singletons' lValueSingletons'
            if IntMap.size w == 0 then pure Nothing else Just <$> chooseKeyFromMap w
        else Just <$> chooseKeyFromMap singletons'

chooseArray :: DType -> GState ArrSpec
chooseArray dtype = do
    (_, arrSpec) <- gets ((V.! fromEnum dtype) . mDimArrs) >>= chooseKeyFromMap
    pure arrSpec

chooseActiveIndex :: GState (Int, ActiveIndexVar)
chooseActiveIndex = do
    gets activeIndexes >>= chooseKeyFromMap


-- Assumption: the length of pdf and xs are the same
chooseFromList :: Maybe (V.Vector Int) -> V.Vector a -> GState a
chooseFromList Nothing xs = (xs V.!) <$> execRandGen(0, V.length xs - 1)
chooseFromList (Just pdf) xs =
    let cdf = V.scanl1 (+) pdf in do
        value <- execRandGen (0, V.maximum cdf)
        pure . (xs V.!) . fromMaybe (V.length xs - 1) $ V.findIndex (>value) cdf

