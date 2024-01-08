module CommonGen where

import Common
import Control.Monad               (replicateM)
import Control.Monad.Trans.State
import Data.IntMap                 qualified as IntMap
import Data.Maybe                  (fromJust)
import Data.Tuple                  (swap)
import Data.Vector                 qualified as V
import Language.C.Data.Ident
import Language.C.Data.Node        (undefNode)
import Language.C.Data.Position    (nopos)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Selectors


--------------------------------------------------------------------------------
---------------------------- Generate Expressions ------------------------------
--------------------------------------------------------------------------------
-- Assumption: operations are done on the same dtype of variables

genArrayAccessExpr :: DType -> GState (CExpr, CExpr)
genArrayAccessExpr dtype = do
    -- 1. Choose an array => (ident, dimension specification)
    (ident, dimSpec) <- chooseArray dtype
    -- 2. Recursively index all the dimensions with the active indexes
    --    2.1. Add, subtract or multiply a constant with the index variable
    --    2.2. Then mod with dimension's size to keep access within the bound.
    let genIndexArrExpr :: Int -> Int -> (CExpr, CExpr) -> [DimSpec] -> GState (CExpr, CExpr)
        genIndexArrExpr _ _ completeExpr [] = pure completeExpr
        genIndexArrExpr targetDim targetKey (partialExpr, partialExprMod) (dim:rest) = do
            -- Choose an active index
            -- (indexKey, activeIndexVar) <- do
            mKI <- do
                allowDiagonalAccess' <- gets (head . allowDiagonalAccess)
                if allowDiagonalAccess'
                   then Just <$> chooseActiveIndex
                   else if length rest == targetDim
                        then Just <$> (gets (head . immediateLoopIndexes) >>= chooseKeyFromMap)
                        else do
                            let selectNonTargetIndex :: Int -> GState (Maybe (Int, ActiveIndexVar))
                                selectNonTargetIndex depth = do
                                    (k, a) <- gets activeIndexes >>= chooseKeyFromMap
                                    if k == targetKey
                                       then if depth <= 0
                                               then pure Nothing
                                               else selectNonTargetIndex (depth-1)
                                       else pure . Just $ (k, a)
                            selectNonTargetIndex 5
            constIndexVal <- execRandGen (0, 10) -- TODO
            indexOp <- chooseFromList Nothing [CAddOp, CAddOp]
            lit <- case indexOp of
                  CMulOp -> execRandGen (1, 5)
                  _      -> execRandGen (0, 5)
            -- Generates a partial expression like:
            --    <partial-expr>[((<index><op><lit>)%<size> + <size>)%<size>]
            --                   ---------- expr1 ---------
            --                   ----------------- expr2 -----------
            --                   --------------------- expr3 ---------------
            let genIndexArrExpr' = genIndexArrExpr targetDim targetKey
                indexExpr =
                    case mKI of
                        Just (_, activeIndexVar) -> CVar (activeIndexIdent activeIndexVar) undefNode
                        Nothing -> constructConstExpr constIndexVal
                indexBinOpExpr = CBinary indexOp indexExpr (constructConstExpr lit) undefNode
                -- fromJust: should be safe at this point because every dimensions have a size
                sizeExpr = constructExprFromEither $ fmap fromJust dim
                expr1 = CBinary CRmdOp indexBinOpExpr sizeExpr undefNode
                expr2 = CBinary CAddOp expr1 sizeExpr undefNode
                expr3 = CBinary CRmdOp expr2 sizeExpr undefNode
                partialExpr' = CIndex partialExprMod expr3 undefNode
            case mKI of
                Just (indexKey, _) -> updateActiveIndex indexKey lit indexOp sizeExpr
                Nothing -> pure ()
            genIndexArrExpr' (CIndex partialExpr indexBinOpExpr undefNode, partialExpr') rest
    -- Pick the target dimension of the array which should use the immediate index
    targetDim <- do
        ir <- gets isReduction
        if ir
            then pure $ length dimSpec + 1
            -- The weights for choosing each dimension from the right
            -- s^d, s^(d-1), ..., 1
            else do
                coeff <- gets weightCoeffForDims
                let noOfDimensions = length dimSpec
                -- TODO: slight chance of overflowing
                chooseFromList (Just . V.fromList $ [coeff ^ i | i <- [1 .. noOfDimensions]]) [1 .. noOfDimensions]
    targetKey <- gets (head . IntMap.keys . head . immediateLoopIndexes)
    genIndexArrExpr (length dimSpec - targetDim) targetKey (CVar ident undefNode, CVar ident undefNode) dimSpec


updateActiveIndex :: Int -> Integer -> CBinaryOp -> CExpr -> GState ()
updateActiveIndex indexKey constLit binOp arraySizeExpr = do
    let addToEnd :: ActiveIndexVar -> Maybe ActiveIndexVar
        addToEnd s =
            Just $ s { activeIndexEnd = (Right upperLimitExpr:) $ activeIndexEnd s }
        upperLimitExpr =
            case binOp of
                CAddOp -> CBinary CSubOp arraySizeExpr (constructConstExpr constLit) undefNode
                CMulOp -> CBinary CDivOp arraySizeExpr (constructConstExpr constLit) undefNode
                _ -> error $ "updateActiveIndex: Unsupported binary operation - " <> show binOp
    modify' (\s -> s { activeIndexes = IntMap.update addToEnd indexKey $ activeIndexes s })


genLValueExpr :: DType -> GState CExpr
genLValueExpr dtype = do
    p :: Int <- execRandGen(0,1)
    case p of
        -- Access
        0 -> do
            (arrayExpr, arrayExprMod) <- genArrayAccessExpr dtype
            modify' (\s -> s { expressionBucket = arrayExprMod:expressionBucket s})
            pure arrayExpr
        -- Singleton
        1 -> do
            mKI <- chooseSingleton dtype True
            case mKI of
                Just (key, ident) -> do
                    let expr = CVar ident undefNode
                    modify' $ \s -> s
                        { lValueSingletons = V.accum (flip $ IntMap.insert key) (lValueSingletons s) [(fromEnum dtype, ident)]
                        , expressionBucket = expr : expressionBucket s
                        }
                    pure expr
                Nothing -> genLValueExpr dtype
        _ -> undefined


genRValueExpr :: DType -> Int -> GState CExpr
genRValueExpr dtype noOfBinOps = do
    ops <- replicateM noOfBinOps $
            uncurry chooseFromList . swap . fmap Just $ V.unzip [(CAddOp, 20), (CSubOp, 20), (CMulOp, 10), (CXorOp, 1), (COrOp, 1), (CAndOp, 1)]
    operands <- replicateM (noOfBinOps + 1) $ do
        let chooseOperand = do
                p :: Int <- execRandGen(0, 2)
                case p of
                    -- Access
                    0 -> fst <$> genArrayAccessExpr dtype
                    -- Singleton
                    1 -> do
                        mKI <- chooseSingleton dtype True
                        case mKI of
                          Just (key, ident) -> do
                              modify' (\s -> s { lValueSingletons = V.accum (flip $ IntMap.insert key) (lValueSingletons s) [(fromEnum dtype, ident)] } )
                              pure $ CVar ident undefNode
                          Nothing             -> chooseOperand
                    -- Constant
                    2 -> constructConstExpr <$> execRandGen (0, 1000)
                    _ -> undefined
        chooseOperand
    pure $ constructBinaryExprTree ops operands

genAssignExpr :: DType -> GState CExpr
genAssignExpr dtype = do
    op <- uncurry chooseFromList . swap . fmap Just . V.unzip $
          [ (CAssignOp, 20)
          , (CMulAssOp, 1)
          , (CAddAssOp, 10)
          , (CSubAssOp, 10)
          , (CShlAssOp, 5)
          , (CShrAssOp, 5)
          , (CAndAssOp, 5)
          , (CXorAssOp, 5)
          , (COrAssOp , 5)
          ]
    case op of
      CAssignOp -> do
            lhs <- genLValueExpr dtype
            rhs <- gets expressionDepthRange >>= execRandGen >>= genRValueExpr dtype
            pure $ CAssign CAssignOp lhs rhs undefNode
      _ -> do
          modify' (\s -> s {isReduction = allowReduction s})
          lhs <- genLValueExpr dtype
          modify' (\s -> s {isReduction = False})
          rhs <- gets expressionDepthRange >>= execRandGen >>= genRValueExpr dtype
          pure $ CAssign op lhs rhs undefNode

--------------------------------------------------------------------------------
---------------------------- Generate Variables --------------------------------
--------------------------------------------------------------------------------

genHoistedVars :: Int -> GState [CDecl]
genHoistedVars 0 = pure []
genHoistedVars n = do
    cNameId <- getId
    nHVars <- gets (length . hoistedVars)
    let
        name = "n" ++ show nHVars
        ident = mkIdent nopos name cNameId
        decl = constructSingleton ident DInt (Just constructInitializerZero)
    modify' (\s -> s { hoistedVars = ident:hoistedVars s })
    (decl:) <$> genHoistedVars (n-1)


genSingletons :: DType -> Int -> GState [CDecl]
genSingletons _ 0 = pure []
genSingletons dtype n = do
    cNameId <- getId
    nSingleton <- gets (length . (V.! fromEnum dtype) . singletons)
    let
        name = "s" ++ dtypeToIdent dtype ++ show nSingleton
        ident = mkIdent nopos name cNameId
        decl = constructSingleton ident dtype (Just $ CInitExpr (constructRandomValue stdFuncIdents dtype) undefNode)
    updateSingletons dtype nSingleton ident
    (decl:) <$> genSingletons dtype (n-1)

genIndexVars :: Int -> GState [CDecl]
genIndexVars 0 = pure []
genIndexVars n = do
    cNameId <- getId
    nIndex <- gets (length . indexVars)
    let
        name = "i" ++ show nIndex
        ident = mkIdent nopos name cNameId
        decl = constructSingleton ident DInt (Just constructInitializerZero)
    updateIndexes nIndex ident
    (decl:) <$> genIndexVars (n-1)


-- Returns (Key, Declaration) of the array
genHeapArrays :: DType -> [Int] -> GState [(Int, CDecl)]
genHeapArrays _ [] = pure []
genHeapArrays dtype (dims:rest) = do
    cNameId <- getId
    key <- gets (length . (V.! fromEnum dtype) . mDimArrs)
    let
        name = "A" ++ dtypeToIdent dtype ++ show key
        ident = mkIdent nopos name cNameId
        decl = constructHeapArray ident dtype dims
    -- Assumption: the heap allocated arrays will get their sizes from the user
    updateArrs dtype key (ident, replicate dims (Right Nothing))
    ((key, decl):) <$> genHeapArrays dtype rest

constructSingleton :: Ident -> DType -> Maybe CInit -> CDecl
constructSingleton ident dtype mCInit =
    CDecl
        -- Type Specifier
        (CTypeSpec <$> dtypeToCTypeSpecifier dtype)
        -- Declarator
        [(Just $ constructSingletonDeclr ident, mCInit, Nothing)]
        undefNode

constructSingletonDeclr :: Ident -> CDeclr
constructSingletonDeclr ident =
    CDeclr (Just ident) [] Nothing [] undefNode

constructHeapArray :: Ident -> DType -> Int -> CDecl
constructHeapArray ident dtype dims =
    CDecl
        -- Type Specifier
        (CTypeSpec <$> dtypeToCTypeSpecifier dtype)
        -- Declarator
        [(Just $ constructHeapArrayDeclr ident dims, Just constructInitializerZero, Nothing)]
        undefNode

constructHeapArrayDeclr :: Ident -> Int -> CDeclr
constructHeapArrayDeclr ident nDims =
    CDeclr
        (Just ident)
        -- Pointers
        (replicate nDims $ CPtrDeclr [] undefNode)
        Nothing
        []
        undefNode

constructInitializerZero :: CInit
constructInitializerZero =
    let expr = CConst (flip CIntConst undefNode $ cInteger 0)
    in CInitExpr expr undefNode

dtypeToIdent :: DType -> String
dtypeToIdent DInt    = "I"
dtypeToIdent DUInt   = "UI"
dtypeToIdent DLong   = "L"
dtypeToIdent DULong  = "UL"
dtypeToIdent DChar   = "C"
dtypeToIdent DDouble = "D"


dtypeToCTypeSpecifier :: DType -> [CTypeSpec]
dtypeToCTypeSpecifier DInt    = [CIntType undefNode]
dtypeToCTypeSpecifier DUInt   = [CUnsigType undefNode, CIntType undefNode]
dtypeToCTypeSpecifier DLong   = [CLongType undefNode]
dtypeToCTypeSpecifier DULong  = [CUnsigType undefNode, CIntType undefNode]
dtypeToCTypeSpecifier DChar   = [CCharType undefNode]
dtypeToCTypeSpecifier DDouble = [CDoubleType undefNode]

mDtypeToCTypeDecl :: Maybe DType -> CDecl
mDtypeToCTypeDecl mDtype =
    case mDtype of
        Nothing ->
          -- void *
          CDecl
            [CTypeSpec (CVoidType undefNode)]
            [(
               Just $ CDeclr Nothing [CPtrDeclr [] undefNode] Nothing [] undefNode
             , Nothing
             , Nothing
             )]
            undefNode
        Just dtype ->
          -- <type>
          CDecl
            (CTypeSpec <$> dtypeToCTypeSpecifier dtype)
            []
            undefNode

-- TODO: Generalize it to floats and doubles
constructConstExpr :: Integer -> CExpr
constructConstExpr v = CConst (CIntConst (cInteger v) undefNode)

constructExprFromEither :: Either Int Ident -> CExpr
constructExprFromEither e = do
  case e of
    Left intLiteral -> constructConstExpr $ fromIntegral intLiteral
    Right ident     -> CVar ident undefNode

-- Must be a NonEmpty List
constructBinaryExprTree :: [CBinaryOp] -> [CExpr] -> CExpr
constructBinaryExprTree _ [x] = x
constructBinaryExprTree (op:rest) (x:xs) =
    CBinary op x (constructBinaryExprTree rest xs) undefNode
constructBinaryExprTree _ _ = error "constructBinaryExprTree: List is empty"


-- Unsafe in the C layer
constructPrintf :: StdFunctions -> String -> [CExpr] -> CExpr
constructPrintf stdFunctionIdents formatString arguments =
  let printfIdent :: Ident = stdFunctionIdents V.! fromEnum CPrintf
      formatStringExpr :: CExpr = CConst (CStrConst (cString formatString) undefNode)
  in CCall (CVar printfIdent undefNode) (formatStringExpr:arguments) undefNode

-- Unsafe in the C layer
constructFprintf :: Ident -> StdFunctions -> String -> [CExpr] -> CExpr
constructFprintf out stdFunctionIdents formatString arguments =
  let printfIdent :: Ident = stdFunctionIdents V.! fromEnum CFprintf
      formatStringExpr :: CExpr = CConst (CStrConst (cString formatString) undefNode)
      outExpr :: CExpr = CVar out undefNode
  in CCall (CVar printfIdent undefNode) (outExpr:formatStringExpr:arguments) undefNode


constructRandomValue :: StdFunctions -> DType -> CExpr
constructRandomValue stdFunctionIdents dtype =
  let
    randIdent = stdFunctionIdents V.! fromEnum CRand
    randCallExpr :: CExpr = CCall (CVar randIdent undefNode) [] undefNode
  in case dtype of
    DChar -> CBinary CRmdOp randCallExpr (constructConstExpr 256) undefNode
    -- Fixed point values
    DDouble ->
      -- Constructs the following:
      --  ((float)rand()/2147483647) * 1e6
      let
        expr1 :: CExpr = CCast (mDtypeToCTypeDecl (Just dtype)) randCallExpr undefNode
        expr2 :: CExpr = CCast (mDtypeToCTypeDecl (Just dtype)) (constructConstExpr 2147483647) undefNode
        expr3 :: CExpr = CBinary CDivOp expr1 expr2 undefNode
        expr4 :: CExpr = CBinary CMulOp expr3 (constructConstExpr 1_000_000)  undefNode
      in expr4
    _ -> randCallExpr
