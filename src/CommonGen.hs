module CommonGen where

import Common
import Control.Monad.Trans.State
import Data.IntMap                 qualified as IntMap
import Data.Maybe                  (fromJust)
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

genArrayAccessExpr :: DType -> GState CExpr
genArrayAccessExpr dtype = do
    -- 1. Choose an array => (ident, dimension specification)
    (ident, dimSpec) <- chooseArray dtype
    -- 2. Recursively index all the dimensions with the active indexes
    --    2.1. Add, subtract or multiply a constant with the index variable
    --    2.2. Then mod with dimension's size to keep access within the bound.
    --    TODO: Can we do something smarter to remove the mods?
    let genIndexArrExpr :: CExpr -> [DimSpec] -> GState CExpr
        genIndexArrExpr completeExpr [] = pure completeExpr
        genIndexArrExpr partialExpr (dim:rest) = do
            -- Choose an active index
            activeIndexVar <- chooseActiveIndex
            op <- chooseFromList [CAddOp, CSubOp, CMulOp]
            lit <-
                case op of
                  CMulOp -> execRandGen (1, 5)
                  _      -> execRandGen (0, 5)
            -- Generates a partial expression like:
            --    <partial-expr>[((<index><op><lit>)%<size> + <size>)%<size>]
            --                   ---------- expr1 ---------
            --                   ----------------- expr2 -----------
            --                   --------------------- expr3 ---------------
            let indexExpr = CVar (activeIndexIdent activeIndexVar) undefined
                indexBinOpExpr = CBinary op indexExpr (constructConstExpr lit) undefNode
                -- fromJust: should be safe at this point because every dimensions have a size
                sizeExpr = constructExprFromEither $ fmap fromJust dim
                expr1 = CBinary CRmdOp indexBinOpExpr sizeExpr undefNode
                expr2 = CBinary CAddOp expr1 sizeExpr undefNode
                expr3 = CBinary CRmdOp expr2 sizeExpr undefNode
                partialExpr' = CIndex partialExpr expr3 undefNode
            genIndexArrExpr partialExpr' rest

    genIndexArrExpr (CVar ident undefNode) dimSpec

genLValueExpr :: DType -> GState CExpr
genLValueExpr dtype = do
    p :: Int <- execRandGen(0,1)
    case p of
        -- Access
        0 -> genArrayAccessExpr dtype
        -- Singleton
        1 -> do
            mKI <- chooseSingleton dtype False
            case mKI of
                Just (key, ident) -> do
                    modify' (\s -> s { lValueSingletons = V.accum (flip $ IntMap.insert key) (lValueSingletons s) [(fromEnum dtype, ident)] } )
                    pure $ CVar ident undefNode
                Nothing -> genLValueExpr dtype
        _ -> undefined


genRValueExpr :: DType -> Int -> GState CExpr
genRValueExpr dtype depth = do
    p :: Int <- execRandGen(0, max 1 (min 2 depth))
    case p of
        -- Access
        0 -> genArrayAccessExpr dtype
        -- Singleton
        1 -> do
            mKI <- chooseSingleton dtype True
            case mKI of
              Just (_, singleton) -> pure $ CVar singleton undefNode
              Nothing             -> genRValueExpr dtype depth
        -- Binary Op
        2 -> do
            expr1 <- genRValueExpr dtype (depth - 1)
            expr2 <- genRValueExpr dtype (depth - 1)
            op <-
                case dtype of
                    DInt -> chooseFromList [CAddOp, CSubOp, CMulOp, CXorOp, COrOp, CAndOp]
                    DChar -> chooseFromList [CAddOp, CSubOp, CMulOp, CXorOp, COrOp, CAndOp]
                    _ -> chooseFromList [CAddOp, CSubOp, CMulOp]
            pure $ CBinary op expr1 expr2 undefNode
        _ -> undefined


genAssignExpr :: DType -> GState CExpr
genAssignExpr dtype = do
    lhs <- genLValueExpr dtype
    rhs <- gets expressionDepthRange >>= execRandGen >>= genRValueExpr dtype
    pure $ CAssign CAssignOp lhs rhs undefNode


--------------------------------------------------------------------------------
---------------------------- Generate Variables --------------------------------
--------------------------------------------------------------------------------

genSingletons :: DType -> Int -> GState [CDecl]
genSingletons _ 0 = pure []
genSingletons dtype n = do
    cNameId <- getId
    nSingleton <- gets (length . (V.! fromEnum dtype) . singletons)
    let
        name = "s" ++ dtypeToIdent dtype ++ show nSingleton
        ident = mkIdent nopos name cNameId
        decl = constructSingleton ident dtype (Just constructInitializerZero)
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
        [CTypeSpec (dtypeToCTypeSpecifier dtype)]
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
        [CTypeSpec (dtypeToCTypeSpecifier dtype)]
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
dtypeToIdent DChar   = "C"
dtypeToIdent DFloat  = "F"
dtypeToIdent DDouble = "D"


dtypeToCTypeSpecifier :: DType -> CTypeSpec
dtypeToCTypeSpecifier DInt    = CIntType undefNode
dtypeToCTypeSpecifier DChar   = CCharType undefNode
dtypeToCTypeSpecifier DFloat  = CFloatType undefNode
dtypeToCTypeSpecifier DDouble = CDoubleType undefNode

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
            [CTypeSpec (dtypeToCTypeSpecifier dtype)]
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

