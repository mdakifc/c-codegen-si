module CommonGen where

import Common
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Ident
import Language.C.Data.Node (undefNode)
import Language.C.Data.Position (nopos)
import Control.Monad.Trans.State
import Data.Vector qualified as V

---- GEN --- 
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
dtypeToIdent DInt = "I"
dtypeToIdent DChar = "C"
dtypeToIdent DFloat = "F"
dtypeToIdent DDouble = "D"


dtypeToCTypeSpecifier :: DType -> CTypeSpec
dtypeToCTypeSpecifier DInt = CIntType undefNode
dtypeToCTypeSpecifier DChar = CCharType undefNode
dtypeToCTypeSpecifier DFloat = CFloatType undefNode
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
