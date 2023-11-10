{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, NumericUnderscores, ScopedTypeVariables #-}
-- (1) Start with defining the global arrays.
-- (2) Create the main function.

module GenMain where


import Control.Monad
import Control.Monad.Trans.State
import Data.Map qualified as Map
import Data.ByteString.Char8 qualified as BS
import Data.Vector qualified as V
import Language.C.Data.Ident (mkIdent)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Position (nopos)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Common
import GenFor


buildAST :: GState CTranslUnit
buildAST = do
    decls <- declareGlobals
    main <- createMain
    pure $ CTranslUnit ((CDeclExt <$> decls) ++ [CFDefExt main]) undefNode


createMain :: GState CFunDef
createMain = do
    cNameId <- getId
    nstats :: Int <- get >>= execRandGen . ((,) 10 . maxFuncDepth)
    stats <- replicateM nstats $ genStat 3
    pure $ 
      CFunDef
        -- Return type
        [CTypeSpec (CIntType undefNode)] -- Return type
        -- Declarator
        (CDeclr
           -- Identifier
           (Just $ mkIdent nopos "main" cNameId)
           [ CFunDeclr
               ( Right
                   ( []
                   , False
                   )
               ) []
               undefNode
           ]
           Nothing
           []
           undefNode
        )
        -- Declaration
        []
        -- Statement (Block)
        ( CCompound [] (fmap CBlockStmt stats) undefNode )
        undefNode

-- Declares a random number of global arrays
declareGlobals :: GState [CDecl]
declareGlobals = do
    sprog <- get
    -- Generate the number of global arrays
    nGlobals <- execRandGen (1, maxGlobals sprog)
    -- Generate dims for each array
    dims <- sequence . take nGlobals . repeat $ execRandGen (1, maxDims sprog)
    -- Generate size for each dimension for each array
    -- sizess <- sequence $ map (\d ->  sequence . take d . repeat $ execRandGen (sizeRange sprog)) dims
    let sizess = map (\d ->  take d . repeat $ maxSize sprog) dims
    -- Generate the arrays
    concat <$> sequence [genIndexVar >>= (pure . (:[]) . snd), genArrs sizess, genSingletons 2]


genSingletons :: Int -> GState [CDecl]
genSingletons 0 = pure []
genSingletons n = do
    cNameId <- getId
    sprog <- get
    let 
      name = "s" ++ show (Map.size $ singletons sprog)
      ident = mkIdent nopos name cNameId
      decl = CDecl [CTypeSpec (CIntType undefNode)]
             [(Just $ CDeclr (Just $ ident) 
                      []
                      Nothing 
                      []
                      undefNode
              , Nothing
              , Nothing
              )] 
              undefNode
    updateSingletons $ Map.insert (BS.pack name) (ident, CInt) (singletons sprog)
    genSingletons (n-1) >>= (pure . (decl:))

-- Structure: 
-- CDecl 
--      [CDeclarationSpecifier a]
--      [
--          ( Maybe (CDeclarator a)
--          , Maybe (CInitializer a)
--          , Maybe (CExpression a)
--          )
--      ] a
-- CDeclr 
--      (Maybe Ident) 
--      [CDerivedDeclarator a] 
--      (Maybe (CStringLiteral a)) 
--      [CAttribute a] a
-- Takes a list of [Sizes] for each array
genArrs :: [[Int]] -> GState [CDecl]
genArrs [] = pure []
genArrs (sizes:rest) = do
    cNameId <- getId
    sprog <- get
    let 
        name = "A" ++ show (Map.size $ mDimArrs sprog)
        ident = mkIdent nopos name cNameId
        decl = CDecl [CTypeSpec (CIntType undefNode)]
               [(Just $ CDeclr (Just $ ident) 
                        ((\size -> CArrDeclr [] (CArrSize False (CConst (CIntConst (cInteger $ fromIntegral size) undefNode))) undefNode) <$> sizes)
                        Nothing 
                        []
                        undefNode
                , Nothing
                , Nothing)] undefNode
    updateArrs $ Map.insert (BS.pack name) (ident, V.fromList sizes) (mDimArrs sprog)
    genArrs rest >>= (pure . (decl:))
    

