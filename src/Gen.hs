{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, NumericUnderscores #-}
-- (1) Start with defining the global arrays.
-- (2) Create the main function.
-- (3) 

module Gen where

import System.Random
import Control.Monad.Trans.State
import Data.Map qualified as Map
import Data.ByteString.Char8 qualified as BS
import Data.Vector qualified as V
import Language.C.Data.Ident (mkIdent)
import Language.C.Data.Name
import Language.C.Data.Node (undefNode)
import Language.C.Data.Position (nopos)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Common

config :: StdGen -> ScopeMeta
config g = ScopeMeta (mempty, mempty, mempty) m g
    where
        m = Meta 5 100 10_000 3 5 5 -- TODO: Config

runGState :: StdGen -> CTranslUnit
runGState g = evalState buildAST $ config g

buildAST :: GState CTranslUnit
buildAST = do
    decls <- declareGlobals
    main <- createMain
    pure $ CTranslUnit ((CDeclExt <$> decls) ++ [CFDefExt main]) undefNode

createMain :: GState CFunDef
createMain = 
    pure $ 
      CFunDef
        -- Return type
        [CTypeSpec (CIntType undefNode)] -- Return type
        -- Declarator
        (CDeclr
           -- Identifier
           (Just . mkIdent nopos "main" $ Name 0)
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
        ( CCompound [] [] undefNode )
        undefNode


declareGlobals :: GState [CDecl]
declareGlobals = do
    scopeMeta <- get
    let mg = (maxGlobals . meta) scopeMeta
        md = (maxDims . meta) scopeMeta
        minS = (minSize . meta) scopeMeta
        maxS = (maxSize . meta) scopeMeta
    nGlobals <- execRandGen (1, mg)
    dims <- sequence . take nGlobals . repeat $ execRandGen (1, md)
    sizess <- sequence $ map (\d ->  sequence . take d . repeat $ execRandGen (minS, maxS)) dims
    let (arrs, decls) = genArrs $ zip [1..nGlobals] sizess
    scopeMeta' <- get
    put $ ScopeMeta (updateArrs arrs $ variables scopeMeta')  (meta scopeMeta') (generator scopeMeta')
    return decls

-- Annotations are the ones that we care about
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

-- TODO: Generalize
genArrs :: [(Int, [Int])] -> (MultiDimensionalArrays, [CDecl])
genArrs [] = (Map.empty, [])
genArrs ((i, sizes):rest) = -- TODO: multi-dim
    let 
        name = "A" ++ show i
        decl = CDecl [CTypeSpec (CIntType undefNode)] 
               [(Just $ CDeclr (Just . mkIdent nopos name $ Name i) 
                        ((\size -> CArrDeclr [] (CArrSize False (CConst (CIntConst (cInteger $ fromIntegral size) undefNode))) undefNode) <$> sizes)
                        Nothing 
                        []
                        undefNode
                , Nothing
                , Nothing)] undefNode
        (arrs, decls) = genArrs rest
    in (Map.insert (BS.pack name) (V.fromList sizes) arrs, decl:decls)
    

