module GenMain where


import Control.Monad
import Control.Monad.Trans.State
import Data.IntMap qualified as IntMap
import Language.C.Data.Ident
import Language.C.Data.Node (undefNode)
import Language.C.Data.Position (nopos)
import Language.C.Syntax.AST
import Common
import GenFunc


buildAST :: GState CTranslUnit
buildAST = do
    -- decls <- declareGlobals
    funcDefs :: [CFunDef] <- gets noOfFunctions >>= flip replicateM genFunc
    main <- createMain
    -- pure $ CTranslUnit ((CDeclExt <$> decls) ++ [CFDefExt main]) undefNode
    pure $ CTranslUnit (CFDefExt <$> funcDefs ++ [main]) undefNode

createMain :: GState CFunDef
createMain = do
    cNameId <- getId
    body <- genMainBody
    let ident = mkIdent nopos "main" cNameId
    pure $ constructMainFn ident body

genMainBody :: GState CStat
genMainBody = do 
    -- Calls each defined function
    stdFunctionIdents <- gets stdFunctions
    fnCallStats :: [CStat] <- gets (IntMap.elems . functions) >>= traverse (uncurry (genFuncCallBlock stdFunctionIdents))

    pure $ CCompound [] (CBlockStmt <$> fnCallStats) undefNode

constructMainFn :: Ident -> CStat -> CFunDef
constructMainFn ident body = 
    CFunDef
      -- Return type
      [CTypeSpec (CIntType undefNode)] -- Return type
      -- Declarator
      (CDeclr
         -- Identifier
         (Just ident)
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
      -- Declaration (Parameters)
      []
      -- Statement (Block)
      body
      undefNode

-- Declares a random number of global arrays
-- declareGlobals :: GState [CDecl]
-- declareGlobals = do
--     sprog <- get
--     -- Generate the number of global arrays
--     nGlobals <- execRandGen (1, maxGlobals sprog)
--     -- Generate dims for each array
--     dims <- sequence . take nGlobals . repeat $ execRandGen (1, maxDims sprog)
--     -- Generate size for each dimension for each array
--     -- sizess <- sequence $ map (\d ->  sequence . take d . repeat $ execRandGen (sizeRange sprog)) dims
--     let sizess = map (\d ->  take d . repeat $ maxSize sprog) dims
--     -- Generate the arrays
--     concat <$> sequence [genIndexVar >>= (pure . (:[]) . snd), genArrs sizess, genSingletons 2]


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
-- genArrs :: [[Int]] -> GState [CDecl]
-- genArrs [] = pure []
-- genArrs (sizes:rest) = do
--     cNameId <- getId
--     sprog <- get
--     let 
--         name = "A" ++ show (Map.size $ mDimArrs sprog)
--         ident = mkIdent nopos name cNameId
--         decl = CDecl [CTypeSpec (CIntType undefNode)]
--                [(Just $ CDeclr (Just $ ident) 
--                         ((\size -> CArrDeclr [] (CArrSize False (CConst (CIntConst (cInteger $ fromIntegral size) undefNode))) undefNode) <$> sizes)
--                         Nothing 
--                         []
--                         undefNode
--                 , Nothing
--                 , Nothing)] undefNode
--     updateArrs $ Map.insert (BS.pack name) (ident, V.fromList sizes) (mDimArrs sprog)
--     genArrs rest >>= (pure . (decl:))
    

