module GenFunc where

import           Common
import           CommonGen
import           Control.Applicative         ((<|>))
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.IntMap                 as IntMap
import           Data.Maybe                  (fromJust)
import           Data.Traversable
import qualified Data.Vector                 as V
import           GenVectorizable
import           Language.C.Data.Ident
import           Language.C.Data.Node        (undefNode)
import           Language.C.Data.Position    (nopos)
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants

{-
Goal:
  - Create functions f1..fn based on #noOfFunctions
  - Each function defines:
  -      - Function parameters <=> Array Specification.
           - We can do it after constructing the block.
           - i.e. Keep a map for the function parameters and build over it.
           - Use it to create the function signature and clear it.
  -      - c Index Variables (stack allocated)
  -          c = arrays * dims[i]
  -      - b Scalars (stack allocated)
  -      - a Arrays (heap allocated) + initialization to random values
           - Multidimensional arrays must be allocated with nested for loops.
  - TODO:
    1. flattened arrays => Use 1D arrays with row major indexing.
-}


{-
   Generate a function call block as:
   ```
   {
      int p1, ..., pn;
      scanf("%d %d .. %d", &p1, ..., &pn);
      fm(p1, ..., pn);
   }
   ```
-}
genFuncCallBlock :: StdFunctions -> Ident -> Parameters -> GState CStat
genFuncCallBlock stdFunctionIdents fnIdent params = do
  let argumentIdents = IntMap.elems params
  decls :: [CBlockItem] <- ((CBlockDecl <$>) <$>) . for argumentIdents $ \pIdent -> do
    pure $ constructSingleton pIdent DInt Nothing
  let
    -- Call to scanf
    scanfCall :: CBlockItem = CBlockStmt  . flip CExpr undefNode . Just $ constructScanf stdFunctionIdents argumentIdents
    -- Function call
    fnCall :: CBlockItem = CBlockStmt . flip CExpr undefNode . Just $
      CCall (CVar fnIdent undefNode) (flip CVar undefNode <$> argumentIdents) undefNode
  -- reset singletons
  modify' (\s -> s { singletons = mempty })
  pure $ CCompound [] (decls ++ [scanfCall, fnCall]) undefNode

genFunc :: GState CFunDef
genFunc = do
    cNameId <- getId
    key <- gets (IntMap.size . functions)

    -- Generate the body
    body <- genFuncBody
    params <- gets parameters
    let
        name = "f" ++ show key
        ident = mkIdent nopos name cNameId
        funcDef = constructFunc ident params body

    -- Remove the local scope
    popFunctionScope key ident
    pure funcDef

constructFunc :: Ident -> Parameters -> CStat -> CFunDef
constructFunc ident params body =
    CFunDef
     -- Return type
     [CTypeSpec (CVoidType undefNode)] -- Void return type
     -- Declarator
     (CDeclr
        -- Identifier
        (Just ident)
        -- Function Declarator Specifier
        [ CFunDeclr
            ( Right
                (
                  -- Parameters
                  constructParams params
                  -- IsVariadic
                , False
                )
            ) []
            undefNode
        ]
        Nothing
        -- CAttributes: Not necessary for us
        []
        undefNode
     )
     -- Declaration
     []
     body
     undefNode

constructParams :: Parameters -> [CDecl]
constructParams params = (\ident -> constructSingleton ident DInt Nothing) <$> IntMap.elems params

genFuncBody :: GState CStat
genFuncBody = do
  -- For each type:
  targetDTypeValues <- gets targetDTypes
  topStats :: [[CBlockItem]] <- for targetDTypeValues $ \dtype -> do
    -- Generate Singletons
    nSingletons <- execRandGen (2, 5)
    singletonDecls :: [CBlockItem] <- (CBlockDecl <$>) <$> genSingletons dtype nSingletons
    -- Generate Arrays
    nArrays <- execRandGen (2, 5)
    dims <- replicateM nArrays $ execRandGen (1,3)
    (arrKeys, arrDecls) :: ([Int], [CBlockItem]) <- fmap (CBlockDecl <$>) . unzip <$> genHeapArrays dtype dims
    -- Generate Indexes
    --    = Number of dimensions of all the arrays
    let nIndexVars = sum dims
    indexDecls :: [CBlockItem] <- (CBlockDecl <$>) <$> genIndexVars nIndexVars
    -- Allocate memory for arrays
    allocAndInit ::  [CBlockItem] <- (concat <$>) . for arrKeys $ fmap (CBlockStmt <$>) . genAllocateAndInitialize dtype
    -- TODO: Wrap with timing call
    pure $ singletonDecls ++ arrDecls ++ indexDecls ++ allocAndInit
  body :: [CBlockItem] <- (fmap CBlockStmt <$>) $ replicateM 4 genVectorizableForForward
    -- pure $ concatMap CBlockDecl [singletonDecls]
  pure $ CCompound [] (concat topStats ++ body) undefNode

-- Description: Allocates memory and initializes the ith array with the given dtype.
-- Effectful: Update parameters and array dims if applicable (Nothing -> Just <ident>)
genAllocateAndInitialize :: DType -> Int -> GState [CStat]
genAllocateAndInitialize dtype key = do
  -- We know if an array as d non-constant size dimensions, then
  -- we will need to have d parameters that map to those dimensions
  arrSpec@(arrIdent, dims) <- gets ((IntMap.! key) . (V.! fromEnum dtype) . mDimArrs)
  paramIdents :: [Maybe Ident] <- for dims $ \d -> do
    -- Effectful: Updates parameters
    case d of
      Left _ -> pure Nothing
      Right Nothing -> do
          cNameId <- getId
          nParam <- gets (length . parameters)
          let name = "p" ++ show nParam
              pIdent = mkIdent nopos name cNameId
          updateParams nParam pIdent
          pure $ Just pIdent
      Right (Just _) -> error "Partially defined Array." -- should not be the case, since at this point any heap allocated does not have param ids
  let dims' = zipWith (\pIdent dim -> fmap (pIdent <|>) dim) paramIdents dims
  -- Update the dimensions of the array
  updateArrs dtype key (dims' <$ arrSpec)
  -- Choose indexes
  indexes <- gets (take (length dims') . IntMap.elems . indexVars)
  stdFunctionIdents <- gets stdFunctions
  pure $ constructAllocateAndInitialize stdFunctionIdents dtype (CVar arrIdent undefNode) (zip indexes dims')

-- Arguments:
--  (1) DType
--  (2) partialArrExpr => <id>[.]..[.] with (k < d) index ops
--  (3) [(Index identifiers, dimension specification)]
constructAllocateAndInitialize :: StdFunctions -> DType -> CExpr -> [(Ident, DimSpec)] -> [CStat]
-- This case should never occur, since an array must have at least 1 dimension
constructAllocateAndInitialize _ _ _ [] = error "Dimension-less array."
-- This case can't happen either, since each dimension should have an associated size at this point
constructAllocateAndInitialize _ _ _ ((_, Right Nothing):_) = error "A dimension of the array does not have an associated size."
constructAllocateAndInitialize stdFunctionIdents dtype partialArrExpr [(indexIdent, dimSpec)] =
  let
    -- 1. Allocate space for the type:
    --    - i.e. <id>((d-1)-dimensional access) = malloc(sizeof (<type>) * <dim-size>);
    allocLhs = partialArrExpr
    -- Here fromJust is safe
    allocRhs = constructMalloc stdFunctionIdents (Just dtype) (fromJust <$> dimSpec)
    allocStat :: CStat = flip CExpr undefNode . Just $ CAssign CAssignOp allocLhs allocRhs undefNode
    -- 2. Assign values to the array
    --    i.e. construct a for loop with `indexIdent` and assign values
    -- initExpr: <index-ident> = 0
    indexExpr = CVar indexIdent undefNode
    initExpr :: CExpr = CAssign CAssignOp indexExpr (constructConstExpr 0) undefNode
    conditionExpr :: CExpr =
      let
        condLhs = indexExpr
        condRhs =
          case dimSpec of
            Left intLiteral    -> constructConstExpr $ fromIntegral intLiteral
            Right (Just ident) -> CVar ident undefNode
      in CBinary CLeOp condLhs condRhs undefNode
    updateExpr :: CExpr = CUnary CPostIncOp indexExpr undefNode
    assignStat =
      let
        assignLhs :: CExpr = CIndex partialArrExpr indexExpr undefNode
        assignRhs :: CExpr = constructRandomValue stdFunctionIdents dtype
      in flip CExpr undefNode . Just $ CAssign CAssignOp assignLhs assignRhs undefNode
    assignFor :: CStat =
      CFor (Left (Just initExpr)) -- init
        (Just conditionExpr) -- condition
        (Just updateExpr) -- update
        assignStat -- Assign
        undefNode
  in [allocStat, assignFor]

constructAllocateAndInitialize stdFunctionIdents dtype partialArrExpr ((indexIdent, dimSpec):rest) =
  let
    -- 1. Allocate space for the pointer to array:
    --    - i.e. <id>((d-1)-dimensional access) = malloc(sizeof (<type>) * <dim-size>);
    allocLhs = partialArrExpr
    -- Here fromJust is safe
    allocRhs = constructMalloc stdFunctionIdents Nothing (fromJust <$> dimSpec)
    allocStat :: CStat = flip CExpr undefNode . Just $ CAssign CAssignOp allocLhs allocRhs undefNode
    -- 2. Loop over the next dimension
    --    - i.e. construct a for loop with `indexIdent` and recurse with updated partialArrExpr
    indexExpr = CVar indexIdent undefNode
    initExpr :: CExpr = CAssign CAssignOp indexExpr (constructConstExpr 0) undefNode
    conditionExpr :: CExpr =
      let
        condLhs = indexExpr
        condRhs =
          case dimSpec of
            Left intLiteral    -> constructConstExpr $ fromIntegral intLiteral
            Right (Just ident) -> CVar ident undefNode
      in CBinary CLeOp condLhs condRhs undefNode
    updateExpr :: CExpr = CUnary CPostIncOp indexExpr undefNode
    partialArrExpr' = CIndex partialArrExpr indexExpr undefNode
    blockStat :: CStat =
      CCompound []
      (CBlockStmt <$> constructAllocateAndInitialize stdFunctionIdents dtype partialArrExpr' rest)
      undefNode
    assignFor :: CStat =
      CFor (Left (Just initExpr)) -- init
        (Just conditionExpr) -- condition
        (Just updateExpr) -- update
        blockStat -- block
        undefNode
  in [allocStat, assignFor]

constructRandomValue :: StdFunctions -> DType -> CExpr
constructRandomValue stdFunctionIdents dtype =
  let
    randIdent = stdFunctionIdents V.! fromEnum CRand
    randCallExpr :: CExpr = CCall (CVar randIdent undefNode) [] undefNode
  in case dtype of
    DInt -> randCallExpr
    DChar -> CBinary CRmdOp randCallExpr (constructConstExpr 256) undefNode
    -- Fixed point values
    _ ->
      -- Constructs the following:
      --  ((float)rand()/2147483647) * 1e6
      let
        expr1 :: CExpr = CCast (mDtypeToCTypeDecl (Just dtype)) randCallExpr undefNode
        expr2 :: CExpr = CCast (mDtypeToCTypeDecl (Just dtype)) (constructConstExpr 2147483647) undefNode
        expr3 :: CExpr = CBinary CDivOp expr1 expr2 undefNode
        expr4 :: CExpr = CBinary CMulOp expr3 (constructConstExpr 1_000_000)  undefNode
      in expr4

-- Construct a malloc call
-- If mDtype is Nothing then it allocates with the size of of a void pointer
constructMalloc :: StdFunctions -> Maybe DType -> Either Int Ident -> CExpr
constructMalloc stdFunctionIdents mDtype eSize =
  let mallocIdent = stdFunctionIdents V.! fromEnum CMalloc
      lhs = constructSizeOf mDtype
      rhs =
        case eSize of
          Left intLiteral -> constructConstExpr $ fromIntegral intLiteral
          Right ident     -> CVar ident undefNode
      expr = CBinary CMulOp lhs rhs undefNode
  in CCall (CVar mallocIdent undefNode) [expr] undefNode

constructScanf :: StdFunctions -> [Ident] -> CExpr
constructScanf stdFunctionIdents varIdents =
  let scanfIdent :: Ident = stdFunctionIdents V.! fromEnum CScanf
      formatString :: CExpr = CConst . flip CStrConst undefNode . cString . unwords $ replicate (length varIdents) "%d"
      arguments :: [CExpr] = flip (CUnary CAdrOp) undefNode . flip CVar undefNode <$> varIdents
  in CCall (CVar scanfIdent undefNode) (formatString:arguments) undefNode

-- Well it is not guaranteed by the C standard that all pointer sizes are equal,
-- but in most modern system all pointer sizes are usually equal. So, for
-- simplicity we consider all pointer size to be equal to sizeof (void *)
constructSizeOf :: Maybe DType -> CExpr
constructSizeOf mDtype = CSizeofType (mDtypeToCTypeDecl mDtype) undefNode

