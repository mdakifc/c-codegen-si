module Block where

import Common

data BlockElem = BFor | BAssign
    deriving (Eq, Enum, Bounded, Show)

-- Builds a random block
buildBlock :: Int -> GState [CCompoundBlockItem NodeInfo] -- Will emit a CCompound statement
buildBlock n
    | n <= 0 = pure []
    | otherwise = do
        c <- execRandGen (minBound :: BlockElem, maxBound)
        -- Choose between
        -- (1) CBlockStat 
        --   (1.a) For block
        --   (1.b) If block
        --   (1.c) An assignment
        -- (2) CBlockDecl -> It will declare a Singleton
        elem <- 
            case c of
              BFor -> genFor
              -- TODO: BIf -> genIf
              BAssign -> genAssign
              -- TODO: BDecl -> undefined
        rest <- buildBlock (n-1)
        return $ elem:rest

genAssign :: GState CExpr
genAssign = undefined

genFor :: GState CStat
genFor = 
    -- Create an index variable
    indexName <- createIndexVar
    ScopeMeta (arrs, ixs, singles) _ _ <- get
    let nameIndex = sum $ fmap M.size [arrs, ixs, singles]
    pure $ 
        CLabel 
            (mkIdent nopos ("Label_" ++ indexName) $ Name nameIndex) -- TODO: ERROR
            CFor (Right $
                    CDeclr 
                        (Just . mkIdent nopos name . Name $ nameIndex+1)
                 )
    
    
genFor = CFor (Left (Just exprAssign)) Nothing Nothing (CBreak placeholder) placeholder

genIf :: GState CStat
genIf = undefined

genDecl :: GState CStat
genDecl = undefined

createIndexVar :: GState String
createIndexVar = 
    ScopeMeta (arrs, ixs, singles) m g <- get
    let n = Map.size ixs
        name = "i" ++ show n
    put $ ScopeMeta (arrs, Map.insert (B.pack name) CInt ixs, singles) m g
    pure name
