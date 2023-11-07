module Grammar where

import System.Random

-- data Program = Body
    
data Stat = 
      VariableDef
    | Assignment
    | For
    | Control
    | VLoopF
    -- | VLoopB
 deriving (Eq, Ord, Enum, Show)

data VExpr = 
        Constant
      | Identifier
      | Access
      | BinOp
      | VOp
 deriving (Eq, Ord, Enum, Show)
    
-- gen :: RandomGen g => g -> 
