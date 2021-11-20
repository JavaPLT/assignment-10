module Types where

data SimpleExpr = 
      SLit Bool 
    | SIdent String
    deriving Eq

data BoolExpr = 
        BLeaf SimpleExpr
    |   BNot BoolExpr
    |   BAnd BoolExpr BoolExpr
    |   BOr BoolExpr BoolExpr
    |   BImplies BoolExpr BoolExpr
    |   BIf BoolExpr BoolExpr BoolExpr
    deriving Eq

data IfExpr =
    ILeaf SimpleExpr
  | IIf IfExpr IfExpr IfExpr
  deriving Eq

-- Normalized expressions
data NExpr
  = NIf SimpleExpr NExpr NExpr
  | NLeaf SimpleExpr
  deriving Eq

type Env = [(String, Bool)]