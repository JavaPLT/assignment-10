module UnParser where

import Types

instance Show SimpleExpr where
    show (SIdent x) = x
    show (SLit True) = "T"
    show (SLit False) = "F"

instance Show BoolExpr where
    show (BLeaf x) = show x
    show (BAnd x y) = "( & " ++ show x ++ " " ++ show y ++ " )"
    show (BOr x y) = "( | " ++ show x ++ " " ++ show y ++ " )"
    show (BImplies x y) = "( > " ++ show x ++ " " ++ show y ++ " )"
    show (BNot x) = "( ! " ++ show x ++ " )"
    show (BIf x y z) = "( ? " ++ show x ++ " " ++ show y ++  " " ++ show z ++ " )"

instance Show IfExpr where
    show (ILeaf x) = show x
    show (IIf x y z) = "( ? " ++ show x ++ " " ++ show y ++  " " ++ show z ++ " )"

instance Show NExpr where
    show (NLeaf x) = show x
    show (NIf x y z) = "( ? " ++ show x ++ " " ++ show y ++  " " ++ show z ++ " )"