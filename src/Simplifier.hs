module Simplifier where

import Types

toIf :: BoolExpr -> IfExpr
toIf = undefined

normalize :: IfExpr -> NExpr
normalize = undefined

headNormalize :: NExpr -> NExpr -> NExpr -> NExpr
headNormalize = undefined

eval :: Env -> NExpr -> NExpr
eval = undefined

toBool :: NExpr -> BoolExpr
toBool = undefined


reduce :: BoolExpr -> BoolExpr
reduce = undefined
