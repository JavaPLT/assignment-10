module Parser (parseBoolExpr, parseIfExpr, parseNExpr) where

import NanoParsec
import Types

import Data.Char (isAlphaNum)
import Control.Monad
import Control.Applicative

parseBoolExpr :: String -> BoolExpr
parseBoolExpr = runParser boolExpr

parseIfExpr :: String -> IfExpr
parseIfExpr = runParser ifExpr

parseNExpr :: String -> NExpr
parseNExpr = runParser nExpr

boolExpr :: Parser BoolExpr
boolExpr = bsimpExpr <|> neg <|> band <|> bor <|> implies <|> bif

ifExpr :: Parser IfExpr
ifExpr = isimpExpr <|> iif

nExpr :: Parser NExpr
nExpr = nsimpExpr <|> nif

bsimpExpr :: Parser BoolExpr
bsimpExpr = BLeaf <$> simpExpr

isimpExpr :: Parser IfExpr
isimpExpr = ILeaf <$> simpExpr

nsimpExpr :: Parser NExpr
nsimpExpr = NLeaf <$> simpExpr

simpExpr :: Parser SimpleExpr
simpExpr = slit <|> sident

slit :: Parser SimpleExpr
slit =  (reserved "T"  >> (pure $ SLit True))
    <|> (reserved "F"  >> (pure $ SLit False))


sident :: Parser SimpleExpr
sident = SIdent <$> (token $ some $ satisfy isAlphaNum) 

neg :: Parser BoolExpr
neg = parens $ do
    reserved "!"
    BNot <$> boolExpr

band :: Parser BoolExpr
band = parens $ do
    reserved "&"
    BAnd <$> boolExpr <*> boolExpr

bor :: Parser BoolExpr
bor = parens $ do
    reserved "|"
    BOr <$> boolExpr <*> boolExpr

implies :: Parser BoolExpr
implies = parens $ do
    reserved ">"
    BImplies <$> boolExpr <*> boolExpr

bif :: Parser BoolExpr
bif = parens $ do
    reserved "?"
    BIf <$> boolExpr <*> boolExpr <*> boolExpr


iif :: Parser IfExpr
iif = parens $ do
    reserved "?"
    IIf <$> ifExpr <*> ifExpr <*> ifExpr

nif :: Parser NExpr
nif = parens $ do
    reserved "?"
    NIf <$> simpExpr <*> nExpr <*> nExpr