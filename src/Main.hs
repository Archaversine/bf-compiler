{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}

module Main where

import Control.Monad

import Data.Data
import Data.Generics.Uniplate.Data
import Data.Functor (($>))
import Data.Void (Void)

import Language.C99.Pretty (pretty)
import Language.C99.Simple

import Text.Megaparsec
import Text.Megaparsec.Char

import System.Environment

type Parser = Parsec Void String

data BF = MLeft | MRight | BInc | BDec | Out | In | BFWhile [BF] deriving Show
data IR = LeftBy Integer IR
        | RightBy Integer IR
        | IncBy Integer IR
        | DecBy Integer IR
        | Output IR
        | Input IR
        | Loop IR IR -- Loop <body> <next>
        | EndIR deriving (Data, Typeable, Show)

parseBF :: Parser BF
parseBF = choice [ char '>' $> MRight
                 , char '<' $> MLeft
                 , char '+' $> BInc
                 , char '-' $> BDec
                 , char '.' $> Out
                 , char ',' $> In
                 , BFWhile <$> (char '[' *> space *> many (parseBF <* space) <* char ']') 
                 ]

parseBFFile :: Parser [BF]
parseBFFile = many (parseBF <* space) <* eof

toIR :: [BF] -> IR
toIR (MLeft :xs) = LeftBy  1 (toIR xs)
toIR (MRight:xs) = RightBy 1 (toIR xs)
toIR (BInc  :xs) = IncBy   1 (toIR xs)
toIR (BDec  :xs) = DecBy   1 (toIR xs)
toIR (Out   :xs) = Output  (toIR xs)
toIR (In    :xs) = Input   (toIR xs)
toIR (BFWhile body:xs) = Loop (toIR body) (toIR xs)
toIR [] = EndIR

mergeNeighbors :: IR -> Maybe IR
mergeNeighbors (LeftBy  a (LeftBy  b xs)) = Just $ LeftBy  (a + b) xs
mergeNeighbors (RightBy a (RightBy b xs)) = Just $ RightBy (a + b) xs
mergeNeighbors (LeftBy  a (RightBy b xs)) = Just $ LeftBy  (a - b) xs
mergeNeighbors (RightBy a (LeftBy  b xs)) = Just $ RightBy (a - b) xs
mergeNeighbors (IncBy a (IncBy b xs))     = Just $ IncBy   (a + b) xs
mergeNeighbors (DecBy a (DecBy b xs))     = Just $ DecBy   (a + b) xs
mergeNeighbors (IncBy a (DecBy b xs))     = Just $ IncBy   (a - b) xs
mergeNeighbors (DecBy a (IncBy b xs))     = Just $ DecBy   (a - b) xs
mergeNeighbors _ = Nothing

removeRedundant :: IR -> Maybe IR
removeRedundant (LeftBy 0 xs) = Just xs
removeRedundant (RightBy 0 xs) = Just xs
removeRedundant (IncBy 0 xs) = Just xs
removeRedundant (DecBy 0 xs) = Just xs
removeRedundant (Loop EndIR xs) = Just xs
removeRedundant _ = Nothing

optimize :: IR -> IR
optimize = rewrite (\ir -> mergeNeighbors ir `mplus` removeRedundant ir)

toStmt :: IR -> [Stmt]
toStmt (LeftBy x xs)  = Expr (AssignOp AssignSub (Ident "curr") (LitInt x)) : toStmt xs
toStmt (RightBy x xs) = Expr (AssignOp AssignAdd (Ident "curr") (LitInt x)) : toStmt xs
toStmt (IncBy x xs)   = Expr (AssignOp AssignAdd (Index (Ident "tape") (Ident "curr")) (LitInt x)) : toStmt xs
toStmt (DecBy x xs)   = Expr (AssignOp AssignSub (Index (Ident "tape") (Ident "curr")) (LitInt x)) : toStmt xs
toStmt (Output xs)    = Expr (Funcall (Ident "printf") [LitString "%c", Index (Ident "tape") (Ident "curr")]) : toStmt xs
toStmt (Input xs)     = Expr (AssignOp Assign (Index (Ident "tape") (Ident "curr")) (Funcall (Ident "getchar") [])) : toStmt xs
toStmt (Loop body xs) = While (Index (Ident "tape") (Ident "curr")) (toStmt body) : toStmt xs
toStmt EndIR = []

toTransUnit :: [Stmt] -> TransUnit
toTransUnit body = TransUnit [] [FunDef Nothing (TypeSpec Int) "main" [] [decTape, decCurr] body]
    where decTape = VarDecln Nothing (Array (TypeSpec Char) (Just (LitInt 1000))) "tape" (Just (InitList (pure (InitItem Nothing (InitExpr (LitInt 0))))))                                                          
          decCurr = VarDecln Nothing (TypeSpec Int) "curr" (Just (InitExpr (LitInt 0)))

toC :: IR -> TransUnit
toC = toTransUnit . toStmt

main :: IO ()
main = getArgs >>= \case 
    [infile, outfile] -> (parse parseBFFile "<stdin>" <$> readFile infile) >>= \case 
        Left err -> putStrLn (errorBundlePretty err)
        Right xs -> writeFile outfile $ "#include <stdio.h>\n\n" ++ show (pretty $ translate $ toC $ optimize $ toIR xs)
    _ -> putStrLn "Usage: <infile> <outfile>"
