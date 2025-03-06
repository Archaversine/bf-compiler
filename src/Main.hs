{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}

module Main where

import Data.Data
import Data.List.NonEmpty
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
data IR 
    = LeftBy Integer IR
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
toIR (MLeft:xs) = LeftBy 1 (toIR xs)
toIR (MRight:xs) = RightBy 1 (toIR xs)
toIR (BInc:xs) = IncBy 1 (toIR xs)
toIR (BDec:xs) = DecBy 1 (toIR xs)
toIR (Out:xs) = Output (toIR xs)
toIR (In:xs) = Input (toIR xs)
toIR (BFWhile body:xs) = Loop (toIR body) (toIR xs)
toIR [] = EndIR

mergeNeighbors :: IR -> IR
mergeNeighbors (LeftBy  a (LeftBy  b xs)) = LeftBy  (a + b) xs
mergeNeighbors (RightBy a (RightBy b xs)) = RightBy (a + b) xs
mergeNeighbors (LeftBy  a (RightBy b xs)) = LeftBy  (a - b) xs
mergeNeighbors (RightBy a (LeftBy  b xs)) = RightBy (a - b) xs
mergeNeighbors (IncBy a (IncBy b xs))     = IncBy   (a + b) xs
mergeNeighbors (DecBy a (DecBy b xs))     = DecBy   (a + b) xs
mergeNeighbors (IncBy a (DecBy b xs))     = IncBy   (a - b) xs
mergeNeighbors (DecBy a (IncBy b xs))     = DecBy   (a - b) xs
mergeNeighbors x = x

removeRedundant :: IR -> IR
removeRedundant (LeftBy 0 xs) = xs
removeRedundant (RightBy 0 xs) = xs
removeRedundant (IncBy 0 xs) = xs
removeRedundant (DecBy 0 xs) = xs
removeRedundant (Loop EndIR xs) = xs
removeRedundant x = x

optimize :: IR -> IR
optimize = transform removeRedundant . transform mergeNeighbors

toStmt :: IR -> [Stmt]
toStmt (LeftBy x xs)  = Expr (AssignOp AssignSub (Ident "curr") (LitInt x)) : toStmt xs
toStmt (RightBy x xs) = Expr (AssignOp AssignAdd (Ident "curr") (LitInt x)) : toStmt xs
toStmt (IncBy x xs)   = Expr (AssignOp AssignAdd (Index (Ident "tape") (Ident "curr")) (LitInt x)) : toStmt xs
toStmt (DecBy x xs)   = Expr (AssignOp AssignSub (Index (Ident "tape") (Ident "curr")) (LitInt x)) : toStmt xs
toStmt (Output xs)    = Expr (Funcall (Ident "printf") [LitString "%c", Index (Ident "tape") (Ident "curr")]) : toStmt xs
toStmt (Input xs)      = Expr (AssignOp Assign (Index (Ident "tape") (Ident "curr")) (Funcall (Ident "getchar") [])) : toStmt xs
toStmt (Loop body xs) = While (Index (Ident "tape") (Ident "curr")) (toStmt body) : toStmt xs
toStmt EndIR = []

toTransUnit :: [Stmt] -> TransUnit
toTransUnit body = TransUnit [] [FunDef Nothing (TypeSpec Int) "main" [] [decTape, decCurr] body]
    where decTape = VarDecln Nothing (Array (TypeSpec Char) (Just (LitInt 1000))) "tape" (Just (InitList (singleton (InitItem Nothing (InitExpr (LitInt 0))))))                                                          
          decCurr = VarDecln Nothing (TypeSpec Int) "curr" (Just (InitExpr (LitInt 0)))

toC :: IR -> TransUnit
toC = toTransUnit . toStmt

main :: IO ()
main = getArgs >>= \case 
    [infile, outfile] -> (parse parseBFFile "<stdin>" <$> readFile infile) >>= \case 
        Left err -> putStrLn (errorBundlePretty err)
        Right xs -> writeFile outfile $ "#include <stdio.h>\n\n" ++ show (pretty $ translate $ toC $ optimize $ toIR xs)
    _ -> putStrLn "Usage: <infile> <outfile>"
