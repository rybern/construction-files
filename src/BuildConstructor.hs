{-# LANGUAGE OverloadedStrings #-}
module BuildConstructor
  (
    Constructor (..)
  , EvalError (..)
  , ParseError (..)
  , buildValue
  , buildValueFromFile
  , Symbol
  )
where

import Data.Text
import Data.Text.IO as Text
import qualified Data.Map as Map
import ParseConstructor
import Control.Monad

-- everything in this project badly needs renaming

data Constructor s = C0 s
                   | SimpleString (Text -> Constructor s)
                   | SimpleInt (Integer -> Constructor s)
                   | SimpleFloat (Double -> Constructor s)
                   | SimpleBool (Bool -> Constructor s)
                   | Composer (s -> Constructor s)

newtype EvalError = EvalError Text deriving Show

buildValue :: [(Symbol, (Constructor s))]
           -> Text
           -> Either ParseError (Either EvalError s)
buildValue constructors =
  (buildValueFromParsed (Map.fromList constructors) <$>) . parseConstructors

buildValueFromFile :: [(Symbol, (Constructor s))]
                   -> FilePath
                   -> IO (Either ParseError (Either EvalError s))
buildValueFromFile constructors =
  ((buildValueFromParsed (Map.fromList constructors) <$>) <$>) . parseConstructorFile

buildValueFromParsed :: Map.Map Symbol (Constructor s)
                     -> ( [(Symbol, (Symbol, [Expression]))]
                        , (Symbol, [Expression]))
                     -> Either EvalError s
buildValueFromParsed constructors ([], final) = buildSingleValue constructors final
buildValueFromParsed constructors ((var, constructor):restAssignments, final) = do
  when (var `Map.member` constructors) . Left . EvalError $
    "The variable name " `append` var `append` "is already in use"
  value <- buildSingleValue constructors constructor
  let constructors' = Map.insert var (C0 value) constructors
  buildValueFromParsed constructors' (restAssignments, final)

buildSingleValue :: Map.Map Symbol (Constructor s)
                 -> (Symbol, [Expression])
                 -> Either EvalError s
buildSingleValue constructors (sym, exprs) = case sym `Map.lookup` constructors of
                                               Just c -> consumeExpressions constructors c (sym, exprs)
                                               Nothing -> Left . EvalError $
                                                 "Unknown constructor or variable: " `append` sym

consumeExpressions :: Map.Map Symbol (Constructor s)
                   -> Constructor s
                   -> (Symbol, [Expression])
                   -> Either EvalError s
consumeExpressions _ (C0 s) (_, []) = Right s
consumeExpressions _ (C0 s) (sym, _) = Left . EvalError $
  "Too many arguments to constructor " `append` sym

consumeExpressions cs (SimpleString strFn) (sym, ((Str str):rest)) =
  consumeExpressions cs (strFn str) (sym, rest)
consumeExpressions _ (SimpleString _) (sym, (_:_)) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a string where another type was given"
consumeExpressions _ (SimpleString _) (sym, []) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a string where nothing was given (too few arguments)"

consumeExpressions cs (SimpleInt intFn) (sym, ((Itr i):rest)) =
  consumeExpressions cs (intFn i) (sym, rest)
consumeExpressions _ (SimpleInt _) (sym, (_:_)) = Left . EvalError $
  "Constructor " `append` sym `append` " requires an integer where another type was given"
consumeExpressions _ (SimpleInt _) (sym, []) = Left . EvalError $
  "Constructor " `append` sym `append` " requires an integer where nothing was given (too few arguments)"

consumeExpressions cs (SimpleFloat floatFn) (sym, ((Flt x):rest)) =
  consumeExpressions cs (floatFn x) (sym, rest)
consumeExpressions cs (SimpleFloat floatFn) (sym, ((Itr i):rest)) =
  consumeExpressions cs (floatFn (fromIntegral i)) (sym, rest)
consumeExpressions _ (SimpleFloat _) (sym, (_:_)) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a float where another type was given"
consumeExpressions _ (SimpleFloat _) (sym, []) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a float where nothing was given (too few arguments)"

consumeExpressions cs (SimpleBool boolFn) (sym, ((Bol b):rest)) =
  consumeExpressions cs (boolFn b) (sym, rest)
consumeExpressions _ (SimpleBool _) (sym, (_:_)) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a boolean (True or False) where another type was given"
consumeExpressions _ (SimpleBool _) (sym, []) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a boolean (True or False) where nothing was given (too few arguments)"

consumeExpressions constructors (Composer sFn) (sym, (Sym b):rest) = do
  argS <- buildSingleValue constructors (b, [])
  consumeExpressions constructors (sFn argS) (sym, rest)
consumeExpressions constructors (Composer sFn) (sym, (Paren ((Sym b):exprs)):rest) = do
  argS <- buildSingleValue constructors (b, exprs)
  consumeExpressions constructors (sFn argS) (sym, rest)
consumeExpressions _ (Composer _) (sym, (_:_)) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a Value where another type was given"
consumeExpressions _ (Composer _) (sym, []) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a Value where nothing was given (too few arguments)"
