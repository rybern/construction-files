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
import Data.Text as Text (unlines)
import Data.Text.IO as Text
import qualified Data.Map as Map
import ParseConstructor
import Control.Monad

-- everything in this project badly needs renaming
                -- | ListOfItr [Integer]
                -- | ListOfFlt [Double]
                -- | ListOfExpr [Expression]

-- should switch to making sure that each expression starts with a symbol in the evaluation stage instead of the parsing stage. this would be a useful function when evaluating nested expressions.

data Constructor s = C0 s
                   | SimpleString (Text -> Constructor s)
                   | SimpleInt (Integer -> Constructor s)
                   | SimpleFloat (Double -> Constructor s)
                   | SimpleBool (Bool -> Constructor s)
                   | SimpleListOfInt ([Integer] -> Constructor s)
                   | SimpleListOfFloat ([Double] -> Constructor s)
                   | SimpleListOfValue ([s] -> Constructor s)
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

consumeExpressions cs (SimpleListOfInt liFn) (sym, ((ListOfItr is):rest)) =
  consumeExpressions cs (liFn is) (sym, rest)
consumeExpressions _ (SimpleListOfInt _) (sym, (_:_)) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a list of integers where another type was given"
consumeExpressions _ (SimpleListOfInt _) (sym, []) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a list of integers where nothing was given (too few arguments)"

consumeExpressions cs (SimpleListOfFloat lxFn) (sym, ((ListOfFlt xs):rest)) =
  consumeExpressions cs (lxFn xs) (sym, rest)
consumeExpressions cs (SimpleListOfFloat lxFn) (sym, ((ListOfItr is):rest)) =
  consumeExpressions cs (lxFn (Prelude.map fromIntegral is)) (sym, rest)
consumeExpressions _ (SimpleListOfFloat _) (sym, (_:_)) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a list of floats where another type was given"
consumeExpressions _ (SimpleListOfFloat _) (sym, []) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a list of floats where nothing was given (too few arguments)"

consumeExpressions constructors (SimpleListOfValue lexprFn) (sym, ((ListOfExpr exprs):rest)) = do
  values <- forM exprs $ \expr -> case expr of
    Sym b -> buildSingleValue constructors (b, [])
    Paren (Sym b:args) -> buildSingleValue constructors (b, args)
    otherwise -> Left . EvalError $
      "Constructor "
      `append` sym `append`
      " requires a list of Values (variables or constructors), but another type was given in the list"
  consumeExpressions constructors (lexprFn values) (sym, rest)
consumeExpressions _ (SimpleListOfValue _) (sym, (_:_)) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a list of Values where another type was given"
consumeExpressions _ (SimpleListOfValue _) (sym, []) = Left . EvalError $
  "Constructor " `append` sym `append` " requires a list of Values where nothing was given (too few arguments)"

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

{-
some emergency test code

cs = [ ("id" :: Symbol, SimpleListOfFloat C0)
     , ("join", SimpleListOfValue (C0 . Prelude.concat))
     , ("singleton", SimpleFloat (\x -> C0 [x]))]

test = Text.unlines
  [
    "a = id [1, 2, 3, 54, 5, 6.1234, 6]"
  , "b = id [123, 3, 54, 5, 6, 3, 4, 2]"
  , "join [(singleton 45.56), (join [a, b, (id [123, 4,2134, 5])])]"
  ]
-}
