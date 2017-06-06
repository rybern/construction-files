{-# LANGUAGE OverloadedStrings #-}
module BuildConstructor
  (
    Constructor (..)
  , EvalError (..)
  , ParseError (..)
  , buildValue
  , buildValueFromFile
  , buildValueM
  , buildValueFromFileM
  , Symbol
  , eitherT
  )
where

import Data.Text
import Data.Text as Text (unlines)
import Data.Text.IO as Text
import qualified Data.Map as Map
import ParseConstructor
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either

-- everything in this project badly needs renaming
                -- | ListOfItr [Integer]
                -- | ListOfFlt [Double]
                -- | ListOfExpr [Expression]

-- should switch to making sure that each expression starts with a symbol in the evaluation stage instead of the parsing stage. this would be a useful function when evaluating nested expressions.

data Constructor m s = C0 s
                     | SimpleString (Text -> Constructor m s)
                     | SimpleInt (Integer -> Constructor m s)
                     | SimpleFloat (Double -> Constructor m s)
                     | SimpleBool (Bool -> Constructor m s)
                     | SimpleListOfInt ([Integer] -> Constructor m s)
                     | SimpleListOfFloat ([Double] -> Constructor m s)
                     | SimpleListOfValue ([s] -> Constructor m s)
                     | Composer (s -> Constructor m s)
                     | Monadic (m (Constructor m s))

newtype EvalError = EvalError Text deriving Show

buildValue :: [(Symbol, (Constructor Identity s))]
           -> Text
           -> Either ParseError (EitherT EvalError Identity s)
buildValue = buildValueM

buildValueFromFile :: [(Symbol, (Constructor Identity s))]
                    -> FilePath
                    -> IO (Either ParseError (EitherT EvalError Identity s))
buildValueFromFile = buildValueFromFileM

buildValueM :: (Monad m)
            => [(Symbol, (Constructor m s))]
            -> Text
            -> Either ParseError (EitherT EvalError m s)
buildValueM constructors =
  (buildValueFromParsed (Map.fromList constructors) <$>) . parseConstructors

buildValueFromFileM :: (Monad m)
                   => [(Symbol, (Constructor m s))]
                   -> FilePath
                   -> IO (Either ParseError (EitherT EvalError m s))
buildValueFromFileM constructors =
  ((buildValueFromParsed (Map.fromList constructors) <$>) <$>) . parseConstructorFile

buildValueFromParsed :: (Monad m)
                     => Map.Map Symbol (Constructor m s)
                     -> ( [(Symbol, (Symbol, [Expression]))]
                        , (Symbol, [Expression]))
                     -> EitherT EvalError m s
buildValueFromParsed constructors ([], final) = buildSingleValue constructors final
buildValueFromParsed constructors ((var, constructor):restAssignments, final) = do
  when (var `Map.member` constructors) . left . EvalError $
    "The variable name " `append` var `append` "is already in use"
  value <- buildSingleValue constructors constructor
  let constructors' = Map.insert var (C0 value) constructors
  buildValueFromParsed constructors' (restAssignments, final)

buildSingleValue :: (Monad m)
                 => Map.Map Symbol (Constructor m s)
                 -> (Symbol, [Expression])
                 -> EitherT EvalError m s
buildSingleValue constructors (sym, exprs) = case sym `Map.lookup` constructors of
                                               Just c -> consumeExpressions constructors c (sym, exprs)
                                               Nothing -> left . EvalError $
                                                 "Unknown constructor or variable: " `append` sym

consumeExpressions :: (Monad m)
                   => Map.Map Symbol (Constructor m s)
                   -> Constructor m s
                   -> (Symbol, [Expression])
                   -> EitherT EvalError m s
consumeExpressions _ (C0 s) (_, []) = right s
consumeExpressions _ (C0 s) (sym, _) = left . EvalError $
  "Too many arguments to constructor " `append` sym

consumeExpressions cs (SimpleString strFn) (sym, ((Str str):rest)) =
  consumeExpressions cs (strFn str) (sym, rest)
consumeExpressions _ (SimpleString _) (sym, (_:_)) = left . EvalError $
  "Constructor " `append` sym `append` " requires a string where another type was given"
consumeExpressions _ (SimpleString _) (sym, []) = left . EvalError $
  "Constructor " `append` sym `append` " requires a string where nothing was given (too few arguments)"

consumeExpressions cs (SimpleInt intFn) (sym, ((Itr i):rest)) =
  consumeExpressions cs (intFn i) (sym, rest)
consumeExpressions _ (SimpleInt _) (sym, (_:_)) = left . EvalError $
  "Constructor " `append` sym `append` " requires an integer where another type was given"
consumeExpressions _ (SimpleInt _) (sym, []) = left . EvalError $
  "Constructor " `append` sym `append` " requires an integer where nothing was given (too few arguments)"

consumeExpressions cs (SimpleFloat floatFn) (sym, ((Flt x):rest)) =
  consumeExpressions cs (floatFn x) (sym, rest)
consumeExpressions cs (SimpleFloat floatFn) (sym, ((Itr i):rest)) =
  consumeExpressions cs (floatFn (fromIntegral i)) (sym, rest)
consumeExpressions _ (SimpleFloat _) (sym, (_:_)) = left . EvalError $
  "Constructor " `append` sym `append` " requires a float where another type was given"
consumeExpressions _ (SimpleFloat _) (sym, []) = left . EvalError $
  "Constructor " `append` sym `append` " requires a float where nothing was given (too few arguments)"

consumeExpressions cs (SimpleBool boolFn) (sym, ((Bol b):rest)) =
  consumeExpressions cs (boolFn b) (sym, rest)
consumeExpressions _ (SimpleBool _) (sym, (_:_)) = left . EvalError $
  "Constructor " `append` sym `append` " requires a boolean (True or False) where another type was given"
consumeExpressions _ (SimpleBool _) (sym, []) = left . EvalError $
  "Constructor " `append` sym `append` " requires a boolean (True or False) where nothing was given (too few arguments)"

consumeExpressions cs (SimpleListOfInt liFn) (sym, ((ListOfItr is):rest)) =
  consumeExpressions cs (liFn is) (sym, rest)
consumeExpressions _ (SimpleListOfInt _) (sym, (_:_)) = left . EvalError $
  "Constructor " `append` sym `append` " requires a list of integers where another type was given"
consumeExpressions _ (SimpleListOfInt _) (sym, []) = left . EvalError $
  "Constructor " `append` sym `append` " requires a list of integers where nothing was given (too few arguments)"

consumeExpressions cs (SimpleListOfFloat lxFn) (sym, ((ListOfFlt xs):rest)) =
  consumeExpressions cs (lxFn xs) (sym, rest)
consumeExpressions cs (SimpleListOfFloat lxFn) (sym, ((ListOfItr is):rest)) =
  consumeExpressions cs (lxFn (Prelude.map fromIntegral is)) (sym, rest)
consumeExpressions _ (SimpleListOfFloat _) (sym, (_:_)) = left . EvalError $
  "Constructor " `append` sym `append` " requires a list of floats where another type was given"
consumeExpressions _ (SimpleListOfFloat _) (sym, []) = left . EvalError $
  "Constructor " `append` sym `append` " requires a list of floats where nothing was given (too few arguments)"

consumeExpressions constructors (SimpleListOfValue lexprFn) (sym, ((ListOfExpr exprs):rest)) = do
  values <- forM exprs $ \expr -> case expr of
    Sym b -> buildSingleValue constructors (b, [])
    Paren (Sym b:args) -> buildSingleValue constructors (b, args)
    otherwise -> left . EvalError $
      "Constructor "
      `append` sym `append`
      " requires a list of Values (variables or constructors), but another type was given in the list"
  consumeExpressions constructors (lexprFn values) (sym, rest)
consumeExpressions _ (SimpleListOfValue _) (sym, (_:_)) = left . EvalError $
  "Constructor " `append` sym `append` " requires a list of Values where another type was given"
consumeExpressions _ (SimpleListOfValue _) (sym, []) = left . EvalError $
  "Constructor " `append` sym `append` " requires a list of Values where nothing was given (too few arguments)"

consumeExpressions constructors (Composer sFn) (sym, (Sym b):rest) = do
  argS <- buildSingleValue constructors (b, [])
  consumeExpressions constructors (sFn argS) (sym, rest)
consumeExpressions constructors (Composer sFn) (sym, (Paren ((Sym b):exprs)):rest) = do
  argS <- buildSingleValue constructors (b, exprs)
  consumeExpressions constructors (sFn argS) (sym, rest)
consumeExpressions _ (Composer _) (sym, (_:_)) = left . EvalError $
  "Constructor " `append` sym `append` " requires a Value where another type was given"
consumeExpressions _ (Composer _) (sym, []) = left . EvalError $
  "Constructor " `append` sym `append` " requires a Value where nothing was given (too few arguments)"

consumeExpressions constructors (Monadic op) exprs =
  lift op >>= \res -> consumeExpressions constructors res exprs

{-
some emergency test code

 - }

cs = [ ("id" :: Symbol, SimpleListOfFloat C0)
     , ("join", SimpleListOfValue (C0 . Prelude.concat))
     , ("singleton", SimpleFloat (\x -> C0 [x]))
     , ("readin", Monadic (readLn >>= \x -> return $ C0 [x]))]

testLines = Text.unlines
  [
    "a = id [1, 2, 3, 54, 5, 6.1234, 6]"
  , "b = id [123, 3, 54, 5, 6, 3, 4, 2]"
  , "join [(singleton 45.56), (join [a, b, (id [123, 4,2134, 5]), (readin)])]"
  ]

test = buildValueM cs testLines
-}
