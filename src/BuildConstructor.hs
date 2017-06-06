{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BuildConstructor
  (
    Constructor (..)
  , TinyLangConstructor (..)
  , TinyLangSpec (..)
  , EvalError (..)
  , ParseError (..)
  , buildValue
  , buildValueFromFile
  , buildValueM
  , buildValueFromFileM
  , printHelpBuilderHelp
  , Symbol
  , eitherT
  , Constructible (..)
  )
where

import Data.Text hiding (map)
import Data.Semigroup ((<>))
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

showConstructor :: Text -> Constructor m s -> Text
showConstructor _ (C0 _) = ""
showConstructor valName (SimpleString f) = " STRING" `append` (showConstructor valName (f undefined))
showConstructor valName (SimpleInt f) = " INT" `append` (showConstructor valName (f undefined))
showConstructor valName (SimpleFloat f) = " FLOAT" `append` (showConstructor valName (f undefined))
showConstructor valName (SimpleBool f) = " BOOL" `append` (showConstructor valName (f undefined))
showConstructor valName (SimpleListOfInt f) = " [LIST OF INT]" `append` (showConstructor valName (f undefined))
showConstructor valName (SimpleListOfFloat f) = " [LIST OF FLOAT]" `append` (showConstructor valName (f undefined))
showConstructor valName (SimpleListOfValue f) = " [LIST OF " `append` valName `append` "]" `append` (showConstructor valName (f undefined))
showConstructor valName (Composer f) = " " `append` valName `append` (showConstructor valName (f undefined))

data TinyLangConstructor m a = TinyLangConstructor {
    sym :: Symbol
  , constructor :: Constructor m a
  , constructorDescription :: Text
  }

data TinyLangSpec m a = TinyLangSpec {
    constructors :: [TinyLangConstructor m a]
  , valName :: Text
  }

printHelpBuilderHelp :: TinyLangSpec m a
                     -> IO ()
printHelpBuilderHelp (TinyLangSpec {..}) = do
  Text.putStrLn "Constructors available:"
  Text.putStrLn ""
  forM_ constructors $ \(TinyLangConstructor {..}) -> do
    Text.putStrLn $
      "  " <> sym <> showConstructor valName constructor
    Text.putStrLn $
      "    " <> constructorDescription

newtype EvalError = EvalError Text deriving Show

buildValue :: TinyLangSpec Identity s
           -> Text
           -> Either ParseError (EitherT EvalError Identity s)
buildValue = buildValueM

buildValueFromFile :: TinyLangSpec Identity s
                    -> FilePath
                    -> IO (Either ParseError (EitherT EvalError Identity s))
buildValueFromFile = buildValueFromFileM

buildValueM :: (Monad m)
            => TinyLangSpec m s
            -> Text
            -> Either ParseError (EitherT EvalError m s)
buildValueM spec =
  (buildValueFromParsed cMap <$>) . parseConstructors
  where cMap = Map.fromList . map (\c -> (sym c, constructor c)) . constructors $ spec

buildValueFromFileM :: (Monad m)
                   => TinyLangSpec m s
                   -> FilePath
                   -> IO (Either ParseError (EitherT EvalError m s))
buildValueFromFileM spec =
  (buildValueM spec <$>) . Text.readFile

buildValueFromParsed :: (Monad m)
                     => Map.Map Symbol (Constructor m s)
                     -> ( [(Symbol, (Symbol, [Expression]))]
                        , (Symbol, [Expression]))
                     -> EitherT EvalError m s
buildValueFromParsed constructors ([], final) = buildSingleValue constructors final
buildValueFromParsed constructors ((var, constructor):restAssignments, final) = do
  when (var `Map.member` constructors) . left . EvalError $
    "The variable name " <> var <> "is already in use"
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
                                                 "Unknown constructor or variable: " <> sym

consumeExpressions :: (Monad m)
                   => Map.Map Symbol (Constructor m s)
                   -> Constructor m s
                   -> (Symbol, [Expression])
                   -> EitherT EvalError m s
consumeExpressions _ (C0 s) (_, []) = right s
consumeExpressions _ (C0 s) (sym, _) = left . EvalError $
  "Too many arguments to constructor " <> sym

consumeExpressions cs (SimpleString strFn) (sym, ((Str str):rest)) =
  consumeExpressions cs (strFn str) (sym, rest)
consumeExpressions _ (SimpleString _) (sym, (_:_)) = left . EvalError $
  "Constructor " <> sym <> " requires a string where another type was given"
consumeExpressions _ (SimpleString _) (sym, []) = left . EvalError $
  "Constructor " <> sym <> " requires a string where nothing was given (too few arguments)"

consumeExpressions cs (SimpleInt intFn) (sym, ((Itr i):rest)) =
  consumeExpressions cs (intFn i) (sym, rest)
consumeExpressions _ (SimpleInt _) (sym, (_:_)) = left . EvalError $
  "Constructor " <> sym <> " requires an integer where another type was given"
consumeExpressions _ (SimpleInt _) (sym, []) = left . EvalError $
  "Constructor " <> sym <> " requires an integer where nothing was given (too few arguments)"

consumeExpressions cs (SimpleFloat floatFn) (sym, ((Flt x):rest)) =
  consumeExpressions cs (floatFn x) (sym, rest)
consumeExpressions cs (SimpleFloat floatFn) (sym, ((Itr i):rest)) =
  consumeExpressions cs (floatFn (fromIntegral i)) (sym, rest)
consumeExpressions _ (SimpleFloat _) (sym, (_:_)) = left . EvalError $
  "Constructor " <> sym <> " requires a float where another type was given"
consumeExpressions _ (SimpleFloat _) (sym, []) = left . EvalError $
  "Constructor " <> sym <> " requires a float where nothing was given (too few arguments)"

consumeExpressions cs (SimpleBool boolFn) (sym, ((Bol b):rest)) =
  consumeExpressions cs (boolFn b) (sym, rest)
consumeExpressions _ (SimpleBool _) (sym, (_:_)) = left . EvalError $
  "Constructor " <> sym <> " requires a boolean (True or False) where another type was given"
consumeExpressions _ (SimpleBool _) (sym, []) = left . EvalError $
  "Constructor " <> sym <> " requires a boolean (True or False) where nothing was given (too few arguments)"

consumeExpressions cs (SimpleListOfInt liFn) (sym, ((ListOfItr is):rest)) =
  consumeExpressions cs (liFn is) (sym, rest)
consumeExpressions _ (SimpleListOfInt _) (sym, (_:_)) = left . EvalError $
  "Constructor " <> sym <> " requires a list of integers where another type was given"
consumeExpressions _ (SimpleListOfInt _) (sym, []) = left . EvalError $
  "Constructor " <> sym <> " requires a list of integers where nothing was given (too few arguments)"

consumeExpressions cs (SimpleListOfFloat lxFn) (sym, ((ListOfFlt xs):rest)) =
  consumeExpressions cs (lxFn xs) (sym, rest)
consumeExpressions cs (SimpleListOfFloat lxFn) (sym, ((ListOfItr is):rest)) =
  consumeExpressions cs (lxFn (map fromIntegral is)) (sym, rest)
consumeExpressions _ (SimpleListOfFloat _) (sym, (_:_)) = left . EvalError $
  "Constructor " <> sym <> " requires a list of floats where another type was given"
consumeExpressions _ (SimpleListOfFloat _) (sym, []) = left . EvalError $
  "Constructor " <> sym <> " requires a list of floats where nothing was given (too few arguments)"

consumeExpressions constructors (SimpleListOfValue lexprFn) (sym, ((ListOfExpr exprs):rest)) = do
  values <- forM exprs $ \expr -> case expr of
    Sym b -> buildSingleValue constructors (b, [])
    Paren (Sym b:args) -> buildSingleValue constructors (b, args)
    otherwise -> left . EvalError $
      "Constructor "
      `append` sym <>
      " requires a list of Values (variables or constructors), but another type was given in the list"
  consumeExpressions constructors (lexprFn values) (sym, rest)
consumeExpressions _ (SimpleListOfValue _) (sym, (_:_)) = left . EvalError $
  "Constructor " <> sym <> " requires a list of Values where another type was given"
consumeExpressions _ (SimpleListOfValue _) (sym, []) = left . EvalError $
  "Constructor " <> sym <> " requires a list of Values where nothing was given (too few arguments)"

consumeExpressions constructors (Composer sFn) (sym, (Sym b):rest) = do
  argS <- buildSingleValue constructors (b, [])
  consumeExpressions constructors (sFn argS) (sym, rest)
consumeExpressions constructors (Composer sFn) (sym, (Paren ((Sym b):exprs)):rest) = do
  argS <- buildSingleValue constructors (b, exprs)
  consumeExpressions constructors (sFn argS) (sym, rest)
consumeExpressions _ (Composer _) (sym, (_:_)) = left . EvalError $
  "Constructor " <> sym <> " requires a Value where another type was given"
consumeExpressions _ (Composer _) (sym, []) = left . EvalError $
  "Constructor " <> sym <> " requires a Value where nothing was given (too few arguments)"

consumeExpressions constructors (Monadic op) exprs =
  lift op >>= \res -> consumeExpressions constructors res exprs


class Constructible m a b where
  buildConstructor :: a -> Constructor m b
instance Constructible m a a where
  buildConstructor = C0
instance (Constructible m a b) => Constructible m (Text -> a) b where
  buildConstructor f = SimpleString (\t -> buildConstructor $ f t)
instance (Constructible m a b) => Constructible m (String -> a) b where
  buildConstructor f = SimpleString (\t -> buildConstructor $ f (unpack t))
instance (Constructible m a b, Integral c) => Constructible m (c -> a) b where
  buildConstructor f = SimpleInt (\t -> buildConstructor $ f (fromIntegral t))
instance (Constructible m a b) => Constructible m (Double -> a) b where
  buildConstructor f = SimpleFloat (\t -> buildConstructor $ f t)
instance (Constructible m a b) => Constructible m (Bool -> a) b where
  buildConstructor f = SimpleBool (\t -> buildConstructor $ f t)
instance (Constructible m a b, Integral c) => Constructible m ([c] -> a) b where
  buildConstructor f = SimpleListOfInt (\t -> buildConstructor $ f (map fromIntegral t))
instance (Constructible m a b) => Constructible m ([Double] -> a) b where
  buildConstructor f = SimpleListOfFloat (\t -> buildConstructor $ f t)
instance (Constructible m a b) => Constructible m ([b] -> a) b where
  buildConstructor f = SimpleListOfValue (\t -> buildConstructor $ f t)
instance (Constructible m a b) => Constructible m (b -> a) b where
  buildConstructor f = Composer (\t -> buildConstructor $ f t)

{-
some emergency test code

 - }

cs = [ ("id" :: Symbol, SimpleListOfFloat C0)
     , ("join", SimpleListOfValue (C0 . concat))
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
