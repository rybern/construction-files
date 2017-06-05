{-# LANGUAGE OverloadedStrings #-}
module ParseConstructor where

import Data.Attoparsec.Text
import Data.Char
import Data.Text hiding (map, any)
import Data.Text as Text (unlines)
import Data.Text.IO as Text (readFile)
import Data.Scientific hiding (scientific)
import qualified Data.Set as Set

-- call it ConstructionParse or ConstructionLang or ConstructionFile
-- need additional cases for IO wrapped, lists of various types
-- everything can be done in this library, to the point where you can have a defaultMain function that takes the list of constructors and an action to do with the final value

type Symbol = Text

data Expression = Sym Symbol
                | Paren [Expression]
                | Str Text
                | Flt Double
                | Itr Integer
                | Bol Bool
                | ListOfItr [Integer]
                | ListOfFlt [Double]
                | ListOfExpr [Expression]
                deriving (Show)

newtype ParseError = ParseError Text deriving Show

parseConstructorFile :: FilePath -> IO (Either ParseError ( [(Symbol, (Symbol, [Expression]))]
                                                          , (Symbol, [Expression])))
parseConstructorFile = (parseConstructors <$>) . Text.readFile

parseConstructors :: Text -> Either ParseError ( [(Symbol, (Symbol, [Expression]))]
                                               , (Symbol, [Expression]))
parseConstructors = packErrors . eitherResult . finish . parse parseFile

parseFile :: Parser ( [(Symbol, (Symbol, [Expression]))]
                    , (Symbol, [Expression]) )
parseFile = do
  skipLines
  assignments <- many' (withLines1 parseAssignment)
  finalConstructor <- parseConstructor
  skipLines
  endOfInput
  return (assignments, finalConstructor)

finish :: IResult Text r -> IResult Text r
finish = flip feed (pack "")

parseAssignment :: Parser (Symbol, (Symbol, [Expression]))
parseAssignment = do
  var <- withSpaces parseSymbol
  _ <- char '='
  skipSpace'
  constructor <- parseConstructor
  return (var, constructor)

parseConstructor :: Parser (Symbol, [Expression])
parseConstructor = do
  identifier <- withSpaces parseSymbol
  rest <- parseExpressions
  return (identifier, rest)

parseSymbol :: Parser Symbol
parseSymbol = takeWhile1 isAlpha

parseExpressions :: Parser [Expression]
parseExpressions = many' (withSpaces parseExpression)

parseExpressions1 :: Parser [Expression]
parseExpressions1 = many1 (withSpaces parseExpression)

parseExpression :: Parser Expression
parseExpression = choice [
  parseExprNumList
  , (ListOfExpr <$> parseList parseExpression)
  , (Paren <$> parseParenthetical)
  , parseExprNumber
  , (Bol <$> parseBool)
  , (Str <$> parseQuoted)
  , (Sym <$> parseSymbol)
  ]

parseExprNumList :: Parser Expression
parseExprNumList = do
  nums <- parseList scientific
  return $ if any isFloating nums
           then ListOfFlt $ map toRealFloat nums
           else ListOfItr . flip map nums $ \n -> case floatingOrInteger n of
                                                    Left r -> round r
                                                    Right i -> i

parseList :: Parser a -> Parser [a]
parseList p = do
  _ <- char '['
  skipSpace'
  ps <- sepBy p $ do
    skipSpace'
    _ <- char ','
    skipSpace'

  skipSpace'
  _ <- char ']'
  return ps


parseParenthetical :: Parser [Expression]
parseParenthetical = do
  _ <- char '('
  skipSpace'
  exprs <- parseExpressions1
  _ <- char ')'
  return exprs

parseExprNumber :: Parser Expression
parseExprNumber = do
  num <- scientific
  return $ case floatingOrInteger num of
    (Left d) -> Flt d
    (Right i) -> Itr i

parseBool :: Parser Bool
parseBool = choice [
    string "True" >> return True
  , string "False" >> return False
  ]

parseQuoted :: Parser Text
parseQuoted = do
  _ <- char '"'
  str <- takeWhile1 (/= '"')
  _ <- char '"'
  return str

-- like isSpace but doesn't match newlines
isSpace' :: Char -> Bool
isSpace' c = any (c ==) [' ', '\t']

skipSpace' :: Parser ()
skipSpace' = skipWhile isSpace'

withSpaces :: Parser a -> Parser a
withSpaces = (<* skipSpace')

skipLines :: Parser ()
skipLines = skipWhile isEndOfLine

withLines :: Parser a -> Parser a
withLines = (<* skipLines)

withLines1 :: Parser a -> Parser a
withLines1 = withLines . (<* endOfLine)

packErrors :: Either String a -> Either ParseError a
packErrors (Right r) = Right r
packErrors (Left s) = Left . ParseError $ pack s


testFile = Text.unlines [ "a = b"
                        , "b = c 1"
                        , "b"
                        ]

p = finish $ parse parseFile testFile
