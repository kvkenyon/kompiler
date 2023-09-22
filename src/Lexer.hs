{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Syntax
import Text.Parsec
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok

-- newtype Parser LispVal = Parser (Text -> [(LispVal,Text)])

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text u Identity
style =
  Lang.emptyDef
    { Tok.commentStart = "{-",
      Tok.commentEnd = "-}",
      Tok.commentLine = ";",
      Tok.opStart = Tok.opLetter style,
      Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~",
      Tok.identStart = letter <|> oneOf "-+/*=|&><",
      Tok.identLetter = digit <|> letter <|> oneOf "?+=|&-/",
      Tok.reservedOpNames = ["'", "\"", "Nil", "#f", "#t"]
    }

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = do
  p <- Tok.identifier lexer
  return (Atom $ T.pack p)

parseNumber :: Parser LispVal
parseNumber = do
  s <-
    (char '-' >> return negate)
      <|> (char '+' >> return id)
      <|> return id
  p <- many1 digit
  return (Number $ s $ read p)

parseString :: Parser LispVal
parseString = do
  reservedOp "\""
  p <- Text.Parsec.many1 $ noneOf "\""
  reservedOp "\""
  return (String $ T.pack p)

parseReserved :: Parser LispVal
parseReserved =
  (reservedOp "Nil" >> return Nil)
    <|> (reservedOp "#t" >> return (Bool True))
    <|> (reservedOp "#f" >> return (Bool False))

parseQuote :: Parser LispVal
parseQuote = do
  reservedOp "\'"
  x <- parseExpr
  return $ List [Atom "quote", x]

parseNil :: Parser LispVal
parseNil = do
  reservedOp "'"
  Tok.parens lexer $ Tok.whiteSpace lexer
  return Nil

parseList :: Parser LispVal
parseList =
  List . concat
    <$> Text.Parsec.many parseExpr
      `sepBy` (char ' ' <|> char '\n')

parseSExp :: Parser LispVal
parseSExp =
  List . concat
    <$> Tok.parens
      lexer
      (Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseExpr :: Parser LispVal
parseExpr =
  try parseNumber
    <|> parseString
    <|> parseAtom
    <|> parseReserved
    <|> try parseNil
    <|> parseQuote
    <|> parseSExp

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"
