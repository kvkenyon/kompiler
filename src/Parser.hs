{-# LANGUAGE OverloadedStrings #-}

module Parser (readExpr, readExprFile) where

import qualified Data.Text as T
import Lexer (lexer)
import Syntax
  ( LispVal
      ( Atom,
        Bool,
        List,
        Nil,
        Number,
        String
      ),
  )
import Text.Parsec
  ( ParseError,
    char,
    digit,
    eof,
    many,
    many1,
    noneOf,
    parse,
    sepBy,
    try,
    (<|>),
  )
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as Tok

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
