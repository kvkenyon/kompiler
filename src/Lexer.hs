{-# LANGUAGE OverloadedStrings #-}

module Lexer (lexer) where

import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Text.Parsec (digit, letter, oneOf, (<|>))
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token as Tok

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
