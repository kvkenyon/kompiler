{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Func IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool

instance Show LispVal where
  show :: LispVal -> String
  show = T.unpack . showLispVal

showLispVal :: LispVal -> T.Text
showLispVal (Atom t) = t
showLispVal (List contents) = T.concat ["(", T.unwords $ showLispVal <$> contents, ")"]
showLispVal (Number x) = T.pack $ show x
showLispVal (String s) = T.concat ["/", s, "/"]
showLispVal Nil = "Nil"
showLispVal (Bool True) = "#t"
showLispVal (Bool False) = "#f"
showLispVal (Func _) = "(internal fn)"
showLispVal (Lambda _ _) = "(lambda)"

newtype IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

type EnvCtx = M.Map T.Text LispVal

newtype Eval a where
  Eval :: {eval :: ReaderT EnvCtx IO a} -> Eval a
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )
