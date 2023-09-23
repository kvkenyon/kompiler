{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Control.Monad.State
import qualified Data.Map as Map
import LLVM.AST
  ( FloatingPointType (DoubleFP),
    Instruction,
    Name,
    Named,
    Operand,
    Terminator,
    Type (FloatingPointType),
  )

double2 :: Type
double2 = FloatingPointType DoubleFP

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState
  { currentBlock :: Name, -- Name of the active block to append to
    blocks :: Map.Map Name BlockState, -- Blocks for function
    symtab :: SymbolTable, -- Function scope symbol table
    blockCount :: Int, -- Count of basic blocks
    count :: Word -- Count of unnamed instructions
    -- names :: Names -- Name Supply
  }
  deriving (Show)

data BlockState = BlockState
  { idx :: Int, -- Block index
    stack :: [Named Instruction], -- Stack of instructions
    term :: Maybe (Named Terminator) -- Block terminator
  }
  deriving (Show)

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
  deriving (Functor, Applicative, Monad, MonadState CodegenState)
