{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen where

import LLVM.Pretty
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.AST
import Control.Monad.State

import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as AST
import qualified Data.Map as Map
import LLVM.Prelude (ShortByteString)
import Data.Sequence.Internal (State)


float :: Type
float = FloatingPointType DoubleFP

type SymbolTable = [(ShortByteString, Operand)]
type Names = Map.Map ShortByteString Int

data CodegenState
  = CodegenState {
      currentBlock :: Name
    , blocks       :: Map.Map Name BlockState
    , symbolTable  :: SymbolTable
    , blockCount   :: Int
    , count        :: Word
    , names        :: Names
  }

data BlockState
  = BlockState {
    idx   :: Int
  , stack :: [Named Instruction]
  , term  :: Maybe (Named Terminator)
  } deriving Show


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)