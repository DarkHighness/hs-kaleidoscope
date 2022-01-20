{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lang.Codegen where

import Control.Lens (makeLenses, use)
import Control.Monad.State (MonadState, State, execState, gets, modify)
import Data.ByteString.Short (ShortByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Transform (toShortByteString)
import Data.Text (Text)
import LLVM.AST (BasicBlock, Definition (GlobalDefinition), FloatingPointType (DoubleFP), Instruction, Module (moduleDefinitions, moduleName), Name (Name), Named, Operand, Parameter (Parameter), Terminator, Type (FloatingPointType), defaultModule, functionDefaults)
import qualified LLVM.AST as AST
import LLVM.AST.Global (Global (basicBlocks, linkage, name, parameters, returnType))
import qualified LLVM.AST.Linkage as L

double :: Type
double = FloatingPointType DoubleFP

type Names = Map ShortByteString Int

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns = case M.lookup nm ns of
  Nothing -> (nm, M.insert nm 1 ns)
  Just ix -> (nm <> (toShortByteString . show) ix, M.insert nm (ix + 1) ns)

type SymbolTable = [(Text, Operand)]

data BlockState = BlockState
  { _idx :: Int,
    _stack :: [Named Instruction],
    _term :: Maybe (Named Terminator)
  }
  deriving (Show)

$(makeLenses ''BlockState)

data CodegenState = CodegenState
  { _currentBlock :: Name,
    _blocks :: Map Name BlockState,
    _symtab :: SymbolTable,
    _blockCount :: Int,
    _count :: Word,
    _names :: Names
  }
  deriving (Show)

$(makeLenses ''CodegenState)

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: Text -> AST.Module
emptyModule label = defaultModule {moduleName = toShortByteString label}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s {moduleDefinitions = defs ++ [d]}

define :: Type -> Text -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retType label argsTypes body =
  addDefn $
    GlobalDefinition $
      functionDefaults
        { name = (Name . toShortByteString) label,
          parameters = ([Parameter ty nm [] | (ty, nm) <- argsTypes], False),
          returnType = retType,
          basicBlocks = body
        }

external :: Type -> Text -> [(Type, Name)] -> LLVM ()
external retType label argsTypes =
  addDefn $
    GlobalDefinition $
      functionDefaults
        { name = (Name . toShortByteString) label,
          linkage = L.External,
          parameters = ([Parameter ty nm [] | (ty, nm) <- argsTypes], False),
          returnType = retType,
          basicBlocks = []
        }

entry :: Codegen Name
entry = use currentBlock

addBlock :: ShortByteString -> Codegen Name
addBlock blkName = do
  blks <- use blocks
  ix <- use blockCount
  nms <- use names

  let newBlk = emptyBlock ix
      (qname, supply) = uniqueName blkName nms

  modify $ \s ->
    s
      { _blocks = M.insert (Name qname) newBlk blks,
        _blockCount = ix + 1,
        _names = supply
      }

  return (Name qname)

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing