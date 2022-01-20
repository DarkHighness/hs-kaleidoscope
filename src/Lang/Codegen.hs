{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lang.Codegen where

import Control.Lens (makeLenses, use, (.=), (.~), (^.))
import Control.Monad.State (MonadState, State, execState, gets, modify)
import Data.ByteString.Short (ShortByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Transform (toShortByteString)
import Data.Text (Text)
import LLVM.AST (BasicBlock, Definition (GlobalDefinition), FloatingPointType (DoubleFP), Instruction, Module (moduleDefinitions, moduleName), Name (Name, UnName), Named ((:=)), Operand (ConstantOperand, LocalReference), Parameter (Parameter), Terminator, Type (FloatingPointType), defaultModule, functionDefaults)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Attribute as Attr
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global (Global (basicBlocks, linkage, name, parameters, returnType))
import qualified LLVM.AST.Linkage as L

double :: Type
double = FloatingPointType DoubleFP

void :: Type
void = AST.VoidType

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

fresh :: Codegen Word
fresh = do
  i <- use count
  count .= i + 1
  return (i + 1)

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

  blocks .= M.insert (Name qname) newBlk blks
  blockCount .= ix + 1
  names .= supply

  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock blkName = do
  currentBlock .= blkName
  return blkName

getBlock :: Codegen Name
getBlock = use currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock newSt = do
  active <- use currentBlock
  blks <- use blocks
  blocks .= M.insert active newSt blks

current :: Codegen BlockState
current = do
  cblk <- use currentBlock
  blks <- use blocks
  case M.lookup cblk blks of
    Nothing -> error $ "No such block: " ++ show cblk
    Just x -> return x

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

local :: Name -> Operand
local = LocalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

assign :: Text -> Operand -> Codegen ()
assign var x = do
  lcls <- use symtab
  symtab .= (var, x) : lcls

getvar :: Text -> Codegen Operand
getvar var = do
  syms <- use symtab
  case lookup var syms of
    Nothing -> error $ "Local variable not in scope: " ++ show var
    Just op -> return op

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = blk ^. stack
  let blk' = stack .~ (ref := ins) : i $ blk
  modifyBlock blk'
  return (local ref)

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  let blk' = term .~ return trm $ blk
  modifyBlock blk'
  return trm

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ AST.FAdd AST.noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ AST.FSub AST.noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ AST.FMul AST.noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ AST.FDiv AST.noFastMathFlags a b []

toArgs :: [Operand] -> [(Operand, [Attr.ParameterAttribute])]
toArgs = map (,[])

br :: Name -> Codegen (Named Terminator)
br val = terminator $ AST.Do $ AST.Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ AST.Do $ AST.CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ AST.Do $ AST.Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ AST.Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ AST.Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ AST.Load False ptr Nothing 0 []