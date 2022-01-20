{-# LANGUAGE OverloadedStrings #-}

module Lang.Emit where

import Lang.Codegen
import Control.Applicative
import Control.Monad.Except
import Data.Int
import qualified Data.Map as Map
import Data.Word
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.Context
import LLVM.Module
import qualified Lang.Syntax as S
import Data.String.Transform (toShortByteString)
import Data.Text (Text)
import qualified Data.Text as T

toSig :: [Text] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name . toShortByteString $ x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \a -> do
          var <- alloca double
          store var (local (AST.Name a))
          assign a var
        cgen body >>= ret
codegenTop (S.Extern name args) = do
  external double name fnargs
  where
    fnargs = toSig args
codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops =
  Map.fromList
    [ ("+", fadd),
      ("-", fsub),
      ("*", fmul),
      ("/", fdiv),
      ("<", lt)
    ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinOp S.Equals (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = getvar x >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT Text IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $
    withModuleFromAST context newast $ \m -> do
      llstr <- moduleLLVMAssembly m
      putStrLn llstr
      return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn