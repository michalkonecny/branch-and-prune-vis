{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LPPaver2.RealConstraints.Expr
  ( Var,
    ExprF (..),
    UnaryOp (..),
    BinaryOp (..),
    Expr (..),
    ExprHash,
    ExprNode,
    exprVar,
    exprLit,
  )
where

import Data.Hashable (Hashable (hash))
import Data.Map qualified as Map
import GHC.Generics (Generic)
import GHC.Records
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P

{- Non-linear Expressions -}

type Var = String

data ExprF e
  = ExprVar {var :: Var}
  | ExprLit {lit :: Rational}
  | ExprUnary {unop :: UnaryOp, e1 :: e}
  | ExprBinary {binop :: BinaryOp, e1 :: e, e2 :: e}
  deriving (P.Eq, Generic, Hashable)

data UnaryOp
  = OpNeg
  | OpSqrt
  | OpSin
  | OpCos
  deriving (P.Eq, Generic, Hashable)

data BinaryOp
  = OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  deriving (P.Eq, Generic, Hashable)

type ExprHash = Int

type ExprNode = ExprF ExprHash

-- |
--  Contains a dictionary indexed by hashes and a root hash that has to exist in the dictionary.
--
--  The dictionary's values are expression nodes.
--  Each node refers to its sub-expressions using their hashes, which also have to be in the dictionary.
--
--  Thus, the expression trees are encoded indirectly and identical sub-expressions are kept only once.
--
--  These hashes can be used to efficiently associate any kind of values with the expression nodes.
data Expr = Expr {nodes :: Map.Map ExprHash ExprNode, root :: ExprHash}

instance P.Eq Expr where
  e1 == e2 = e1.root == e2.root

instance Show Expr where
  show expr = (showExprHash expr.root).str
    where
      showExprHash exprHash =
        case Map.lookup exprHash expr.nodes of
          Nothing -> error "A hash is missing from expr.nodes"
          Just exprNode -> showExprF showExprHash exprNode

type Level = Integer

data StringAndLevel = StringAndLevel {str :: String, level :: Level}

stringAndLevel :: String -> Level -> StringAndLevel
stringAndLevel = StringAndLevel

showExprF :: (e -> StringAndLevel) -> ExprF e -> StringAndLevel
showExprF showExpr exprNode =
  case exprNode of
    (ExprVar var) -> stringAndLevel var 0
    (ExprLit c) -> stringAndLevel (show (double c)) 0
    (ExprUnary OpNeg e) -> stringAndLevel (printf "-%s" (showEL e 1)) 2
    (ExprUnary OpSqrt e) -> stringAndLevel (printf "sqrt(%s)" (showE e)) 0
    (ExprUnary OpSin e) -> stringAndLevel (printf "sin(%s)" (showE e)) 0
    (ExprUnary OpCos e) -> stringAndLevel (printf "cos(%s)" (showE e)) 0
    (ExprBinary OpPlus e1 e2) -> stringAndLevel (printf "%s + %s" (showEL e1 1) (showEL e2 1)) 2
    (ExprBinary OpMinus e1 e2) -> stringAndLevel (printf "%s - %s" (showEL e1 1) (showEL e2 0)) 2
    (ExprBinary OpTimes e1 e2) -> stringAndLevel (printf "%s⋅%s" (showEL e1 0) (showEL e2 0)) 1
    (ExprBinary OpDivide e1 e2) -> stringAndLevel (printf "%s/%s" (showEL e1 0) (showEL e2 0)) 1
  where
    showE e = (showExpr e).str
    showEL e (maxLevel :: Level)
      | eSL.level <= maxLevel = eSL.str
      | otherwise = "(" <> eSL.str <> ")"
      where
        eSL = showExpr e

expr0 :: ExprNode -> Expr
expr0 e =
  Expr {nodes = Map.singleton root e, root}
  where
    root = hash e

expr1 :: UnaryOp -> Expr -> Expr
expr1 unop e1 =
  Expr {nodes = Map.insert h e e1.nodes, root = h}
  where
    e = ExprUnary {unop, e1 = e1.root}
    h = hash e

expr2 :: BinaryOp -> Expr -> Expr -> Expr
expr2 binop e1 e2 =
  Expr {nodes = Map.insert h e $ Map.union e1.nodes e2.nodes, root = h}
  where
    e = ExprBinary {binop, e1 = e1.root, e2 = e2.root}
    h = hash e

exprVar :: Var -> Expr
exprVar var = expr0 $ ExprVar {var}

exprLit :: Rational -> Expr
exprLit lit = expr0 $ ExprLit {lit}

exprNeg :: Expr -> Expr
exprNeg = expr1 OpNeg

exprSqrt :: Expr -> Expr
exprSqrt = expr1 OpSqrt

exprSin :: Expr -> Expr
exprSin = expr1 OpSin

exprCos :: Expr -> Expr
exprCos = expr1 OpCos

exprPlus :: Expr -> Expr -> Expr
exprPlus = expr2 OpPlus

exprSub :: Expr -> Expr -> Expr
exprSub = expr2 OpMinus

exprTimes :: Expr -> Expr -> Expr
exprTimes = expr2 OpTimes

-- Instances to conveniently build expressions using the usual numerical operators

instance CanNeg Expr where
  type NegType Expr = Expr
  negate = exprNeg

instance CanSqrt Expr where
  type SqrtType Expr = Expr
  sqrt = exprSqrt

instance CanSinCos Expr where
  type SinCosType Expr = Expr
  sin = exprSin
  cos = exprCos

instance CanAddAsymmetric Expr Expr where
  type AddType Expr Expr = Expr
  add = exprPlus

instance CanAddAsymmetric Expr Rational where
  type AddType Expr Rational = Expr
  add e q = exprPlus e (exprLit q)

instance CanAddAsymmetric Rational Expr where
  type AddType Rational Expr = Expr
  add q = exprPlus (exprLit q)

instance CanSub Expr Expr where
  sub = exprSub

instance CanSub Expr Rational where
  type SubType Expr Rational = Expr
  sub e q = exprSub e (exprLit q)

instance CanSub Rational Expr where
  type SubType Rational Expr = Expr
  sub q = exprSub (exprLit q)

instance CanMulAsymmetric Expr Expr where
  type MulType Expr Expr = Expr
  mul = exprTimes

instance CanMulAsymmetric Expr Rational where
  type MulType Expr Rational = Expr
  mul e q = exprTimes e (exprLit q)

instance CanMulAsymmetric Rational Expr where
  type MulType Rational Expr = Expr
  mul q = exprTimes (exprLit q)
