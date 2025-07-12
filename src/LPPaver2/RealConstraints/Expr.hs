{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LPPaver2.RealConstraints.Expr
  ( Var,
    Expr (..),
    ExprStruct (..),
    UnaryOp (..),
    BinaryOp (..),
    exprVar,
    CanGetVarDomain (..),
    exprLit,
    CanGetLiteral (..),
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Set as Set
import GHC.Generics (Generic)
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P

{- Non-linear Expressions -}

type Var = String

data Expr b r = Expr
  { eval :: b -> r,
    vars :: Set.Set Var,
    sampleR :: r,
    structure :: ExprStruct (Expr b r),
    opBindingLevel :: Int
    {-
      The binding level of the expression's root operators.
      Operands of operators need to have lower level than the operator.
      Adding brackets to achieve this.

      Operator levels:
        var, (...), f(...): 0
        *: 1
        /: 1
        unary/binary -,+: 2
        <=: 3
        /\,\/: 4
        ==>: 5
    -}
  }

data ExprStruct expr
  = ExprVar {var :: Var}
  | ExprLit {lit :: Rational}
  | ExprUnary {unop :: UnaryOp, e1 :: expr}
  | ExprBinary {binop :: BinaryOp, e1 :: expr, e2 :: expr}
  deriving (P.Eq, Generic)

data UnaryOp
  = OpNeg
  | OpSqrt
  | OpSin
  | OpCos
  deriving (P.Eq, Generic)

instance (Show UnaryOp) where
  show OpNeg = "-"
  show OpSqrt = "sqrt"
  show OpSin = "sin"
  show OpCos = "cos"

data BinaryOp
  = OpPlus
  | OpMinus
  | OpTimes
  | OpDivide
  deriving (P.Eq, Generic)

instance (Show BinaryOp) where
  show OpPlus = "+"
  show OpMinus = "-"
  show OpTimes = "*"
  show OpDivide = "/"

instance Show (ExprStruct (Expr b r)) where
  show (ExprVar var) = var
  show (ExprLit c) = show (double c)
  show (ExprUnary OpNeg e) = printf "-%s" (wrapDescription e 2)
  show (ExprUnary OpSqrt e) = printf "sqrt(%s)" (show e)
  show (ExprUnary OpSin e) = printf "sin(%s)" (show e)
  show (ExprUnary OpCos e) = printf "cos(%s)" (show e)
  show (ExprBinary OpPlus e1 e2) = printf "%s + %s" (wrapDescription e1 2) (wrapDescription e2 2)
  show (ExprBinary OpMinus e1 e2) = printf "%s - %s" (wrapDescription e1 2) (wrapDescription e2 1)
  show (ExprBinary OpTimes e1 e2) = printf "%s⋅%s" (wrapDescription e1 1) (wrapDescription e2 1)
  show (ExprBinary OpDivide e1 e2) = printf "%s/%s" (wrapDescription e1 1) (wrapDescription e2 1)

wrapDescription :: Expr b r -> Int -> String
wrapDescription (Expr {..}) level
  | opBindingLevel < level = show structure
  | otherwise = "(" <> show structure <> ")"

instance Show (Expr b r) where
  show expr = show expr.structure

instance P.Eq (Expr b r) where
  expr1 == expr2 = expr1.structure P.== expr2.structure

instance Hashable (Expr b r) where
  hashWithSalt salt expr = hashWithSalt salt (show expr)

class CanGetVarDomain b r where
  getVarDomain :: r -> b -> Var -> r

exprVar :: (CanGetVarDomain b r) => r -> Var -> Expr b r
exprVar sampleR var =
  Expr
    { eval = \b -> getVarDomain sampleR b var,
      vars = Set.singleton var,
      sampleR,
      structure = ExprVar var,
      opBindingLevel = 0
    }

class CanGetLiteral b r where
  getLiteral :: r -> b -> Rational -> r

exprLit :: (CanGetLiteral b r) => r -> Rational -> Expr b r
exprLit sampleR literal =
  Expr
    { eval,
      vars = Set.empty,
      sampleR,
      structure = ExprLit literal,
      opBindingLevel = if literal < (0 :: Integer) then 2 else 0
    }
  where
    eval scope = getLiteral sampleR scope literal

exprNeg :: (CanNegSameType r) => Expr b r -> Expr b r
exprNeg e =
  e
    { eval = negate . e.eval,
      structure = ExprUnary OpNeg e,
      opBindingLevel = 2
    }

exprSqrt :: (CanSqrtSameType r) => Expr b r -> Expr b r
exprSqrt e =
  e
    { eval = sqrt . e.eval,
      structure = ExprUnary OpSqrt e,
      opBindingLevel = 0
    }

exprSin :: (CanSinCosSameType r) => Expr b r -> Expr b r
exprSin e =
  e
    { eval = sin . e.eval,
      structure = ExprUnary OpSin e,
      opBindingLevel = 0
    }

exprCos :: (CanSinCosSameType r) => Expr b r -> Expr b r
exprCos e =
  e
    { eval = cos . e.eval,
      structure = ExprUnary OpCos e,
      opBindingLevel = 0
    }

exprPlus :: (CanAddSameType r) => Expr b r -> Expr b r -> Expr b r
exprPlus e1 e2 =
  Expr
    { eval = \b -> e1.eval b + e2.eval b,
      vars = e1.vars `Set.union` e2.vars,
      sampleR = e1.sampleR,
      structure = ExprBinary OpPlus e1 e2,
      opBindingLevel = 2
    }

exprTimes :: (CanMulSameType r) => Expr b r -> Expr b r -> Expr b r
exprTimes e1 e2 =
  Expr
    { eval = \b -> e1.eval b * e2.eval b,
      vars = e1.vars `Set.union` e2.vars,
      sampleR = e1.sampleR,
      structure = ExprBinary OpTimes e1 e2,
      opBindingLevel = 1
    }

-- Instances to conveniently build expressions using the usual numerical operators

instance (CanNegSameType r) => CanNeg (Expr b r) where
  type NegType (Expr b r) = Expr b r
  negate = exprNeg

instance (CanSqrtSameType r) => CanSqrt (Expr b r) where
  type SqrtType (Expr b r) = Expr b r
  sqrt = exprSqrt

instance (CanSinCosSameType r) => CanSinCos (Expr b r) where
  type SinCosType (Expr b r) = Expr b r
  sin = exprSin
  cos = exprCos

instance (CanAddSameType r) => CanAddAsymmetric (Expr b r) (Expr b r) where
  type AddType (Expr b r) (Expr b r) = (Expr b r)
  add = exprPlus

instance (CanAddSameType r, CanGetLiteral b r) => CanAddAsymmetric (Expr b r) Rational where
  type AddType (Expr b r) Rational = (Expr b r)
  add e q = exprPlus e (exprLit e.sampleR q)

instance (CanAddSameType r, CanGetLiteral b r) => CanAddAsymmetric Rational (Expr b r) where
  type AddType Rational (Expr b r) = (Expr b r)
  add q e = exprPlus (exprLit e.sampleR q) e

instance (CanNegSameType r, CanAddSameType r) => CanSub (Expr b r) (Expr b r)

instance (CanAddSameType r, CanGetLiteral b r) => CanSub (Expr b r) Rational

instance (CanNegSameType r, CanAddSameType r, CanGetLiteral b r) => CanSub Rational (Expr b r)

instance (CanMulSameType r) => CanMulAsymmetric (Expr b r) (Expr b r) where
  type MulType (Expr b r) (Expr b r) = (Expr b r)
  mul = exprTimes

instance (CanMulSameType r, CanGetLiteral b r) => CanMulAsymmetric (Expr b r) Rational where
  type MulType (Expr b r) Rational = (Expr b r)
  mul e q = exprTimes e (exprLit e.sampleR q)

instance (CanMulSameType r, CanGetLiteral b r) => CanMulAsymmetric Rational (Expr b r) where
  type MulType Rational (Expr b r) = (Expr b r)
  mul q e = exprTimes (exprLit e.sampleR q) e
