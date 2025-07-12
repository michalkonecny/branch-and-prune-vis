{-# OPTIONS_GHC -Wno-partial-fields #-}
module LPPaver2.RealConstraints.Form
  ( Form (..),
    UnaryConn (..),
    BinaryConn (..),
    BinaryComp (..),
    formImpl,
    formIfThenElse,
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import LPPaver2.RealConstraints.Expr
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P

data BinaryComp = CompLeq | CompEq
  deriving (P.Eq, Generic)

instance Hashable BinaryComp

instance Show BinaryComp where
  show CompLeq = "≤"
  show CompEq = "="

data UnaryConn = ConnNeg
  deriving (P.Eq, Generic)

instance Show UnaryConn where
  show ConnNeg = "¬"

instance Hashable UnaryConn

data BinaryConn = ConnAnd | ConnOr | ConnImpl
  deriving (P.Eq, Generic)

instance Hashable BinaryConn

instance Show BinaryConn where
  show ConnAnd = "∧"
  show ConnOr = "∨"
  show ConnImpl = "⇒"

data Form expr
  = FormComp {comp :: BinaryComp, e1 :: expr, e2 :: expr}
  | FormUnary {uconn :: UnaryConn, f1 :: Form expr}
  | FormBinary {bconn :: BinaryConn, f1 :: Form expr, f2 :: Form expr}
  | FormIfThenElse {fc :: Form expr, ft :: Form expr, ff :: Form expr}
  | FormTrue
  | FormFalse
  deriving (P.Eq, Generic)

instance (Hashable expr) => Hashable (Form expr)

instance (Show expr) => Show (Form expr) where
  show :: Form expr -> String
  show (FormComp comp l r) = printf "%s %s %s" (show l) (show comp) (show r)
  show (FormUnary op l) = printf "%s (%s)" (show op) (show l)
  show (FormBinary op l r) = printf "(%s) %s (%s)" (show l) (show op) (show r)
  show (FormIfThenElse c t f) = printf "if (%s) then (%s) else (%s)" (show c) (show t) (show f)
  show FormTrue = "True"
  show FormFalse = "False"

formLeq :: Expr b r -> Expr b r -> Form (Expr b r)
formLeq = FormComp CompLeq

formEq :: Expr b r -> Expr b r -> Form (Expr b r)
formEq = FormComp CompEq

formNeg :: Form expr -> Form expr
formNeg = FormUnary ConnNeg

formAnd :: Form expr -> Form expr -> Form expr
formAnd = FormBinary ConnAnd

formOr :: Form expr -> Form expr -> Form expr
formOr = FormBinary ConnOr

formImpl :: Form expr -> Form expr -> Form expr
formImpl = FormBinary ConnImpl

formIfThenElse :: Form expr -> Form expr -> Form expr -> Form expr
formIfThenElse = FormIfThenElse

instance CanNeg (Form expr) where
  negate = formNeg

instance CanAndOrAsymmetric (Form expr) (Form expr) where
  type AndOrType (Form expr) (Form expr) = (Form expr)
  and2 = formAnd
  or2 = formOr

instance HasIfThenElse (Form expr) (Form expr) where
  type IfThenElseType (Form expr) (Form expr) = (Form expr)
  ifThenElse = formIfThenElse

instance ConvertibleExactly Bool (Form expr) where
  safeConvertExactly True = Right FormTrue
  safeConvertExactly False = Right FormFalse

instance HasOrderAsymmetric (Expr b r) (Expr b r) where
  type OrderCompareType (Expr b r) (Expr b r) = Form (Expr b r)
  leq = formLeq
  lessThan = undefined

instance (CanGetLiteral b r) => HasOrderAsymmetric (Expr b r) Rational where
  type OrderCompareType (Expr b r) Rational = Form (Expr b r)
  leq (e :: e) q = formLeq e (exprLit e.sampleR q :: e)
  lessThan = undefined

instance (CanGetLiteral b r) => HasOrderAsymmetric Rational (Expr b r) where
  type OrderCompareType Rational (Expr b r) = Form (Expr b r)
  leq q (e :: e) = formLeq (exprLit e.sampleR q :: e) e
  lessThan = undefined

instance HasEqAsymmetric (Expr b r) (Expr b r) where
  type EqCompareType (Expr b r) (Expr b r) = Form (Expr b r)
  equalTo = formEq

instance (CanGetLiteral b r) => HasEqAsymmetric (Expr b r) Rational where
  type EqCompareType (Expr b r) Rational = Form (Expr b r)
  equalTo (e :: e) q = formEq e (exprLit e.sampleR q :: e)

instance (CanGetLiteral b r) => HasEqAsymmetric Rational (Expr b r) where
  type EqCompareType Rational (Expr b r) = Form (Expr b r)
  equalTo q (e :: e) = formEq (exprLit e.sampleR q :: e) e
