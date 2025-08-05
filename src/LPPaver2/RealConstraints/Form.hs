{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LPPaver2.RealConstraints.Form
  ( Form (..),
    lookupFormNode,
    FormHash,
    FormNode,
    FormF (..),
    UnaryConn (..),
    BinaryConn (..),
    BinaryComp (..),
    formTrue,
    formFalse,
    getFormDecision,
    formImpl,
    formIfThenElse,
  )
where

import AERN2.Kleenean (Kleenean (CertainFalse, CertainTrue, TrueOrFalse))
import Data.Hashable (Hashable (hash))
import Data.Map qualified as Map
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Expr
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P

data FormF f
  = FormComp {comp :: BinaryComp, e1 :: ExprHash, e2 :: ExprHash}
  | FormUnary {uconn :: UnaryConn, f1 :: f}
  | FormBinary {bconn :: BinaryConn, f1 :: f, f2 :: f}
  | FormIfThenElse {fc :: f, ft :: f, ff :: f}
  | FormTrue
  | FormFalse
  deriving (P.Eq, Generic, Hashable)

data BinaryComp = CompLe | CompLeq | CompEq | CompNeq
  deriving (P.Eq, Generic, Hashable)

instance Show BinaryComp where
  show CompLe = "<"
  show CompLeq = "≤"
  show CompEq = "="
  show CompNeq = "≠"

data UnaryConn = ConnNeg
  deriving (P.Eq, Generic, Hashable)

instance Show UnaryConn where
  show ConnNeg = "¬"

data BinaryConn = ConnAnd | ConnOr | ConnImpl
  deriving (P.Eq, Generic, Hashable)

instance Show BinaryConn where
  show ConnAnd = "∧"
  show ConnOr = "∨"
  show ConnImpl = "⇒"

type FormHash = Int

type FormNode = FormF FormHash

data Form = Form
  { nodesE :: Map.Map ExprHash ExprNode,
    -- | The map has to include all hashes reachable from the root, except FormTrue and FormFalse
    nodesF :: Map.Map FormHash FormNode,
    root :: FormHash
  }

-- | Use this instead of `form.nodesF` to lookup a node in `form`.
--
-- This function understands True and False in addition to `form.nodesF`.
lookupFormNode :: Form -> FormHash -> FormNode
lookupFormNode form h =
  case Map.lookup h form.nodesF of
    Just node -> node
    Nothing
      | h == formTrue.root -> FormTrue
      | h == formFalse.root -> FormFalse
      | otherwise -> error "Missing node in formula"

instance P.Eq Form where
  f1 == f2 = f1.root == f2.root

instance Show Form where
  show form = showFormHash form.root
    where
      showFormHash formHash =
        case Map.lookup formHash form.nodesF of
          Nothing -> error "A hash is missing from form.nodesF"
          Just formNode -> showFormF showExprHash showFormHash formNode
      showExprHash exprHash =
        show (Expr {nodes = form.nodesE, root = exprHash})

showFormF :: (ExprHash -> String) -> (f -> String) -> FormF f -> String
showFormF showE showF formNode =
  case formNode of
    FormComp comp e1 e2 -> printf "%s %s %s" (showE e1) (show comp) (showE e2)
    FormUnary op f1 -> printf "%s (%s)" (show op) (showF f1)
    FormBinary op f1 f2 -> printf "(%s) %s (%s)" (showF f1) (show op) (showF f2)
    FormIfThenElse c t f -> printf "if (%s) then (%s) else (%s)" (showF c) (showF t) (showF f)
    FormTrue -> "True"
    FormFalse -> "False"

form0 :: FormNode -> Form
form0 f =
  Form {nodesE = Map.empty, nodesF = Map.singleton root f, root}
  where
    root = hash f

-- | apply a unary connector to a form
form1 :: UnaryConn -> Form -> Form
form1 uconn f1 =
  Form {nodesE = f1.nodesE, nodesF = Map.insert h e f1.nodesF, root = h}
  where
    e = FormUnary {uconn, f1 = f1.root}
    h = hash e

-- | apply a binary connector to a pair of forms
form2 :: BinaryConn -> Form -> Form -> Form
form2 bconn f1 f2 =
  Form
    { nodesE = Map.union f1.nodesE f2.nodesE,
      nodesF = Map.insert h e $ Map.union f1.nodesF f2.nodesF,
      root = h
    }
  where
    e = FormBinary {bconn, f1 = f1.root, f2 = f2.root}
    h = hash e

formIfThenElse :: Form -> Form -> Form -> Form
formIfThenElse fc ft ff =
  Form
    { nodesE = Map.unions [fc.nodesE, ft.nodesE, ff.nodesE],
      nodesF = Map.insert h e $ Map.unions [fc.nodesF, ft.nodesF, ff.nodesF],
      root = h
    }
  where
    e = FormIfThenElse {fc = fc.root, ft = ft.root, ff = ff.root}
    h = hash e

formComp :: BinaryComp -> Expr -> Expr -> Form
formComp comp e1 e2 =
  Form
    { nodesE = Map.union e1.nodes e2.nodes,
      nodesF = Map.singleton h e,
      root = h
    }
  where
    e = FormComp {comp, e1 = e1.root, e2 = e2.root}
    h = hash e

formTrue :: Form
formTrue = form0 FormTrue

formFalse :: Form
formFalse = form0 FormFalse

getFormDecision :: Form -> Kleenean
getFormDecision form
  | form.root == formTrue.root = CertainTrue
  | form.root == formFalse.root = CertainFalse
  | otherwise = TrueOrFalse

formLe :: Expr -> Expr -> Form
formLe = formComp CompLe

formLeq :: Expr -> Expr -> Form
formLeq = formComp CompLeq

formEq :: Expr -> Expr -> Form
formEq = formComp CompEq

formNeq :: Expr -> Expr -> Form
formNeq = formComp CompNeq

formNeg :: Form -> Form
formNeg = form1 ConnNeg

formAnd :: Form -> Form -> Form
formAnd = form2 ConnAnd

formOr :: Form -> Form -> Form
formOr = form2 ConnOr

formImpl :: Form -> Form -> Form
formImpl = form2 ConnImpl

instance CanNeg Form where
  negate = formNeg

instance CanAndOrAsymmetric Form Form where
  type AndOrType Form Form = Form
  and2 = formAnd
  or2 = formOr

instance HasIfThenElse Form Form where
  type IfThenElseType Form Form = Form
  ifThenElse = formIfThenElse

instance ConvertibleExactly Bool Form where
  safeConvertExactly True = Right formTrue
  safeConvertExactly False = Right formFalse

instance HasOrderAsymmetric Expr Expr where
  type OrderCompareType Expr Expr = Form
  leq = formLeq
  lessThan = formLe

instance HasOrderAsymmetric Expr Rational where
  type OrderCompareType Expr Rational = Form
  leq e q = formLeq e (exprLit q)
  lessThan e q = formLe e (exprLit q)

instance HasOrderAsymmetric Rational Expr where
  type OrderCompareType Rational Expr = Form
  leq q = formLeq (exprLit q)
  lessThan q = formLe (exprLit q)

instance HasEqAsymmetric Expr Expr where
  type EqCompareType Expr Expr = Form
  equalTo = formEq
  notEqualTo = formNeq

instance HasEqAsymmetric Expr Rational where
  type EqCompareType Expr Rational = Form
  equalTo e q = formEq e (exprLit q)
  notEqualTo e q = formNeq e (exprLit q)

instance HasEqAsymmetric Rational Expr where
  type EqCompareType Rational Expr = Form
  equalTo q = formEq (exprLit q)
  notEqualTo q = formNeq (exprLit q)
