module App.Form where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.Types (List(..), NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, E, readImpl)
-- import Debug 

import App.Expr (class FromString, Expr(..), fromJustWithDefault, fromString)

data BinaryComp = CompLeq | CompEq

derive instance Generic BinaryComp _

instance Show BinaryComp where
  show CompLeq = "≤"
  show CompEq = "="

instance FromString BinaryComp
  where
  fromString "≤" = Just CompLeq
  fromString "=" = Just CompEq
  fromString _ = Nothing

data UnaryConn = ConnNeg

instance Show UnaryConn where
  show ConnNeg = "¬"

derive instance Generic UnaryConn _

instance FromString UnaryConn
  where
  fromString "¬" = Just ConnNeg
  fromString _ = Nothing

data BinaryConn = ConnAnd | ConnOr | ConnImpl

derive instance Generic BinaryConn _

instance Show BinaryConn where
  show ConnAnd = "∧"
  show ConnOr = "∨"
  show ConnImpl = "⇒"

instance FromString BinaryConn
  where
  fromString "∧" = Just ConnAnd
  fromString "∨" = Just ConnOr
  fromString "⇒" = Just ConnImpl
  fromString _ = Nothing

-- fromString s = trace ("BinaryConn fromString failed for: " <> s) (\_ -> Nothing)

data Form
  = FormComp { comp :: BinaryComp, e1 :: Expr, e2 :: Expr }
  | FormUnary { uconn :: UnaryConn, f1 :: Form }
  | FormBinary { bconn :: BinaryConn, f1 :: Form, f2 :: Form }
  | FormIfThenElse { fc :: Form, ft :: Form, ff :: Form }
  | FormTrue
  | FormFalse

type Form' =
  { tag :: String
  , comp :: Maybe String
  , e1 :: Maybe Expr
  , e2 :: Maybe Expr
  , uconn :: Maybe String
  , bconn :: Maybe String
  , f1 :: Maybe Form
  , f2 :: Maybe Form
  , fc :: Maybe Form
  , ft :: Maybe Form
  , ff :: Maybe Form
  }

fromForm' :: Form' -> E Form
fromForm'
  { tag
  , comp: maybeBinaryComp
  , e1: maybeE1
  , e2: maybeE2
  , uconn: maybeUnaryConn
  , bconn: maybeBinaryConn
  , f1: maybeF1
  , f2: maybeF2
  , fc: maybeFc
  , ft: maybeFt
  , ff: maybeFf
  } =
  let
    comp = fromJustWithDefault CompEq $ fromString =<< maybeBinaryComp
    e1 = fromJustWithDefault (ExprVar { var: "dummyExpr1" }) maybeE1
    e2 = fromJustWithDefault (ExprVar { var: "dummyExpr2" }) maybeE2
    uconn = fromJustWithDefault ConnNeg $ fromString =<< maybeUnaryConn
    bconn = fromJustWithDefault ConnAnd $ fromString =<< maybeBinaryConn
    f1 = fromJustWithDefault FormTrue maybeF1
    f2 = fromJustWithDefault FormTrue maybeF2
    fc = fromJustWithDefault FormTrue maybeFc
    ft = fromJustWithDefault FormTrue maybeFt
    ff = fromJustWithDefault FormTrue maybeFf
  in
    case tag of
      "FormComp" -> Right $ FormComp { comp, e1, e2 }
      "FormUnary" -> Right $ FormUnary { uconn, f1 }
      "FormBinary" -> Right $ FormBinary { bconn, f1, f2 }
      "FormIfThenElse" -> Right $ FormIfThenElse { fc, ft, ff }
      "FormTrue" -> Right $ FormTrue
      "FormFalse" -> Right $ FormFalse
      _ -> Left (NonEmptyList $ ForeignError "Unrecognised step JSON" :| Nil)

instance ReadForeign Form
  where
  readImpl f = do
    form' <- readImpl f
    case fromForm' form' of
      Right form -> pure form
      Left err -> except $ Left err
