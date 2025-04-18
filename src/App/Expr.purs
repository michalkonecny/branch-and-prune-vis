module App.Expr where

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

class FromString t where
  fromString :: String -> Maybe t

data BinaryOp = OpPlus | OpMinus | OpTimes | OpDivide

derive instance Generic BinaryOp _

instance Show BinaryOp where
  show OpPlus = "+"
  show OpMinus = "-"
  show OpTimes = "×"
  show OpDivide = "/"

instance FromString BinaryOp
  where
  fromString "+" = Just OpPlus
  fromString "-" = Just OpMinus
  fromString "*" = Just OpTimes
  fromString "/" = Just OpDivide
  fromString _ = Nothing

data UnaryOp = OpNeg | OpSqrt | OpSin | OpCos

instance Show UnaryOp where
  show OpNeg = "-"
  show OpSqrt = "sqrt"
  show OpSin = "sin"
  show OpCos = "cos"

derive instance Generic UnaryOp _

instance FromString UnaryOp
  where
  fromString "-" = Just OpNeg
  fromString "sqrt" = Just OpSqrt
  fromString "sin" = Just OpSin
  fromString "cos" = Just OpCos
  fromString _ = Nothing

data Expr
  = ExprVar { var :: String }
  | ExprLit { lit :: Number }
  | ExprUnary { unop :: UnaryOp, e1 :: Expr }
  | ExprBinary { binop :: BinaryOp, e1 :: Expr, e2 :: Expr }

type Expr' =
  { tag :: String
  , var :: Maybe String
  , lit :: Maybe { denominator :: Number, numerator :: Number }
  , unop :: Maybe String
  , binop :: Maybe String
  , e1 :: Maybe Expr
  , e2 :: Maybe Expr
  }

fromExpr' :: Expr' -> E Expr
fromExpr'
  { tag
  , var: maybeVar
  , lit: maybeLit
  , unop: maybeUnaryOp
  , binop: maybeBinaryOp
  , e1: maybeE1
  , e2: maybeE2
  } =
  let
    var = fromJustWithDefault "dummyVar" maybeVar
    lit = fromJustWithDefault 0.0 $
      (\{ denominator, numerator } -> Just $ numerator / denominator) =<< maybeLit
    e1 = fromJustWithDefault (ExprVar { var: "dummyExpr1" }) maybeE1
    e2 = fromJustWithDefault (ExprVar { var: "dummyExpr2" }) maybeE2
    unop = fromJustWithDefault OpNeg $ fromString =<< maybeUnaryOp
    binop = fromJustWithDefault OpPlus $ fromString =<< maybeBinaryOp
  in
    case tag of
      "ExprVar" -> Right $ ExprVar { var }
      "ExprLit" -> Right $ ExprLit { lit }
      "ExprUnary" -> Right $ ExprUnary { unop, e1 }
      "ExprBinary" -> Right $ ExprBinary { binop, e1, e2 }
      _ -> Left (NonEmptyList $ ForeignError "Unrecognised step JSON" :| Nil)

fromJustWithDefault :: forall t. t -> Maybe t -> t
fromJustWithDefault _ (Just value) = value
fromJustWithDefault def _ = def

instance ReadForeign Expr
  where
  readImpl f = do
    expr' <- readImpl f
    case fromExpr' expr' of
      Right expr -> pure expr
      Left err -> except $ Left err
