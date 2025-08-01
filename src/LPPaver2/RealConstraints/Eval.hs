module LPPaver2.RealConstraints.Eval
  ( evalExpr,
    CanGetVarDomain (..),
  )
where

import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Boxes (Box)
import LPPaver2.RealConstraints.Expr (BinaryOp (OpDivide, OpMinus, OpPlus, OpTimes), Expr (..), ExprF (..), ExprHash, UnaryOp (OpCos, OpNeg, OpSin, OpSqrt), Var)
import MixedTypesNumPrelude

class CanGetVarDomain r where
  -- | the first parameter is a value sample to help type inference
  getVarDomain :: r -> Box -> Var -> r

evalExpr ::
  ( CanGetVarDomain r,
    Field r,
    CanSqrtSameType r,
    CanSinCosSameType r
  ) =>
  r ->
  Box ->
  Expr ->
  Map.Map ExprHash r
evalExpr sampleR box expr =
  traverseNodes expr.root Map.empty
  where
    nodes = expr.nodes
    traverseNodes h valuesSoFar =
      case Map.lookup h valuesSoFar of
        -- if this node's value is already in the value dictionary, return the dictionary as is
        Just _ -> valuesSoFar
        Nothing ->
          -- lookup the node details
          case Map.lookup h nodes of
            Nothing -> error "evalExpr: a hash is missing from expr.nodes"
            Just node ->
              -- evaluate the node and it to the value dictionary
              case node of
                ExprVar {var} -> Map.insert h (getVarDomain sampleR box var) valuesSoFar
                ExprLit {lit} -> Map.insert h (convertExactlyWithSample sampleR lit) valuesSoFar
                ExprUnary {unop, e1} ->
                  let valuesAfterE1 = traverseNodes e1 valuesSoFar
                      e1Value = valuesAfterE1 Map.! e1
                   in Map.insert h (evalUnop unop e1Value) valuesAfterE1
                ExprBinary {binop, e1, e2} ->
                  let valuesAfterE1 = traverseNodes e1 valuesSoFar
                      e1Value = valuesAfterE1 Map.! e1
                      valuesAfterE2 = traverseNodes e2 valuesAfterE1
                      e2Value = valuesAfterE2 Map.! e2
                   in Map.insert h (evalBinop binop e1Value e2Value) valuesAfterE2

evalUnop ::
  ( CanNegSameType r,
    CanSqrtSameType r,
    CanSinCosSameType r
  ) =>
  UnaryOp ->
  r ->
  r
evalUnop OpNeg val = -val
evalUnop OpSqrt val = sqrt val
evalUnop OpSin val = sin val
evalUnop OpCos val = cos val

evalBinop ::
  ( CanAddSameType r,
    CanSubSameType r,
    CanMulSameType r,
    CanDivSameType r
  ) =>
  BinaryOp ->
  r ->
  r ->
  r
evalBinop OpPlus v1 v2 = v1 + v2
evalBinop OpMinus v1 v2 = v1 - v2
evalBinop OpTimes v1 v2 = v1 * v2
evalBinop OpDivide v1 v2 = v1 / v2

-- TODO: simplifyForm :: r -> Box -> Form -> (Form, Map.Map FormHash Kleenean)