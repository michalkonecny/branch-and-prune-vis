{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module LPPaver2.RealConstraints.Eval
  ( evalExpr,
    CanGetVarDomain (..),
    simplifyEvalForm,
    SimplifyFormResult (..),
    EvaluatedForm (..),
  )
where

import AERN2.Kleenean
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Boxes (Box)
import LPPaver2.RealConstraints.Expr
  ( BinaryOp (..),
    Expr (..),
    ExprF (..),
    ExprHash,
    UnaryOp (..),
    Var,
  )
import LPPaver2.RealConstraints.Form
import LPPaver2.RealConstraints.Form qualified as Form

import MixedTypesNumPrelude

class CanGetVarDomain r where
  -- | the first parameter is a value sample to help type inference
  getVarDomain :: r -> Box -> Var -> r

evalExpr ::
  ( CanGetVarDomain r,
    Ring r,
    CanDivSameType r,
    HasRationalsWithSample r,
    CanSqrtSameType r,
    CanSinCosSameType r
  ) =>
  r ->
  Box ->
  Expr ->
  Map.Map ExprHash r ->
  Map.Map ExprHash r
evalExpr sampleR box expr =
  evalNode expr.root
  where
    nodes = expr.nodes
    evalNode h valuesSoFar =
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
                  let valuesAfterE1 = evalNode e1 valuesSoFar
                      e1Value = valuesAfterE1 Map.! e1
                   in Map.insert h (evalUnop unop e1Value) valuesAfterE1
                ExprBinary {binop, e1, e2} ->
                  let valuesAfterE1 = evalNode e1 valuesSoFar
                      e1Value = valuesAfterE1 Map.! e1
                      valuesAfterE2 = evalNode e2 valuesAfterE1
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

type HasKleenanComparison r =
  ( HasOrder r r,
    OrderCompareType r r ~ Kleenean,
    HasEq r r,
    EqCompareType r r ~ Kleenean
  )

-- |
--  A formula with the values of all its sub-expresions, the intermediate values
--  of evaluating the truth value of the formula over some set.
--
--  As the formulas are aggresively simplified while evaluating, their
--  truth value can be tested using `getFormDecision` which simply compares
--  the formula to FormTrue and FormFalse,
data EvaluatedForm r = EvaluatedForm
  { form :: Form,
    exprValues :: Map.Map ExprHash r
  }

data SimplifyFormResult r = SimplifyFormResult
  { evaluatedForm :: EvaluatedForm r,
    oldToNew :: Map.Map FormHash FormHash
  }

buildResult ::
  SimplifyFormResult r -> FormHash -> EvaluatedForm r -> SimplifyFormResult r
buildResult prevResult oldH evaluatedForm =
  SimplifyFormResult
    { evaluatedForm,
      oldToNew = Map.insert oldH evaluatedForm.form.root prevResult.oldToNew
    }

simplifyEvalForm ::
  ( CanGetVarDomain r,
    Ring r,
    CanDivSameType r,
    HasRationalsWithSample r,
    CanSqrtSameType r,
    CanSinCosSameType r,
    HasKleenanComparison r
  ) =>
  r ->
  Box ->
  Form ->
  SimplifyFormResult r
simplifyEvalForm (sapleR :: r) box formInit =
  simplify
    SimplifyFormResult
      { evaluatedForm =
          EvaluatedForm {form = formInit, exprValues = Map.empty},
        oldToNew = Map.empty
      }
  where
    -- Traverse the formula nodes, evaluating over the box to true/false/don't know and
    -- replacing decided sub-formulas with True / False nodes.
    -- While traversing, we accummulate dictionaries of expressions.

    -- As expression structure does not change, we can use the initial expression nodes dictionary
    -- in all sub-formulas.  Prepare a shortcut for evaluating an expression given by its hash:
    evalEH eH = evalExpr sapleR box (Expr {nodes = formInit.nodesE, root = eH})

    simplify :: (_) => SimplifyFormResult r -> SimplifyFormResult r
    simplify resultSoFar =
      simplifyNodeReusingPrev eForm.root
      where
        evaluatedForm = resultSoFar.evaluatedForm
        eForm = evaluatedForm.form
        exprValues0 = evaluatedForm.exprValues

        simplifyH h =
          -- TODO pass resultPrev parameter
          simplify
            (resultSoFar {evaluatedForm = evaluatedForm {form = eForm {Form.root = h}}})

        simplifyNodeReusingPrev h =
          case Map.lookup h resultSoFar.oldToNew of
            -- if we have simplified this sub-formula previously, reuse the previous result
            Just newH ->
              resultSoFar {evaluatedForm = evaluatedForm {form = eForm {Form.root = newH}}} -- TODO avoid repeating this
            Nothing ->
              simplifyNode h

        simplifyNode h =
          -- branch by the type of node
          case lookupFormNode eForm h of
            FormComp binComp e1H e2H ->
              -- get the expression values and do the comparison
              let valuesAfterE1 = evalEH e1H exprValues0
                  e1Value = valuesAfterE1 Map.! e1H
                  valuesAfterE2 = evalEH e2H valuesAfterE1
                  e2Value = valuesAfterE2 Map.! e2H
                  comparison = case binComp of
                    CompLe -> e1Value < e2Value
                    CompLeq -> e1Value <= e2Value
                    CompEq -> e1Value == e2Value
                    CompNeq -> e1Value /= e2Value
                  buildR (f :: Form) = buildResult resultSoFar h (EvaluatedForm {form = f, exprValues = valuesAfterE2})
               in case comparison of
                    -- if decided, replace by constant True/False, keeping track of evaluated expressions
                    CertainTrue -> buildR formTrue
                    CertainFalse -> buildR formFalse
                    -- if undecided, keep formula unchanged, keeping track of evaluated expressions
                    _ -> buildR (eForm {Form.root = h})
            FormUnary ConnNeg f1H ->
              let f1Result = simplifyH f1H
                  valuesAfterF1 = f1Result.evaluatedForm.exprValues
                  buildR (f :: Form) = buildResult f1Result h (EvaluatedForm {form = f, exprValues = valuesAfterF1})
               in case getFormDecision f1Result.evaluatedForm.form of
                    CertainTrue -> buildR formFalse -- negating
                    CertainFalse -> buildR formTrue -- negating
                    _ -> buildR (negate f1Result.evaluatedForm.form)
            _ -> undefined

-- simplify (FormBinary ConnAnd f1 f2) =
--   case (simplify f1, simplify f2) of
--     (FormFalse, _) -> FormFalse
--     (_, FormFalse) -> FormFalse
--     (FormTrue, simplifiedF2) -> simplifiedF2
--     (simplifiedF1, FormTrue) -> simplifiedF1
--     (simplifiedF1, simplifiedF2) -> FormBinary ConnAnd simplifiedF1 simplifiedF2
-- simplify (FormBinary ConnOr f1 f2) =
--   case (simplify f1, simplify f2) of
--     (FormTrue, _) -> FormTrue
--     (_, FormTrue) -> FormTrue
--     (FormFalse, simplifiedF2) -> simplifiedF2
--     (simplifiedF1, FormFalse) -> simplifiedF1
--     (simplifiedF1, simplifiedF2) -> FormBinary ConnOr simplifiedF1 simplifiedF2
-- simplify (FormBinary ConnImpl f1 f2) =
--   case (simplify f1, simplify f2) of
--     (FormFalse, _) -> FormTrue
--     (_, FormTrue) -> FormTrue
--     (FormTrue, simplifiedF2) -> simplifiedF2
--     (simplifiedF1, FormFalse) -> FormUnary ConnNeg simplifiedF1
--     (simplifiedF1, simplifiedF2) -> FormBinary ConnImpl simplifiedF1 simplifiedF2
-- simplify (FormIfThenElse fc ft ff) =
--   case (simplify fc, simplify ft, simplify ff) of
--     (FormTrue, simplifiedT, _) -> simplifiedT
--     (FormFalse, _, simplifiedF) -> simplifiedF
--     (_, FormTrue, FormTrue) -> FormTrue
--     (_, FormFalse, FormFalse) -> FormFalse
--     (simplifiedC, simplifiedT, simplifiedF) -> FormIfThenElse simplifiedC simplifiedT simplifiedF
-- simplify FormTrue = FormTrue
-- simplify FormFalse = FormFalse
