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
    oldToNew :: OldToNew
  }

-- utility for convenient extraction of all three result elements at once
flattenResult :: SimplifyFormResult r -> (Form, Map.Map ExprHash r, OldToNew)
flattenResult result = (result.evaluatedForm.form, result.evaluatedForm.exprValues, result.oldToNew)

type OldToNew = Map.Map FormHash FormHash

buildResult ::
  OldToNew -> FormHash -> EvaluatedForm r -> SimplifyFormResult r
buildResult oldToNew oldH evaluatedForm =
  SimplifyFormResult
    { evaluatedForm,
      oldToNew = Map.insert oldH evaluatedForm.form.root oldToNew
    }

resultWithH :: SimplifyFormResult r -> FormHash -> SimplifyFormResult r
resultWithH result h =
  resultWithForm result (result.evaluatedForm.form {Form.root = h})

resultWithForm :: SimplifyFormResult r -> Form -> SimplifyFormResult r
resultWithForm result f =
  result {evaluatedForm = result.evaluatedForm {form = f}}

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

    simplifyH prevResult h = simplify (resultWithForm prevResult (formInit {Form.root = h}))

    simplify :: (_) => SimplifyFormResult r -> SimplifyFormResult r
    simplify result0 =
      simplifyNodeReusingPrev form0.root
      where
        (form0, exprValues0, oldToNew0) = flattenResult result0

        simplifyNodeReusingPrev h =
          case getFormDecision form0 of
            -- fast-track the trivial cases
            CertainTrue -> result0
            CertainFalse -> result0
            _ ->
              case Map.lookup h oldToNew0 of
                -- if we have simplified this sub-formula previously, reuse the previous result
                Just newH ->
                  resultWithH result0 newH
                Nothing ->
                  simplifyNode h

        simplifyNode h =
          -- branch by the type of node
          case lookupFormNode form0 h of
            FormComp binComp e1H e2H ->
              -- get the expression values and do the comparison
              let exprValues1 = evalEH e1H exprValues0
                  e1Value = exprValues1 Map.! e1H
                  exprValues12 = evalEH e2H exprValues1
                  e2Value = exprValues12 Map.! e2H
                  comparison = case binComp of
                    CompLe -> e1Value < e2Value
                    CompLeq -> e1Value <= e2Value
                    CompEq -> e1Value == e2Value
                    CompNeq -> e1Value /= e2Value
                  buildR (f :: Form) = buildResult oldToNew0 h (EvaluatedForm {form = f, exprValues = exprValues12})
               in case comparison of
                    -- if decided, replace by constant True/False, keeping track of evaluated expressions
                    CertainTrue -> buildR formTrue
                    CertainFalse -> buildR formFalse
                    -- if undecided, keep formula unchanged, keeping track of evaluated expressions
                    _ -> buildR (form0 {Form.root = h})
            FormUnary ConnNeg f1H ->
              let result1 = simplifyH result0 f1H
                  (simplifiedF1, exprValues1, oldToNew1) = flattenResult result1
                  buildR (f :: Form) = buildResult oldToNew1 h (EvaluatedForm {form = f, exprValues = exprValues1})
                  decision1 = getFormDecision simplifiedF1
               in case decision1 of
                    CertainTrue -> buildR formFalse -- negating
                    CertainFalse -> buildR formTrue -- negating
                    _ -> buildR (negate simplifiedF1)
            FormBinary binaryConn f1H f2H ->
              let result1 = simplifyH result0 f1H
                  (simplifiedF1, _, _) = flattenResult result1
                  result2 = simplifyH result1 f2H
                  (simplifiedF2, exprValues12, oldToNew12) = flattenResult result2
                  buildR (f :: Form) = buildResult oldToNew12 h (EvaluatedForm {form = f, exprValues = exprValues12})
                  decision1 = getFormDecision simplifiedF1
                  decision2 = getFormDecision simplifiedF2
               in case binaryConn of
                    ConnAnd ->
                      case (decision1, decision2) of
                        (CertainFalse, _) -> buildR formFalse
                        (_, CertainFalse) -> buildR formFalse
                        (CertainTrue, _) -> buildR simplifiedF2
                        (_, CertainTrue) -> buildR simplifiedF1
                        _ -> buildR $ simplifiedF1 && simplifiedF2
                    ConnOr ->
                      case (decision1, decision2) of
                        (CertainTrue, _) -> buildR formTrue
                        (_, CertainTrue) -> buildR formTrue
                        (CertainFalse, _) -> buildR simplifiedF2
                        (_, CertainFalse) -> buildR simplifiedF1
                        _ -> buildR $ simplifiedF1 || simplifiedF2
                    ConnImpl ->
                      case (decision1, decision2) of
                        (CertainFalse, _) -> buildR formTrue
                        (_, CertainTrue) -> buildR formTrue
                        (CertainTrue, _) -> buildR simplifiedF2
                        (_, CertainFalse) -> buildR $ negate simplifiedF1
                        _ -> buildR $ formImpl simplifiedF1 simplifiedF2
            FormIfThenElse fcH ftH ffH ->
              let resultC = simplifyH result0 fcH
                  (simplifiedC, _, _) = flattenResult resultC
                  resultT = simplifyH resultC ftH
                  (simplifiedT, _, _) = flattenResult resultT
                  resultF = simplifyH resultT ffH
                  (simplifiedF, exprValuesCTF, oldToNewCTF) = flattenResult resultF
                  buildR (f :: Form) = buildResult oldToNewCTF h (EvaluatedForm {form = f, exprValues = exprValuesCTF})
                  decisionC = getFormDecision simplifiedC
                  decisionT = getFormDecision simplifiedC
                  decisionF = getFormDecision simplifiedC
               in case (decisionC, decisionT, decisionF) of
                    (CertainTrue, _, _) -> buildR simplifiedT -- "then" branch
                    (CertainFalse, _, _) -> buildR simplifiedF -- "else" branch
                    (_, CertainTrue, CertainTrue) -> buildR formTrue -- undecided but both branches "true"
                    (_, CertainFalse, CertainFalse) -> buildR formFalse -- undecided but both branches "false"
                    _ -> buildR $ formIfThenElse simplifiedC simplifiedT simplifiedF -- undecided, keep if-then-else
            FormTrue -> error "Internal error: FormTrue case should be caught earlier"
            FormFalse -> error "Internal error: FormFalse case should be caught earlier"
