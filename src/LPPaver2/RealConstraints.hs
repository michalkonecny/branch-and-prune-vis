module LPPaver2.RealConstraints
  ( module LPPaver2.RealConstraints.Expr,
    module LPPaver2.RealConstraints.Form,
    module LPPaver2.RealConstraints.Boxes,
    module LPPaver2.RealConstraints.Eval,
  )
where

import LPPaver2.RealConstraints.Boxes
import LPPaver2.RealConstraints.Expr
import LPPaver2.RealConstraints.Form
import LPPaver2.RealConstraints.Eval
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.EvalArithmetic.AffArith ()

-- TODO remove all code below and restore module exports
-- Temporary ad-hoc testing code follows
-- open using 
-- > stack repl src/LPPaver2/RealConstraints.hs 

import AERN2.MP
import MixedTypesNumPrelude
import qualified Data.Map as Map
import GHC.Records (HasField(..))


sampleMB :: MPBall
sampleMB = mpBall 1

x :: Expr
x = exprVar "x"

b :: Box
b = mkBox [("x", (1.0, 2.0))]

e :: Expr
e = sqrt(2.0*x*2.0)

evalResult :: Map.Map ExprHash MPBall
evalResult = evalExpr sampleMB b e Map.empty

valueOfE :: MPBall
valueOfE = evalResult Map.! e.root

valueOf2X :: MPBall
valueOf2X = evalResult Map.! (2.0*x).root

f :: Form
-- f = e == 3.0 -- False
-- f = negate $ e == 3.0 -- True
-- f = negate $ e == 2.0 -- undecided, unsimplified
-- f = negate $ 1.0 < e -- False
-- f = 1.0 < e && 2.0 < e -- simplifies to 2.0 < e
f = (1.0 < e && 2.0 < e) || 2.1 < e -- simplifies to 2.0 < e || 2.1 < e

simplifyResult :: SimplifyFormResult MPBall
simplifyResult = simplifyEvalForm sampleMB b f

simplifiedF :: Form
simplifiedF = simplifyResult.evaluatedForm.form

oldToNewMap :: Map.Map FormHash FormHash
oldToNewMap = simplifyResult.oldToNew