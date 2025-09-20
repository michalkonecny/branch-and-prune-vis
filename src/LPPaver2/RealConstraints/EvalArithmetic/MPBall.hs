module LPPaver2.RealConstraints.EvalArithmetic.MPBall () where

import AERN2.MP (MPBall)
import Data.Map qualified as Map
import LPPaver2.RealConstraints.Expr (Var)
import LPPaver2.RealConstraints.Boxes (Box (..))
import LPPaver2.RealConstraints.Eval (CanGetVarDomain (..))
import Text.Printf (printf)
import Prelude

boxGetVarDomain :: Box -> Var -> MPBall
boxGetVarDomain (Box {varDomains}) var =
  case Map.lookup var varDomains of
    Nothing -> error $ printf "variable %s not present in box %s" var (show varDomains)
    Just dom -> dom

instance CanGetVarDomain MPBall where
  getVarDomain _sampleMPBall = boxGetVarDomain
