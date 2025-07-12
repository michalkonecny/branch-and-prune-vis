module LPPaver2.RealConstraints.Eval.MPBall () where

import AERN2.MP (MPBall)
import AERN2.MP qualified as MP
import Data.Map qualified as Map
import LPPaver2.RealConstraints (CanGetLiteral (..), CanGetVarDomain (..), Var)
import LPPaver2.RealConstraints.Boxes (Box (..))
import Text.Printf (printf)

boxGetVarDomain :: Box -> Var -> MPBall
boxGetVarDomain (Box {..}) var =
  case Map.lookup var varDomains of
    Nothing -> error $ printf "variable %s not present in box %s" var (show varDomains)
    Just dom -> dom

instance CanGetLiteral Box MPBall where
  getLiteral sampleMPBall _box = MP.mpBallP (MP.getPrecision sampleMPBall)

instance CanGetVarDomain Box MPBall where
  getVarDomain _sampleMPBall = boxGetVarDomain
