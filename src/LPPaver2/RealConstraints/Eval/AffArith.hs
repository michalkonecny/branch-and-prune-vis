module LPPaver2.RealConstraints.Eval.AffArith () where

import AERN2.MP.Affine (MPAffine)
import AERN2.MP.Affine qualified as Aff
import Data.Map qualified as Map
import LPPaver2.RealConstraints (CanGetLiteral (..), CanGetVarDomain (..), Var)
import LPPaver2.RealConstraints.Boxes (Box (..))
import Text.Printf (printf)

boxGetVarDomain :: MPAffine -> Box -> Var -> MPAffine
boxGetVarDomain sampleAff (Box {varDomains}) var =
  case Map.lookup var varDomains of
    Nothing -> error $ printf "variable %s not present in box %s" var (show varDomains)
    Just dom -> Aff.mpAffineFromBall sampleAff errId dom
  where
    errId = var -- using variable's name as the error variable ID

instance CanGetLiteral Box MPAffine where
  getLiteral sampleAff _box = Aff.mpAffineWithSample sampleAff

instance CanGetVarDomain Box MPAffine where
  getVarDomain = boxGetVarDomain
