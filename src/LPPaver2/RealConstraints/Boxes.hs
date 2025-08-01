{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}

module LPPaver2.RealConstraints.Boxes
  ( --
    Box (..),
    mkBox,
    Boxes (..),
    splitBox,
    BoxProblem,
  )
where

import AERN2.MP (MPBall)
import AERN2.MP qualified as MP
import AERN2.MP.Affine.Type ()
import AERN2.MP.Ball (CentreRadius (CentreRadius))
import AERN2.MP.Ball.Type qualified as MP
import AERN2.MP.Dyadic (dyadic)
-- has instance Hashable MPBall (TODO: move that instance)

import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.Sets (Subset)
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.RealConstraints.Expr (Var)
import MixedTypesNumPrelude
import Text.Printf (printf)
import Prelude qualified as P
import LPPaver2.RealConstraints.Form (Form)

{- N-dimensional Boxes -}

data Box = Box {varDomains :: Map.Map Var MP.MPBall, splitOrder :: [Var]}
  deriving (Generic, Hashable)

instance Show Box where
  show (Box {..}) =
    printf "[%s]" $ List.intercalate ", " $ map showVarDom $ Map.toList varDomains
    where
      showVarDom :: (Var, MPBall) -> String
      showVarDom (var, ball) = printf "%s ∈ [%s..%s]" var (show (double l)) (show (double u))
        where
          (l, u) = MP.endpoints ball

instance P.Eq Box where
  b1 == b2 = varDomainsR b1 == varDomainsR b2

varDomainsR :: Box -> [(Var, (Rational, Rational))]
varDomainsR b = sortOn fst $ map fromBall (Map.toList b.varDomains)
  where
    fromBall (var, ball) = (var, (rational lR, rational uR))
      where
        (lR, uR) = MP.endpoints ball

instance MP.HasPrecision Box where
  getPrecision (Box {}) = MP.defaultPrecision -- TODO : use precision from varDomains if possible

mkBox :: [(Var, (Rational, Rational))] -> Box
mkBox varDomainsRational =
  Box
    { varDomains = Map.fromList (map toBall varDomainsRational),
      splitOrder = map fst varDomainsRational
    }
  where
    toBall (var, (lR, uR)) = (var, MP.mpBall (CentreRadius mR rR))
      where
        mR = (lR + uR) / 2
        rR = (uR - lR) / 2

boxAreaD :: Box -> Double
boxAreaD (Box {..}) = product (map (double . dyadic . MP.radius) (Map.elems varDomains))

{- Collections of boxes. -}

data Boxes
  = Boxes [Box]
  | BoxesUnion [Boxes]
  deriving (P.Eq, Show, Generic)

boxesCount :: Boxes -> Integer
boxesCount (Boxes boxes) = length boxes
boxesCount (BoxesUnion union) = sum (map boxesCount union)

boxesAreaD :: Boxes -> Double
boxesAreaD (Boxes boxes) = sum (map boxAreaD boxes)
boxesAreaD (BoxesUnion union) = sum (map boxesAreaD union)

instance BP.ShowStats (Subset Boxes Box) where
  showStats (BP.Subset {..}) =
    printf "{|boxes| = %d, coverage = %3.4f%%}" (boxesCount subset) coveragePercent
    where
      coveragePercent = 100 * (boxesAreaD subset / boxAreaD superset)

instance BP.IsSet Boxes where
  emptySet = Boxes []
  setIsEmpty (Boxes boxes) = null boxes
  setIsEmpty (BoxesUnion union) = List.all BP.setIsEmpty union
  setUnion bs1 bs2 = BoxesUnion [bs1, bs2]

instance BP.BasicSetsToSet Box Boxes where
  basicSetsToSet = Boxes

instance BP.CanSplitProblem constraint Box where
  splitProblem (BP.Problem {scope, constraint}) =
    map (\box -> BP.Problem {scope = box, constraint}) $ splitBox scope

splitBox :: Box -> [Box]
splitBox box = case box.splitOrder of
  [] -> [box] -- No split order?? Perhaps there are no variables that can be split, ie are exact points.
  (splitVar : splitRest) ->
    -- We split the first variable in the list.
    let splitOrder = splitRest ++ [splitVar] -- Cycle the variables round-robin-like.
     in case Map.lookup splitVar box.varDomains of
          Nothing -> [box] -- The split variable does not exist...
          Just splitVarDomain ->
            [ Box {varDomains = varDomainsL, splitOrder},
              Box {varDomains = varDomainsU, splitOrder}
            ]
            where
              (splitVarDomainL, splitVarDomainU) = splitMPBall splitVarDomain
              varDomainsL = Map.insert splitVar splitVarDomainL box.varDomains
              varDomainsU = Map.insert splitVar splitVarDomainU box.varDomains

splitMPBall :: MPBall -> (MPBall, MPBall)
splitMPBall b = (bL, bU)
  where
    (l, u) = MP.mpBallEndpoints b
    m = (l + u) / 2 -- TODO: adjust precision if needed to get the exact middle
    bL = MP.fromMPBallEndpoints l m
    bU = MP.fromMPBallEndpoints m u

type BoxProblem = BP.Problem Form Box
