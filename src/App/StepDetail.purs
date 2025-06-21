module App.StepDetail
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , State
  , StepInfo(..)
  , component
  ) where

import Prelude

import App.Expr (Expr(..))
import App.Form (Form(..))
import App.Steps (Interval, Problem, Step, Var)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number.Format as Num
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = Maybe StepInfo

type StepInfo =
  { step :: Step
  , problem :: Problem
  }

type Input = State

data Query a = VoidQuery a

type Output = Void

type Slot id = H.Slot Query Output id

initialState :: Input -> State
initialState _ = Nothing

data Action = NewState State

component :: forall m. (MonadAff m) => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render: renderFocusedStep
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< NewState
        }
    }

handleAction :: forall cs m. (MonadAff m) => Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  NewState state -> do
    H.put state

renderFocusedStep :: forall cs m. State -> H.ComponentHTML Action cs m
renderFocusedStep Nothing = HH.div_ []
renderFocusedStep (Just { problem }) =
  HH.div_
    (boxDescription <> [ (renderForm problem.constraint).html ])
  where
  (varRanges :: Array _) = Map.toUnfoldable $ problem.scope.varDomains
  boxDescription = Array.concat $ map describeVar varRanges

  describeVar :: (Tuple Var Interval) -> Array _
  describeVar (Tuple var { l, u }) = [ HH.text descr, HH.br_ ]
    where
    descr = var <> " ∈ [ " <> (showNumber l) <> ", " <> showNumber u <> " ]"

renderForm :: forall cs m. Form -> { html :: H.ComponentHTML Action cs m, width :: Int }
renderForm FormTrue = renderedString "True"
renderForm FormFalse = renderedString "False"
renderForm (FormComp { comp, e1, e2 }) =
  alignHorizOrVert [ renderExpr e1, renderedString (show comp), renderExpr e2 ]
renderForm (FormUnary { uconn, f1 }) =
  alignHorizOrVert [ renderedString (show uconn), renderForm f1 ]
renderForm (FormBinary { bconn, f1, f2 }) =
  alignHorizOrVert [ renderForm f1, renderedString (show bconn), renderForm f2 ]
renderForm (FormIfThenElse { fc, ft, ff }) =
  alignHorizOrVert
    [ renderedString "if"
    , renderForm fc
    , renderedString "then"
    , renderForm ft
    , renderedString "else"
    , renderForm ff
    ]

renderExpr :: forall cs m. Expr -> { html :: H.ComponentHTML Action cs m, width :: Int }
renderExpr (ExprVar { var }) = renderedStringDiv var
renderExpr (ExprLit { lit }) = renderedStringDiv (showNumber lit)
renderExpr (ExprUnary { unop, e1 }) =
  alignHorizOrVert [ renderedString (show unop), renderExpr e1 ]
renderExpr (ExprBinary { binop, e1, e2 }) =
  alignHorizOrVert [ renderExpr e1, renderedString (show binop), renderExpr e2 ]

showNumber ∷ Number → String
showNumber = Num.toStringWith (Num.precision 5)

renderedString :: forall cs m. String -> { html :: H.ComponentHTML Action cs m, width :: Int }
renderedString s = { html: HH.text s, width: String.length s }

renderedStringDiv :: forall cs m. String -> { html :: H.ComponentHTML Action cs m, width :: Int }
renderedStringDiv s = { html: HH.div [ formStyle ] [ HH.text s ], width: 1 + (String.length s) }

alignHorizOrVert :: forall cs m. Array { html :: H.ComponentHTML Action cs m, width :: Int } -> { html :: H.ComponentHTML Action cs m, width :: Int }
alignHorizOrVert elements =
  if widthHoriz < 30 then { html: htmlHoriz, width: widthHoriz }
  else { html: htmlVert, width: widthVert }
  where
  widths = map (\e -> e.width) elements
  n = Array.length widths
  widthHoriz = sum widths + n + 1
  widthVert = (Array.foldl max 0 widths) + 2
  htmls = map (\e -> e.html) elements
  htmlVert = HH.div [ formStyle ] htmls
  htmlHoriz = HH.div [ formStyleHoriz ] htmls

formStyle :: forall r i. HH.IProp (style :: String | r) i
formStyle = HP.style "border-style: dotted; margin: 1px 5px;"

formStyleHoriz :: forall r i. HH.IProp (style :: String | r) i
formStyleHoriz = HP.style "border-style: dotted; margin: 1px 5px; display: flex; align-items: center;"