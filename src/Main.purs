module Main where

import Prelude

import App.ListSteps as ListSteps
import App.PlotTest as PlotTest
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI PlotTest.component unit body
