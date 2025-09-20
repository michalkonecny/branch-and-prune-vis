module App.UIUtils where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

renderWithPopup
  :: forall a cs m
   . { popupContents :: Array (H.ComponentHTML a cs m)
     , popupTargetElement :: H.ComponentHTML a cs m
     , onClose :: Event -> a
     }
  -> H.ComponentHTML a cs m
renderWithPopup { popupContents, popupTargetElement, onClose } =
  HH.div [ HP.class_ (H.ClassName "popup") ]
    [ popupTargetElement
    , HH.span [ HP.class_ (H.ClassName "popupcontents show") ]
        (popupContents <> [ closeIcon ])
    ]
  where
  closeIcon = HH.div
    [ HE.onClick (\e -> onClose (toEvent e)), HP.class_ (H.ClassName "popupcloseicon") ]
    [ HH.text "ⓧ" ]

