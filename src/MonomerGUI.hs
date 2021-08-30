{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MonomerGUI where

import Data.Text (Text, pack)
import Monomer
import Monomer.Core.WidgetTypes (WidgetEnv)
import Optics

newtype AppModel = AppModel
  { _clickCount :: Int
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Hello world",
          spacer,
          hstack
            [ label $
                "Click count: "
                  <> pack
                    (show $ model ^. clickCount),
              spacer,
              button "Increase count" AppIncrease
            ]
        ]
        `styleBasic` [padding 10]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model $ model & clickCount %~ (+ 1)]

tmain :: IO ()
tmain = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "test 01",
        appTheme darkTheme,
        appFontDef "Regular" "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf",
        appInitEvent AppInit
      ]
    model = AppModel 0