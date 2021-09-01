{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module MonomerGUI where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (forM_)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (cast)
import Debug.Trace
import Monomer
import Monomer.Common.BasicTypes
import qualified Monomer.Lens as L
import Monomer.Widgets.Single

data ICCanvas
  = ICCricle
      { _icPosition :: Point,
        _icRadius :: Double
      }
  | ICArrow
      { _icStartPoint :: Point,
        _icEndPoint :: Point
      }
  deriving (Eq, Show)

makeLenses ''ICCanvas

inICCanvas :: Point -> ICCanvas -> Bool
inICCanvas p ICCricle {..} =
  (p ^. L.x - _icPosition ^. L.x) ^ 2
    + (p ^. L.y - _icPosition ^. L.y) ^ 2 < _icRadius ^ 2
inICCanvas p ICArrow {..} =
  let x0 = p ^. L.x
      x1 = _icStartPoint ^. L.x
      x2 = _icEndPoint ^. L.x
      y0 = p ^. L.y
      y1 = _icStartPoint ^. L.y
      y2 = _icEndPoint ^. L.y
   in ((x0 - x1) * (y1 - y2) - (x1 - x2) * (y0 - y1)) ^ 2 / ((x1 - x2) ^ 2 + (y1 - y2) ^ 2) < 100 -- (power distancs)

-- l =|(x0-x1)(y1-y2)-(x1-x2)(y0-y1)|/√[(x1-x2)^2+(y1-y2)^2]
-- distance p0 to  line (p1,p2), this less 10

data Stage
  = InteractiveCanvas -- create node
  | WriteHandlerFunction -- write handler function
  deriving (Eq, Show)

data CanvasState = CanvasState
  { _icStage :: Stage,
    _icCreatedNode :: [ICCanvas],
    _icSelected :: [ICCanvas],
    _icMarked :: [ICCanvas],
    _icText :: Text
  }
  deriving (Eq, Show)

makeLenses 'CanvasState

newtype CanvasCfg = CanvasCfg
  { _canvasColors :: [Color]
  }
  deriving (Eq, Show)

instance Default CanvasCfg where
  def =
    CanvasCfg
      { _canvasColors = []
      }

instance Semigroup CanvasCfg where
  (<>) c1 c2 =
    CanvasCfg
      { _canvasColors = _canvasColors c1 <> _canvasColors c2
      }

instance Monoid CanvasCfg where
  mempty = def

data CanvasMessage
  = ResetCanvas
  deriving (Eq, Show)

makeLenses 'CanvasCfg

canvasColor :: Color -> CanvasCfg
canvasColor col = def & canvasColors .~ [col]

canvas :: WidgetNode s e
canvas = canvas_ def

canvas_ :: [CanvasCfg] -> WidgetNode s e
canvas_ configs = defaultWidgetNode "canvas" newWidget
  where
    config = mconcat configs
    state =
      CanvasState
        { _icStage = InteractiveCanvas,
          _icCreatedNode = [],
          _icSelected = [],
          _icMarked = [],
          _icText = ""
        }
    newWidget = makeCanvas config state

globalRidaus :: Double
globalRidaus = 30

makeCanvas :: CanvasCfg -> CanvasState -> Widget s e
makeCanvas cfg state = widget
  where
    widget =
      createSingle
        state
        def
          { singleMerge = merge,
            singleHandleEvent = handleEvent,
            singleHandleMessage = handleMessage,
            singleGetSizeReq = getSizeReq,
            singleRender = render
          }

    colors
      | null (cfg ^. canvasColors) = [orange, green, steelBlue, deepPink]
      | otherwise = cfg ^. canvasColors
    nextColor idx = colors !! (idx `mod` length colors)

    merge wenv node oldNode oldState = result
      where
        newNode =
          node
            & L.widget .~ makeCanvas cfg oldState
        result = resultNode newNode

    handleEvent wenv node target evt =
      case evt of
        Click point BtnLeft _ -> case state ^. icSelected of
          [] -> Just result
          [ic@ICCricle {}] -> Just $ resultNode $ node & L.widget .~ makeCanvas cfg (state & icMarked %~ (ic :) & icSelected .~ [])
          _ -> Nothing
          where
            newPoint = subPoint point origin
            newState = state & icCreatedNode %~ (ICCricle newPoint globalRidaus :)
            newNode =
              node
                & L.widget .~ makeCanvas cfg newState
            result = resultNode newNode
        -- Click _ BtnMiddle _ -> case trace (show 111111111) state ^. icSelected of
        --   [ic1@ICCricle {}, ic2@ICCricle {}] ->
        --     Just $
        --       resultNode $
        --         node & L.widget
        --           .~ makeCanvas
        --             cfg
        --             ( state & icCreatedNode
        --                 %~ (ICArrow (_icPosition (trace (show ic1) ic1)) (_icPosition ic2) :)
        --                   & icSelected .~ []
        --             )
        --   _ -> Nothing
        Move p ->
          let res = filter (inICCanvas (subPoint p origin)) (state ^. icCreatedNode)
           in Just $ resultReqs (node & L.widget .~ makeCanvas cfg (state & icSelected .~ res)) [RenderOnce]
        _ -> Nothing
      where
        vp = node ^. L.info . L.viewport
        origin = Point (vp ^. L.x) (vp ^. L.y)

    handleMessage wenv node target msg = case cast msg of
      Just ResetCanvas -> Just result
        where
          newState = state & icCreatedNode .~ [] & icMarked .~ []
          newNode =
            node
              & L.widget .~ makeCanvas cfg newState
          result = resultNode newNode
      _ -> Nothing

    getSizeReq wenv node = (sizeReqW, sizeReqH)
      where
        sizeReqW = minWidth 100
        sizeReqH = minHeight 100

    render wenv node renderer = do
      drawInTranslation renderer origin $ do
        forM_ tuples $ \(idx, icc) -> do
          setStrokeColor renderer (nextColor idx)
          setStrokeWidth renderer 2
          beginPath renderer
          -- renderLine renderer pointA pointB
          -- renderRect renderer (Rect (_pX pointA) (_pY pointA) 40 30)
          -- renderText renderer (icc ^. icPosition) def (FontSize 10) def (T.pack $ show $ icc ^. icPosition)
          case icc of
            ICCricle {..} -> renderArc renderer _icPosition globalRidaus 0 360 CW
            ICArrow {..} -> renderLine renderer _icStartPoint _icEndPoint
          stroke renderer

        forM_ (state ^. icSelected) $ \icc -> do
          setStrokeColor renderer yellow
          setStrokeWidth renderer 3
          beginPath renderer
          -- renderLine renderer pointA pointB
          -- renderRect renderer (Rect (_pX pointA) (_pY pointA) 40 30)
          -- renderText renderer (icc ^. icPosition) def (FontSize 10) def (T.pack $ show $ icc ^. icPosition)
          case icc of
            ICCricle {..} -> renderArc renderer _icPosition globalRidaus 0 360 CW
            ICArrow {..} -> renderLine renderer _icStartPoint _icEndPoint
          stroke renderer

        forM_ (state ^. icMarked) $ \icc -> do
          setStrokeColor renderer blue
          setStrokeWidth renderer 3
          beginPath renderer
          -- renderLine renderer pointA pointB
          -- renderRect renderer (Rect (_pX pointA) (_pY pointA) 40 30)
          -- renderText renderer (icc ^. icPosition) def (FontSize 10) def (T.pack $ show $ icc ^. icPosition)
          case icc of
            ICCricle {..} -> renderArc renderer _icPosition globalRidaus 0 360 CW
            ICArrow {..} -> renderLine renderer _icStartPoint _icEndPoint
          stroke renderer
      where
        vp = node ^. L.info . L.viewport
        mousePos = wenv ^. L.inputStatus . L.mousePos
        newPoint = subPoint mousePos origin
        origin = Point (vp ^. L.x) (vp ^. L.y)
        clicked = state ^. icCreatedNode -- [] -- state ^. clickedPoints
        tuples = zip [0 ..] $ reverse clicked

data AppModel
  = AppModel
  deriving (Eq, Show)

data AppEvent
  = AppResetCanvas
  | AppInit
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ button "Reset canvas" AppResetCanvas,
          spacer,
          draggable () (label "item")
          -- canvas `nodeKey` "mainCanvas" `styleBasic` [border 1 gray]
          --      canvas_ [canvasColor pink] `nodeKey` "mainCanvas" `styleBasic` [border 1 gray]
        ]
        `styleBasic` [padding 10]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [setFocusOnKey wenv "mainCanvas"]
  AppResetCanvas -> [Message "mainCanvas" ResetCanvas]

main07 :: IO ()
main07 = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "canvas test",
        appTheme darkTheme,
        appFontDef "Regular" "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf",
        appInitEvent AppInit
      ]
    model = AppModel