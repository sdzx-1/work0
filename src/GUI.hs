{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module GUI where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
-- import Optics

import Control.Concurrent
import Control.Effect.Optics (use, (%=), (.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Foldable (forM_)
import Data.Kind
import Data.Maybe
import Data.Text (Text, pack)
import Data.Typeable
import Data.Word (Word8)
import GHC.Real (Integral)
import MyLib
import Optics (ix, (%), (^.), _1)
import SDL
import SDL.Font as SF
import SDL.Framerate
import SDL.Primitive
import Widget

data Body = Body

bodyWidget' :: Widget Body
bodyWidget' =
  Widget
    { _width = 100,
      _heigh = 100,
      _model = Body,
      _backgroundColor = 255,
      _frontColor = 90,
      _visible = True,
      _path = [],
      _children =
        [ (10, SomeWidget (modelWidget [0])),
          (110, SomeWidget (modelWidget [1]))
        ]
    }

instance WidgetRender Body where
  renderSelf bp w@Widget {..} = do
    renderer <- asks _renderer
    clear renderer

inRectangle :: (Ord a, Num a, Integral b) => Point V2 a -> Rectangle b -> Bool
inRectangle (P v0@(V2 x0 y0)) (Rectangle (P vs) size) =
  let V2 x1 y1 = fmap fromIntegral vs
      V2 x2 y2 = fmap fromIntegral (vs + size)
   in x0 > x1 && x0 < x2 && y0 > y1 && y0 < y2

-- v0 > fmap fromIntegral vs && v0 < fmap fromIntegral (vs + size)

instance WidgetHandler Body where
  handler es a = mapM_ handler1 es >> return a
    where
      handler1 e = do
        case eventPayload e of
          -- (MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ pos)) -> do
          --   cs <- use $ bodyWidget % children'
          --   let newmw = modelWidget [length cs]
          --   bodyWidget % children' %= ((fmap fromIntegral pos, SomeWidget newmw) :)
          -- drag model
          (MouseMotionEvent (MouseMotionEventData _ _ [ButtonLeft] pos relPos)) -> do
            ls <- use (bodyWidget % children')
            let vs = map (\(bp, sw) -> (Rectangle bp (V2 (sw ^. width') (sw ^. height')), sw ^. path')) ls
                posIn = listToMaybe $ map snd $ filter (inRectangle pos . Prelude.fst) vs
            case posIn of
              Just [i] -> do
                bodyWidget % children' % ix i % _1 %= (\(P v) -> P $ v + fmap fromIntegral relPos)
              _ -> return ()
          UserEvent _ -> do
            ge <- asks _getUserEvent
            ue <- liftIO $ ge e
            case ue >>= fromDynamic @OEvent of
              Nothing -> return ()
              Just d -> do
                liftIO $ print d
          _ -> return ()

makeUIState :: UIState
makeUIState =
  UIState
    { _bodyWidget = SomeWidget bodyWidget',
      _focus = []
    }

newtype Model = Model Int deriving (Show)

mkp :: Int -> Int -> BasePositon
mkp x y = P $ V2 x y

modelWidget :: [Int] -> Widget Model
modelWidget path =
  Widget
    { _width = 100,
      _heigh = 100,
      _model = Model 1,
      _backgroundColor = 30,
      _frontColor = V4 0 0 0 255,
      _visible = True,
      _path = path,
      _children =
        [ (mkp 15 25, SomeWidget $ textWidget [1, 2]),
          (mkp 15 50, SomeWidget $ textWidget [1, 2])
        ]
    }

instance WidgetRender Model where
  renderSelf bp w@Widget {..} = do
    renderer <- asks _renderer
    font <- asks _font
    liftIO $ do
      renderFont font renderer (pack $ show _model ++ show _path) (fmap fromIntegral bp) _frontColor

instance WidgetHandler Model where
  handler e a = do
    return a

textWidget :: [Int] -> Widget Text
textWidget path =
  Widget
    { _width = 80,
      _heigh = 30,
      _model = "welcome ",
      _backgroundColor = 30,
      _frontColor = V4 255 0 0 255,
      _visible = True,
      _path = path,
      _children = []
    }

instance WidgetRender Text where
  renderSelf bp w@Widget {..} = do
    renderer <- asks _renderer
    font <- asks _font
    liftIO $ do
      renderFont font renderer _model (fmap fromIntegral bp) _frontColor

instance WidgetHandler Text where
  handler e a = return a

data OEvent = OEvent deriving (Show, Typeable)

initGUI :: IO (Renderer, Font, Manager, Event -> IO (Maybe Dynamic))
initGUI = do
  initializeAll
  SF.initialize
  let toTimerEvent _ _ = return . Just $ OEvent
      fromTimerEvent = const $ return emptyRegisteredEvent
  registerEvent <- registerEvent toTimerEvent fromTimerEvent
  ge <- case registerEvent of
    Nothing -> error "regieter event error"
    Just (RegisteredEventType pe ge) -> do
      let go = do
            pe OEvent
            threadDelay (10 ^ 6)
            go
      void $ forkIO go
      return ge
  window <-
    createWindow
      "resize"
      WindowConfig
        { windowBorder = True,
          windowHighDPI = False,
          windowInputGrabbed = False,
          windowMode = Windowed,
          windowGraphicsContext = NoGraphicsContext,
          windowPosition = Wherever,
          windowResizable = True,
          windowInitialSize = V2 800 600,
          windowVisible = True
        }
  renderer <- createRenderer window (-1) defaultRenderer
  addEventWatch $ \ev ->
    case eventPayload ev of
      WindowSizeChangedEvent sizeChangeData ->
        putStrLn $ "eventWatch windowSizeChanged: " ++ show sizeChangeData
      _ -> return ()
  fm <- SDL.Framerate.manager
  SDL.Framerate.set fm 30
  font <- load "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf" 20
  return (renderer, font, fm, fmap (fmap toDyn) . ge)

appLoop1 :: forall sig m. (UI sig m, MonadIO m) => m ()
appLoop1 = go
  where
    go = do
      e <- liftIO pollEvents
      SomeWidget bodyW <- gets _bodyWidget
      handler e Body

      -- TODO: dispatch event to focus widget

      use bodyWidget >>= renderSomeWidget 0

      renderer <- asks _renderer
      present renderer
      man <- asks _manager
      delay_ man
      go

main :: IO ()
main = do
  (r, f, m, ge) <- initGUI
  runReader (UIEnv r f m ge) $ runState makeUIState appLoop1
  return ()