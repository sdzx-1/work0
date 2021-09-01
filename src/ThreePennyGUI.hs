{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ThreePennyGUI where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad (void)
import Data.IORef
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

t :: Has (Lift UI) sig m => Window -> m ()
t w = do
  sendM $ do
    return w # set title "Mouse"

    out <- UI.span # set text "Coordinates: "
    wrap <-
      UI.div #. "wrap"
        # set style [("width", "300px"), ("height", "300px"), ("border", "solid black 1px")]
        # set (attr "tabindex") "1" -- allow key presses
        #+ [element out]
    getBody w #+ [element wrap]

    on UI.mousemove wrap $ \xy ->
      element out # set text ("Coordinates: " ++ show xy)

    on UI.keydown wrap $ \c ->
      element out # set text ("Keycode: " ++ show c)

et = startGUI defaultConfig (runM . t)

tmain :: IO ()
tmain = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
  return w # set title "Mouse"
  mospos <- liftIO $ newIORef (0, 0)
  isMove <- liftIO $ newIORef False

  body <- getBody w

  out <- UI.span # set text "Coordinates: "
  wrap <-
    UI.div #. "wrap"
      # set
        style
        [ ("position", "relative"),
          ("width", "300px"),
          ("height", "300px"),
          ("border", "solid black 1px"),
          ("top", "100px"),
          ("left", "100px")
        ]
      # set (attr "tabindex") "1" -- allow key presses
      #+ [element out]
  return body #+ [element wrap]

  on UI.mousedown wrap $ \(x, y) -> liftIO $ writeIORef isMove True
  on UI.mouseup wrap $ \(x, y) -> liftIO $ writeIORef isMove False
  on UI.mousemove wrap $ \c -> do
    element out # set text ("Keycode: " ++ show c)
    (x, y) <- liftIO $ readIORef mospos
    liftIO (readIORef isMove) >>= \case
      False -> return ()
      True ->
        void $
          element wrap
            # set
              style
              [ ("top", show (floor $ y - 150) ++ "px"),
                ("left", show (floor $ x - 150) ++ "px")
              ]

  on UI.mousemove body $ \xy@(x, y) -> do
    liftIO $ writeIORef mospos (x, y)

  on UI.mousedown body $ \xy@(x, y) -> do
    liftIO $ print "wellcome"
    create body mospos (x, y)

create ::
  Element ->
  IORef (Double, Double) ->
  (Double, Double) ->
  UI ()
create body mosPosRef (sx, sy) = do
  isMove <- liftIO $ newIORef False
  wrap <-
    UI.div #. "wrap"
      # set
        style
        [ ("position", "relative"),
          ("width", "300px"),
          ("height", "300px"),
          ("border", "solid black 1px"),
          ("top", show (floor sy) ++ "px"),
          ("left", show (floor sx) ++ "px")
        ]
      # set (attr "tabindex") "1" -- allow key presses
  return body #+ [element wrap]

  on UI.mousedown wrap $ \(x, y) -> liftIO $ writeIORef isMove True
  on UI.mouseup wrap $ \(x, y) -> liftIO $ writeIORef isMove False
  on UI.mousemove wrap $ \c -> do
    (x, y) <- liftIO $ readIORef mosPosRef
    liftIO (readIORef isMove) >>= \case
      False -> return ()
      True ->
        void $
          element wrap
            # set
              style
              [ ("top", show (floor $ y - 150) ++ "px"),
                ("left", show (floor $ x - 150) ++ "px")
              ]
