{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Graphics.UI.Gtk as Gtk
import Data.IORef

import Diagrams.Prelude
import Diagrams.Backend.Gtk

import Control.Monad.IO.Class (liftIO)

import Types
import Graphics
import Game

createMainWindow :: IORef GameState -> IO Window
createMainWindow gameStateRef = do
  let winW = 640
      winH = 640

  win <- windowNew

  Gtk.set win [windowDefaultWidth := winW, windowDefaultHeight := winH]

  _ <- onSizeRequest win $ return (Requisition winW winH)
  _ <- onDestroy win mainQuit
  drawArea <- drawingAreaNew
  
  _ <- drawArea `onExpose` \_dirtyRect -> do
    gameState <- readIORef gameStateRef
    windowSetTitle win (gameScore gameState)
    (canvasX,canvasY) <- widgetGetSize drawArea
    let dia = gameDiagram gameState
        spec = dims $ V2 (fromIntegral canvasX) (fromIntegral canvasY)
        scaledDia = toGtkCoords $ transform (requiredScaling spec (size dia)) dia
    drawWindow <- widgetGetDrawWindow drawArea
    renderToGtk drawWindow scaledDia
    return True

  _ <- drawArea `on` buttonPressEvent $ tryEvent $ do
    (x, y) <- eventCoordinates
    let mousePos = floor <$> V2 (x / winW) ((winH - y) / winH) * 8
    liftIO $ do
      updateGameState gameStateRef mousePos 
      widgetQueueDraw drawArea

  containerAdd win drawArea
  return win

main :: IO ()
main = do
  _ <- initGUI
  gameState <- newIORef initialGameState
  win <- createMainWindow gameState
  widgetShowAll win
  mainGUI
