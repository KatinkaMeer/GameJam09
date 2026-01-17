{-# LANGUAGE RecordWildCards #-}

module View (render) where

import GlossyRuler (drawRuler)

import Graphics.Gloss
  ( Picture,
    black,
    circleSolid,
    color,
    pictures,
    rectangleSolid,
    translate,
    white, yellow, red
  )
import Model (Assets (Assets, player), Object (Object, position), World (World, assets, character, windowSize), initialWorld)

render :: World -> Picture
render World {character = Object {position = (x, y)}, assets = Assets {player = playerSprite, ..}, ..} =
  pictures $
    -- UI
    let
      windowWidth = fromIntegral $ fst windowSize
      windowHeight = fromIntegral $ snd windowSize
      rightBorderPosition =  (windowWidth / 2, 0)
      rulerDimensions = (100, windowHeight) 
      rulerDimensionsX = fst rulerDimensions
      rulerDimensionsY = snd rulerDimensions
      rulerPosition = rightBorderPosition
      rulerNumberOfTickMarks = 10
      rulerIndicatedMeasurement = 0
      in
        drawRuler rulerPosition rulerDimensions rulerNumberOfTickMarks rulerIndicatedMeasurement white yellow red
      :
    -- player sprite
    playerSprite
      :
      -- other stuff in the scene
      map
        (translate (-x) (-y))
        [ translate 80 40 $ circleSolid 30,
          translate (-250) 0 $ rectangleSolid 100 1000
        ]

