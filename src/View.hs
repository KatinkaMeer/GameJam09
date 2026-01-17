{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Graphics.Gloss (
  Picture (Pictures),
  black,
  circleSolid,
  color,
  pictures,
  rectangleSolid,
  translate,
 )

import Model (
  Assets (Assets, player),
  Object (Object, position),
  World (World, assets, character),
 )
import View.Frog (
  FrogState(FrogState, eyesOpen, mouthOpen),
  frogSprite
 )

render :: World -> Picture
render
  World
    { character = Object {position = (x, y)},
      assets = assets@Assets {player = playerSprite, ..},
      ..
    } =
    pictures
      $
      -- player sprite
      playerSprite
    : frogSprite assets FrogState{ eyesOpen = True, mouthOpen = False }
        :
        -- other stuff in the scene
        map
          (translate (-x) (-y))
          [ translate 80 40 $ circleSolid 30,
            translate (-250) 0 $ rectangleSolid 100 1000
          ]
