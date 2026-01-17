{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Graphics.Gloss (
  Picture,
  black,
  circleSolid,
  color,
  pictures,
  rectangleSolid,
  translate,
 )

import Model (Object (Object, position), World (World, character, assets), Assets (Assets, player))

render :: World -> Picture
render World {character = Object {position = (x, y)}, assets = Assets {player = playerSprite, ..},..} =
  pictures
    $
    -- player sprite
    playerSprite
      :
      -- other stuff in the scene
      map
        (translate (-x) (-y))
        [ translate 80 40 $ circleSolid 30,
          translate (-250) 0 $ rectangleSolid 100 1000
        ]
