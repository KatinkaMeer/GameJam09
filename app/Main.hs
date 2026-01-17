{-# LANGUAGE LexicalNegation #-}

module Main where

import Graphics.Gloss

import Controller
import Model
import View

loadSprites :: IO Assets
loadSprites = do
  player <- loadBMP "./assets/sprite.bmp"
  bubble <- loadBMP "./assets/bubble/bubble.bmp"
  frogBody <- loadBMP "./assets/frog/layers/body.bmp"
  frogEyesOpen <- loadBMP "./assets/frog/layers/eyes-open.bmp"
  frogEyesClosed <- loadBMP "./assets/frog/layers/eyes-closed.bmp"
  frogMouth <- loadBMP "./assets/frog/layers/mouth.bmp"
  cloud <- loadBMP "./assets/clouds/white.bmp"
  pure
    Assets
      { player,
        bubble,
        frogBody,
        frogEyesOpen,
        frogEyesClosed,
        frogMouth,
        cloud
      }

main :: IO ()
main =
  do
    assets <- loadSprites
    play
      (InWindow "GlossyGaming" (500, 500) (10, 10))
      green
      60
      (initialWorld assets)
      render
      handleInput
      update
