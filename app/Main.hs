{-# LANGUAGE LexicalNegation #-}

module Main where

import Graphics.Gloss

import Controller
import Model
import View
import Sound hiding (play)

loadSprites :: IO Assets
loadSprites = do
  player <- loadBMP "./assets/sprite.bmp"
  pure
    Assets
      { player = player,
        bubble = circleSolid 30
      }

main :: IO ()
main =
  withProgNameAndArgs runALUT $ \progName args -> do
    playBubblesSound
    assets <- loadSprites
    play
      (InWindow "GlossyGaming" (500, 500) (10, 10))
      green
      60
      (initialWorld assets)
      render
      handleInput
      update
