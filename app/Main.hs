{-# LANGUAGE LexicalNegation #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)

import Controller
import Model
import Sound hiding (play)
import View

loadSprites :: IO Assets
loadSprites = do
  player <- loadBMP "./assets/sprite.bmp"
  bubble <- loadBMP "./assets/bubble/bubble.bmp"
  -- placeholder
  bubbleTimerAttention <- loadBMP "./assets/bubble/bubble.bmp"
  -- placeholder
  bubbleTimerDanger <- loadBMP "./assets/bubble/bubble.bmp"
  frogBody <- loadBMP "./assets/frog/layers/body.bmp"
  frogEyesOpen <- loadBMP "./assets/frog/layers/eyes-open.bmp"
  frogEyesClosed <- loadBMP "./assets/frog/layers/eyes-closed.bmp"
  frogMouth <- loadBMP "./assets/frog/layers/mouth.bmp"
  cloud <- loadBMP "./assets/clouds/white.bmp"
  pure
    Assets
      { player,
        bubble,
        bubbleTimerAttention,
        bubbleTimerDanger,
        frogBody,
        frogEyesOpen,
        frogEyesClosed,
        frogMouth,
        cloud
      }

main :: IO ()
main =
  withProgNameAndArgs runALUT $ \_ _ -> do
    playStartSound
    assets <- loadSprites
    playIO
      (InWindow "GlossyGaming" (500, 500) (10, 10))
      green
      60
      (initialGlobalState assets)
      (pure . render)
      handleInput
      update
