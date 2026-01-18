{-# LANGUAGE LexicalNegation #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)

import Controller
import Model
import Sound hiding (play)
import View

initialWindowPosition :: (Int, Int)
initialWindowPosition = (10, 10)

loadSprites :: IO Assets
loadSprites = do
  player <- loadBMP "./assets/sprite.bmp"
  bubble <- loadBMP "./assets/bubble/bubble.bmp"
  -- placeholder
  bubbleTimerAttention <- loadBMP "./assets/bubble/bubble.bmp"
  -- placeholder
  bubbleTimerDanger <- loadBMP "./assets/bubble/bubble.bmp"
  frogBodyRight <- loadBMP "./assets/frog/layers/body-right.bmp"
  frogBodyLeft <- loadBMP "./assets/frog/layers/body-left.bmp"
  frogEyesOpenRight <- loadBMP "./assets/frog/layers/eyes-open-right.bmp"
  frogEyesOpenLeft <- loadBMP "./assets/frog/layers/eyes-open-left.bmp"
  frogEyesClosedRight <- loadBMP "./assets/frog/layers/eyes-closed-right.bmp"
  frogEyesClosedLeft <- loadBMP "./assets/frog/layers/eyes-closed-left.bmp"
  frogMouthRight <- loadBMP "./assets/frog/layers/mouth-right.bmp"
  frogMouthLeft <- loadBMP "./assets/frog/layers/mouth-left.bmp"
  cloud <- loadBMP "./assets/clouds/white.bmp"
  pure
    Assets
      { player,
        bubble,
        bubbleTimerAttention,
        bubbleTimerDanger,
        frogBodyRight,
        frogBodyLeft,
        frogEyesOpenRight,
        frogEyesOpenLeft,
        frogEyesClosedRight,
        frogEyesClosedLeft,
        frogMouthRight,
        frogMouthLeft,
        cloud
      }

main :: IO ()
main =
  withProgNameAndArgs runALUT $ \_ _ -> do
    playStartSound
    assets <- loadSprites
    playIO
      (InWindow "GlossyGaming" (500, 500) (10, 10))
      ( makeColor -- himmelblau #FF007CB0
          (0x00 / 255)
          (0x7C / 255)
          (0xB0 / 255)
          (0xFF / 255)
      )
      60
      (initialGlobalState assets)
      (pure . render)
      handleInput
      update
