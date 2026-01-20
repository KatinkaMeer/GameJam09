{-# LANGUAGE LexicalNegation #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)

import Controller
import Model
import Paths_GameJam09 (getDataFileName)
import Sound hiding (play)
import View

initialWindowPosition :: (Int, Int)
initialWindowPosition = (10, 10)

loadSprites :: IO Assets
loadSprites = do
  player <- loadBMP =<< getDataFileName "assets/sprite.bmp"
  bubble <- loadBMP =<< getDataFileName "assets/bubble/bubble.bmp"
  -- placeholder
  bubbleTimerAttention <- loadBMP =<< getDataFileName "assets/bubble/bubble.bmp"
  -- placeholder
  bubbleTimerDanger <- loadBMP =<< getDataFileName "assets/bubble/bubble.bmp"
  frogBodyRight <- loadBMP =<< getDataFileName "assets/frog/layers/body-right.bmp"
  frogBodyLeft <- loadBMP =<< getDataFileName "assets/frog/layers/body-left.bmp"
  frogEyesOpenRight <- loadBMP =<< getDataFileName "assets/frog/layers/eyes-open-right.bmp"
  frogEyesOpenLeft <- loadBMP =<< getDataFileName "assets/frog/layers/eyes-open-left.bmp"
  frogEyesClosedRight <- loadBMP =<< getDataFileName "assets/frog/layers/eyes-closed-right.bmp"
  frogEyesClosedLeft <- loadBMP =<< getDataFileName "assets/frog/layers/eyes-closed-left.bmp"
  frogMouthRight <- loadBMP =<< getDataFileName "assets/frog/layers/mouth-right.bmp"
  frogMouthLeft <- loadBMP =<< getDataFileName "assets/frog/layers/mouth-left.bmp"
  cloud <- loadBMP =<< getDataFileName "assets/clouds/white.bmp"
  ballonBlue <- loadBMP =<< getDataFileName "assets/ballony/blue.bmp"
  ballonGreen <- loadBMP =<< getDataFileName "assets/ballony/green.bmp"
  ballonPink <- loadBMP =<< getDataFileName "assets/ballony/pink.bmp"
  ballonRed <- loadBMP =<< getDataFileName "assets/ballony/red.bmp"
  ballonYellow <- loadBMP =<< getDataFileName "assets/ballony/yellow.bmp"
  ground <- loadBMP =<< getDataFileName "assets/ballony/yellow.bmp"
  titleScreen <- loadBMP =<< getDataFileName "assets/title_screen/title_screen.bmp"
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
        cloud,
        ballonBlue,
        ballonGreen,
        ballonPink,
        ballonRed,
        ballonYellow,
        ground,
        titleScreen
      }

main :: IO ()
main =
  withProgNameAndArgs runALUT $ \_ _ -> do
    playStartSound
    assets <- loadSprites
    playIO
      (InWindow "Bubble Up!" (1920, 1080) (10, 10))
      ( makeColor -- himmelblau #FF007CB0
          (0x00 / 255)
          (0x7C / 255)
          (0xB0 / 255)
          (0xFF / 255)
      )
      60
      (initialGlobalState assets)
      render
      handleInput
      update
