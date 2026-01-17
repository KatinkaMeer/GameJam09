{-# LANGUAGE RecordWildCards #-}

module View.Frog (
  frogSprite,
  FrogState (..),
) where

import Graphics.Gloss (Picture (Pictures), pictures)

import Model (Assets (Assets, frogBody, frogEyesClosed, frogEyesOpen, frogMouth, player))

data FrogState = FrogState
  { eyesOpen :: Bool,
    mouthOpen :: Bool
  }

frogSprite :: Assets -> FrogState -> Picture
frogSprite Assets {..} state@FrogState {..} =
  pictures
    $ frogBody
      : (if eyesOpen then frogEyesOpen else frogEyesClosed)
      : ([frogMouth | mouthOpen])
