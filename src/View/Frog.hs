{-# LANGUAGE RecordWildCards #-}

module View.Frog (
  frogSprite,
  FrogState (..)
) where

import Model (Assets (Assets, player, frogBody, frogEyesClosed, frogEyesOpen, frogMouth))
import Graphics.Gloss (Picture (Pictures), pictures)

data FrogState = FrogState
  { eyesOpen :: Bool,
    mouthOpen :: Bool
  }

frogSprite :: Assets -> FrogState -> Picture
frogSprite Assets { .. } state@FrogState{..} =
  pictures
  $
    frogBody
    : (if eyesOpen then frogEyesOpen else frogEyesClosed)
    : ([frogMouth | mouthOpen])
