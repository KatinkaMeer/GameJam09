{-# LANGUAGE RecordWildCards #-}

module View.Frog (
  frogSprite,
  FrogState (..),
)
where

import Graphics.Gloss (Picture (Pictures), pictures)

import Model (
  Assets (
    Assets,
    frogBodyLeft,
    frogBodyRight,
    frogEyesClosedLeft,
    frogEyesClosedRight,
    frogEyesOpenLeft,
    frogEyesOpenRight,
    frogMouthLeft,
    frogMouthRight,
    player
  ),
 )

data FrogState = FrogState
  { eyesOpen :: Bool,
    mouthOpen :: Bool,
    directionRight :: Bool
  }

frogSprite :: Assets -> FrogState -> Picture
frogSprite assets state@FrogState {..} =
  let
    body = frogBody directionRight assets
    eyes =
      if eyesOpen
        then frogOpenEyes directionRight assets
        else frogClosedEyes directionRight assets
    mouth = frogMouth directionRight assets
  in
    pictures
      $ body
        : eyes
        : ([mouth | mouthOpen])

frogBody :: Bool -> Assets -> Picture
frogBody right Assets {..} =
  if right then frogBodyRight else frogBodyLeft

frogOpenEyes :: Bool -> Assets -> Picture
frogOpenEyes right Assets {..} =
  if right then frogEyesOpenRight else frogEyesOpenLeft

frogClosedEyes :: Bool -> Assets -> Picture
frogClosedEyes right Assets {..} =
  if right then frogEyesClosedRight else frogEyesClosedLeft

frogMouth :: Bool -> Assets -> Picture
frogMouth right Assets {..} =
  if right then frogMouthRight else frogMouthLeft
