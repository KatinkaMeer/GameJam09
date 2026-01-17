{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Graphics.Gloss (
  Picture (Pictures),
  black,
  blank,
  circleSolid,
  color,
  pictures,
  rectangleSolid,
  translate,
 )

import Model (
  Assets (..),
  CharacterStatus (..),
  GlobalState (..),
  Object (Object, position),
  Screen (..),
  UiState (UiState, assets),
  World (..),
  objectDataToPicture,
 )
import View.Frog (
  FrogState (FrogState, eyesOpen, mouthOpen),
  frogSprite,
 )

render :: GlobalState -> Picture
render GlobalState {..} = case screen of
  StartScreen -> blank
  GameScreen world -> renderWorld (assets uiState) world
  HighScoreScreen -> blank

renderWorld :: Assets -> World -> Picture
renderWorld
  assets
  World
    { character = Object {position = (x, y)},
      ..
    } =
    pictures $
      -- player sprite
      characterBubble assets
        : frogSprite assets FrogState {eyesOpen = True, mouthOpen = False}
        :
        -- other stuff in the scene
        map
          (translate (-x) (-y))
          ( translate (-250) 0 (rectangleSolid 100 1000)
              : map (objectDataToPicture assets) objects
          )
    where
      characterBubble = case characterStatus of
        CharacterInBubble t
          | t < 3 -> bubbleTimerDanger
          | t < 7 -> bubbleTimerAttention
        _ -> const blank
