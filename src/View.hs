{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Data.Maybe
import Graphics.Gloss (
  Picture (Pictures),
  black,
  blank,
  circleSolid,
  color,
  line,
  pictures,
  rectangleSolid,
  text,
  translate,
 )

import Data.Map qualified as M

import Model (
  Assets (..),
  CharacterStatus (..),
  GlobalState (..),
  Jump (..),
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
  GameScreen world@World {..} ->
    pictures
      [ case jump of
          Nothing -> blank
          Just j -> case j of
            InitJump m -> line $ catMaybes [mousePosition, Just m]
            Jump {} -> blank,
        text (show mousePosition),
        renderWorld (assets uiState) world
      ]
  HighScoreScreen -> blank

renderWorld :: Assets -> World -> Picture
renderWorld
  assets
  World
    { character = Object {position = (x, y)},
      ..
    } =
    pictures
      $
      -- player sprite
      characterBubble assets
        : frogSprite assets FrogState {eyesOpen = True, mouthOpen = False}
        :
        -- other stuff in the scene
        map
          (translate (-x) (-y))
          ( translate (-250) 0 (rectangleSolid 100 1000)
              : M.elems (M.map (objectDataToPicture assets) objects)
          )
    where
      characterBubble = case characterStatus of
        CharacterInBubble t
          | t < 3 -> bubbleTimerDanger
          | t < 7 -> bubbleTimerAttention
        _ -> const blank
