{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Data.Map qualified as M
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
  characterInBubble,
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
      ( case characterStatus of
          CharacterInBalloon _ -> [circleSolid 30] -- placeholder
          CharacterInBubble _ -> [characterBubble assets]
          PlainCharacter -> []
      )
        ++ frogSprite assets FrogState {eyesOpen = True, mouthOpen = False}
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
        _ -> const bubble assets
