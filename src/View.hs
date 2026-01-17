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
  Vector,
 )

import Data.Map qualified as M

import Graphics.Gloss.Data.Point.Arithmetic qualified as P(
  (-),
  (*),
 )

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

scalarProduct :: Vector-> Vector -> Float
scalarProduct (x1,y1) (x2,y2) = x1*x2 + y1*y2

getNormVector :: Vector -> Vector
getNormVector v =  (1 / (sqrt $ scalarProduct v v)) P.* v

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
    pictures
      $ case jump of
        --TODO add vectorLength variable infront that depends on strength
        Just (InitJump m) -> line [200 P.* getNormVector (m P.- mousePosition), (0,0)]
        Nothing -> blank
        :
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
