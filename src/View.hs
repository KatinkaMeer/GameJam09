{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe
import Graphics.Gloss (
  Picture (Pictures),
  Vector,
  black,
  blank,
  circleSolid,
  color,
  line,
  pictures,
  rectangleSolid,
  scale,
  text,
  translate,
 )
import Graphics.Gloss.Data.ViewPort (ViewPort, applyViewPortToPicture)

import Data.Map qualified as M
import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (*),
  (-),
 )

import Model (
  Assets (..),
  CharacterStatus (..),
  GlobalState (..),
  Jump (..),
  Object (Object, position),
  ObjectType (..),
  Screen (..),
  UiState (UiState, assets),
  World (..),
  characterInBubble,
  objectDataToPicture,
 )
import Sound (pause)
import View.Frog (
  FrogState (FrogState, eyesOpen, mouthOpen),
  frogSprite,
 )

scalarProduct :: Vector -> Vector -> Float
scalarProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

getNormVector :: Vector -> Vector
getNormVector v = (1 / sqrt (scalarProduct v v)) P.* v

betweenValues :: Float -> Float -> Float -> Float
betweenValues lowlim value uplim = max lowlim (min value uplim)

resizeVectorFactor :: Float -> Float -> Vector -> Float
resizeVectorFactor lowlim uplim v = betweenValues lowlim (sqrt (scalarProduct v v)) uplim

render :: GlobalState -> Picture
render GlobalState {..} = case screen of
  StartScreen ->
    pictures
      $ map
        (uncurry (translate 0 . (* 100)) . second (scale 0.2 0.2))
        [ (4, text "Some fancy game name"),
          (1, text "Press Space to start a game"),
          (-2, text "Press H to view high scores"),
          (-4, text "Press ESC to quit the game")
        ]
  GameScreen world -> renderWorld (assets uiState) world
  HighScoreScreen -> blank

renderWorld :: Assets -> World -> Picture
renderWorld
  assets
  World
    { character = Object {position = (x, y)},
      ..
    } =
    applyViewPortToPicture viewport
      $ pictures
      $ case jump of
        -- TODO add vectorLength variable infront that depends on strength
        Just (InitJump m) -> line [(x, y), resizeVectorFactor 60 300 (m P.- mousePosition) P.* getNormVector (m P.- mousePosition)]
        Nothing -> blank
        : translate
          x
          y
          ( pictures
              ( ( case characterStatus of
                    CharacterAtBalloon _ -> [circleSolid 30] -- placeholder
                    CharacterInBubble _ -> [characterBubble assets]
                    PlainCharacter -> []
                )
                  ++ [frogSprite assets FrogState {eyesOpen = True, mouthOpen = False}]
              )
          )
        : translate 500 500 (cloud assets)
        : map renderObject (M.elems objects)
    where
      characterBubble = case characterStatus of
        CharacterInBubble t
          | t < 3 -> bubbleTimerDanger
          | t < 7 -> bubbleTimerAttention
        _ -> bubble

      renderObject (t, Object {position}) =
        uncurry
          translate
          position
          ( case t of
              Bubble -> bubble assets
              Balloon -> circleSolid 30
          )
