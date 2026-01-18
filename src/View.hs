{-# LANGUAGE RecordWildCards #-}

module View (render) where

import Data.Bifunctor (Bifunctor (second))
import Data.Fixed
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
  red,
  scale,
  text,
  translate,
  white,
  yellow,
 )
import Graphics.Gloss.Data.ViewPort (ViewPort (ViewPort, viewPortScale), applyViewPortToPicture, viewPortTranslate)

import Data.Map qualified as M
import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (*),
  (+),
  (-),
 )

import GlossyRuler (drawRuler)
import HighScore
import Math
import Model (
  Assets (..),
  CharacterStatus (..),
  GlobalState (..),
  Jump (..),
  Object (Object, position, velocity),
  ObjectType (..),
  Screen (..),
  UiState (UiState, assets, windowSize),
  World (..),
  characterInBubble,
 )
import Sound (pause)
import View.Frog (
  FrogState (FrogState, directionRight, eyesOpen, mouthOpen),
  frogSprite,
 )

render :: GlobalState -> IO Picture
render GlobalState {..} = do
 highScores <- loadHighScores
 case screen of
  StartScreen ->
      pure $ titleScreen $ assets uiState
  GameScreen world ->
      pure $ pictures
        [ text $ show (bonusPoints world + floor (elapsedTime world)
          + floor (characterAltitude world * 3)),
          renderWorld (windowSize uiState) (assets uiState) world
        ]
  HighScoreScreen score characterAltitude ->
    pure $ scale 0.2 0.2 $ pictures $
    map (uncurry (translate 0 . (* 120)) . second (text . showHighScore)) $ zip [0 ..] highScores

renderWorld :: Vector -> Assets -> World -> Picture
renderWorld
  windowSize
  assets
  World
    { character = Object {position = (x, y), velocity = (vx, vy)},
      viewport = viewport@ViewPort {viewPortTranslate},
      ..
    } =
    applyViewPortToPicture viewport
      $ pictures
      $ generateClouds viewPortTranslate
        : drawRuler ((0, y) P.+ rulerPosition) rulerDimensions rulerNumberOfTickMarks rulerIndicatedMeasurement white yellow red
        : case jump of
          -- TODO add vectorLength variable infront that depends on strength
          Just (InitJump m) -> line [(x, y), (x, y) P.+ resizeVectorFactor 60 300 (m P.- mousePosition) P.* getNormVector (m P.- mousePosition)]
          Nothing -> blank
        : translate
          x
          y
          ( pictures
              ( ( case characterStatus of
                    CharacterAtBalloon _ -> [ballonBlue assets] -- placeholder
                    CharacterInBubble _ -> [characterBubble assets]
                    PlainCharacter _ -> []
                )
                  ++ [frogSprite assets FrogState {eyesOpen = True, mouthOpen = False, directionRight = vx >= 0}]
              )
          )
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
              Balloon -> ballonRed assets
          )

      -- using prime factors and the screen size as modulo 'ransomly' scatters the clouds can
      generateClouds (x, y) =
        translate x y
          $ pictures
          $ map (\i -> translate ((751 * i) `mod'` windowWidth - (windowWidth / 2)) ((971 * i) `mod'` windowHeight - (windowHeight / 2)) (cloud assets)) [1 .. 7]
      windowWidth = fst windowSize * viewPortScale viewport
      windowHeight = snd windowSize * viewPortScale viewport
      rightBorderPosition = (windowWidth / 2, 0)
      rulerDimensions = (100, windowHeight)
      rulerDimensionsX = fst rulerDimensions
      rulerDimensionsY = snd rulerDimensions
      rulerPosition = rightBorderPosition
      rulerNumberOfTickMarks = 10
      rulerIndicatedMeasurement = windowHeight / 2
