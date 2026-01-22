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
  withAlpha,
  yellow,
 )
import Graphics.Gloss.Data.ViewPort (
  ViewPort (ViewPort, viewPortScale),
  applyViewPortToPicture,
  viewPortTranslate,
 )

import Data.Map qualified as M
import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (*),
  (+),
  (-),
 )

-- import GlossyRuler (drawRuler)
import Controller (levelBoundary, oneMeter)
import HighScore
import Math
import Model (
  Assets (..),
  CharacterStatus (..),
  GlobalState (..),
  HighScore (..),
  Jump (..),
  NewHighScore (..),
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
  case screen of
    StartScreen ->
      pure $ titleScreen $ assets uiState
    GameScreen world ->
      pure
        $ pictures
          [ applyViewPortToPicture (viewport world)
              $ translate
                ((-fst (windowSize uiState)) / 2 + 10 - fst (viewPortTranslate (viewport world)))
                (snd (windowSize uiState) / 2 - 60 - snd (viewPortTranslate (viewport world)))
              $ scale 0.5 0.5
              $ text
              $ "Score: "
                ++ show
                  ( bonusPoints world
                      + floor (elapsedTime world)
                      + floor (characterAltitude world * 3)
                  ),
            renderWorld (windowSize uiState) (assets uiState) world
          ]
    HighScoreScreen highScores newHighScore -> do
      highScores <- loadHighScores
      let
        maybeNewHighScore = case newHighScore of
          NoNewHighScore -> Nothing
          NewHighScore {..} ->
            Just
              HighScore
                { playerName = incompleteNewPlayerName,
                  scorePoints = newScorePoints,
                  scoreAltitude = newScoreAltitude
                }
          CompleteNewHighScore {..} ->
            Just
              HighScore
                { playerName = newPlayerName,
                  scorePoints = newScorePoints,
                  scoreAltitude = newScoreAltitude
                }
        additionalText = case maybeNewHighScore of
          Nothing -> []
          Just {} ->
            ["Type your name then press", "Enter to save your score."]
        xFactor = fst (windowSize uiState) / 2500
        yFactor = snd (windowSize uiState) / 2500
        scalingFactor = min xFactor yFactor
      pure
        $ addBackgroundPicture (titleScreen $ assets uiState)
        $ scale scalingFactor scalingFactor
        $ addBackgroundPicture (color (withAlpha 0.9 white) $ rectangleSolid 2400 2360)
        $ translate (-1200) 920
        $ pictures
        $ scale 2 2 (text "High Score List")
          : zipWith
            (translate 0 . (* (-130)))
            [1 ..]
            ( blank
                : map
                  (text . showHighScore)
                  (maybeToList maybeNewHighScore ++ highScores)
                ++ blank
                : map text (additionalText ++ ["Press ESC to exit."])
            )

addBackgroundPicture :: Picture -> Picture -> Picture
addBackgroundPicture p = pictures . (p :) . (: [])

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
      $ ruler
        : generateClouds viewPortTranslate
        {-
        : drawRuler
          ((0, y) P.+ rulerPosition)
          rulerDimensions
          rulerNumberOfTickMarks
          rulerIndicatedMeasurement
          white
          yellow
          red
          -}
        : case jump of
          -- TODO add vectorLength variable infront that depends on strength
          Just (InitJump m) ->
            line
              [ (x, y),
                (x, y)
                  P.+ resizeVectorFactor 60 300 (m P.- mousePosition)
                    P.* getNormVector (m P.- mousePosition)
              ]
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
                  ++ [ frogSprite
                         assets
                         FrogState
                           { eyesOpen = True,
                             mouthOpen = False,
                             directionRight = vx >= 0
                           }
                     ]
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

      -- using prime factors and the screen size as modulo 'ransomly'
      -- scatters the clouds can
      generateClouds (x, y) =
        translate x y
          $ pictures
          $ map
            ( \i ->
                translate
                  ((751 * i) `mod'` steppedWindowWidth - steppedWindowWidth / 2)
                  ((971 * i) `mod'` steppedWindowHeight - steppedWindowHeight / 2)
                  (cloud assets)
            )
            [-7 .. 7]
      steppedScale =
        min 1
          $ fromIntegral (round (5 * sqrt (viewPortScale viewport))) / 10
      steppedWindowWidth = fst windowSize / steppedScale
      steppedWindowHeight = snd windowSize / steppedScale
      windowWidth = fst windowSize * viewPortScale viewport
      windowHeight = snd windowSize * viewPortScale viewport

      ruler :: Picture
      ruler =
        translate
          (fst windowSize / 2 / viewPortScale viewport - fst viewPortTranslate)
          0
          $ pictures
          $ map
            (\y -> drawTick y $ line [(-10, y * 25), (0, y * 25)])
            [max zeroMeter screenBottom * 10 .. screenTop * 10]

      zeroMeter = fromIntegral $ round (snd levelBoundary) `div` round oneMeter
      screenBottom =
        fromIntegral
          $ ( floor
                (-snd viewPortTranslate - snd windowSize / 2 / viewPortScale viewport)
                `div` round oneMeter
            )
            - 1
      screenTop =
        fromIntegral
          $ ( ceiling
                (-snd viewPortTranslate + snd windowSize / 2 / viewPortScale viewport)
                `div` round oneMeter
            )
            + 1

      drawTick :: Float -> Picture -> Picture
      drawTick y
        | y `mod'` 10 == 0 =
            pictures
              . (scaleLabel :)
              . (: [])
              . scale 2 1
              . color red
        | otherwise =
            color black
        where
          altitude = fromIntegral (y `div'` 10) - zeroMeter
          baseScale = 0.2
          scaleFactor =
            if (y - 10 * zeroMeter) `mod'` 100 == 0
              then baseScale / viewPortScale viewport
              else baseScale
          scaleLabel =
            translate
              (-25 + (-25) * logBase 10 altitude * scaleFactor / baseScale)
              (y * 25)
              $ scale scaleFactor scaleFactor
              $ text
              $ show
              $ round altitude

{--      rightBorderPosition = (windowWidth / 2, 0)
      rulerDimensions = (100, windowHeight)
      rulerDimensionsX = fst rulerDimensions
      rulerDimensionsY = snd rulerDimensions
      rulerPosition = rightBorderPosition
      rulerNumberOfTickMarks = 10
      rulerIndicatedMeasurement = windowHeight / 2 --}
