{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Controller where

import Data.List (delete)
import Data.Tuple.Extra (first, second)
import Graphics.Gloss.Interface.Pure.Game (
  Event (EventKey),
  Key (SpecialKey),
  KeyState (Down, Up),
  SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp),
  blank,
  circleSolid,
  color,
  red,
  yellow,
 )

import Model (
  Assets (..),
  CharacterStatus (..),
  Object (Object, position),
  World (..),
  characterInBalloon,
  characterInBubble,
 )

handleInput :: Event -> World -> World
handleInput event world@World {..} =
  case event of
    EventKey (SpecialKey k) action _ _ ->
      world
        { pressedKeys = case action of
            Down -> k : pressedKeys
            Up -> delete k pressedKeys
        }
    _ -> world

-- (left/right, bottom), top unlimited
levelBoundary :: (Float, Float)
levelBoundary = (500, -500)

moveSpeed, floatSpeed, fallSpeed :: Float
moveSpeed = 300
floatSpeed = 60
fallSpeed = 200

update :: Float -> World -> World
update t world@World {character = me@(Object (x, y) _), assets = a@Assets {..}, ..} =
  world
    { character =
        me
          { position =
              coordinateClamp
                ( x + moveSpeed * t * modifier,
                  y + yChange
                )
          },
      characterStatus = updateCharacterStatus,
      assets = case updateCharacterStatus of
        CharacterInBubble toPop
          -- cannot stack color, e.g. color red $ color yellow ...
          -- is just color yellow ...
          | toPop < 3 -> a {bubble = color red $ circleSolid 30}
          | toPop < 7 -> a {bubble = color yellow bubble}
        PlainCharacter -> a {bubble = blank}
        _ -> a
    }
  where
    modifier
      | KeyLeft `elem` pressedKeys = -1
      | KeyRight `elem` pressedKeys = 1
      | otherwise = 0

    (yChange, updateCharacterStatus) = case characterStatus of
      CharacterInBalloon timer -> (2 * t * floatSpeed, characterInBalloon $ timerUpdate timer)
      CharacterInBubble timer -> (t * floatSpeed, characterInBubble $ timerUpdate timer)
      PlainCharacter -> (-t * fallSpeed, PlainCharacter)

    timerUpdate = (- t)

    coordinateClamp (xCoord, yCoord) =
      ( if abs xCoord > fst levelBoundary then x else xCoord,
        if yCoord < snd levelBoundary then y else yCoord
      )
