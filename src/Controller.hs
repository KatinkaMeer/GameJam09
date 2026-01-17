{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Controller where

import Data.List (delete, findIndex)
import Data.Maybe (isNothing)
import Data.Tuple.Extra (first, second)
import Graphics.Gloss.Interface.Pure.Game (
  Event (EventKey),
  Key (MouseButton, SpecialKey),
  KeyState (Down, Up),
  MouseButton (LeftButton),
  SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp),
  blank,
  circleSolid,
  color,
  red,
  yellow,
 )

import Data.Map qualified as M
import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (+),
 )

import Model (
  Assets (..),
  CharacterStatus (..),
  Jump (..),
  Object (..),
  World (..),
  characterFloats,
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
    EventKey (MouseButton LeftButton) Down _ mpos
      | isNothing jump,
        characterFloats characterStatus ->
          world {jump = Just InitJump {mousePoint = mpos}}
    EventKey (MouseButton LeftButton) Up _ mpos
      | Just InitJump {..} <- jump,
        characterFloats characterStatus ->
          let
            -- TODO add minimum velocity and maximum velocity as variables
            rposx = fst mousePoint
            rposy = snd mousePoint
            mposx = fst mpos
            mposy = snd mpos
            vx = mposx - rposx
            vy = mposy - rposy
            v2 = vx * vx + vy * vy
            rv = sqrt $ v2 / (max 1 $ min v2 100)
            vx' = vx / rv
            vy' = vy / rv
          in
            world
              { character =
                  character
                    { velocity = velocity character P.+ (vx', vy') -- galilei
                    },
                jump = Nothing -- new Jump possible
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
      collisionIndex = M.keys $ M.filter (not . collisionWithPlayer) objects,
      assets = case updateCharacterStatus of
        CharacterInBubble toPop
          -- cannot stack color, e.g. color red $ color yellow ...
          -- is just color yellow ...
          | toPop < 3 -> a {bubble = color red $ circleSolid 30}
          | toPop < 7 -> a {bubble = color yellow bubble}
        PlainCharacter -> a {bubble = blank}
        _ -> a,
      -- TODO: use for new objects, then increment
      -- or just increment always?
      nextId = nextId
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
    -- CHANGE THIS
    collisionWithPlayer (_, Object {position = (oX, oY)}) = sqrt ((oX - x) ^ 2 + (oY - y) ^ 2) <= 20
