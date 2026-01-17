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
  color,
  red,
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
              ( x + moveSpeed * t * modifier,
                y + yChange
              )
          },
      characterStatus = updateCharacterStatus,
      assets = a {player = color red player}
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
