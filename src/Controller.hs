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
 )

import Model (World (World, position, pressedKeys))

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

moveSpeed, floatSpeed :: Float
moveSpeed = 300
floatSpeed = 60

update :: Float -> World -> World
update t world@World {position = (x, y), ..} =
  world
    { position = (x + moveSpeed * t * modifier, y + t * floatSpeed)
    }
  where
    modifier
      | KeyLeft `elem` pressedKeys = -1
      | KeyRight `elem` pressedKeys = 1
      | otherwise = 0
