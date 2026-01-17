{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Controller where

import Data.List (delete, find, findIndex)
import Data.Map (lookup, member)
import Data.Map qualified as M
import Data.Maybe (isNothing, listToMaybe)
import Data.Tuple.Extra (first, second)
import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (+),
 )
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
import Model (
  Assets (..),
  CharacterStatus (..),
  GlobalState (..),
  Jump (..),
  Object (..),
  ObjectType (..),
  Screen (..),
  UiState (..),
  World (..),
  characterFloats,
  characterInBalloon,
  characterInBubble,
  initialWorld,
 )

handleInput :: Event -> GlobalState -> GlobalState
handleInput event state@GlobalState {..} =
  case event of
    EventKey (SpecialKey k) action _ _ ->
      state
        { uiState =
            uiState
              { pressedKeys = case action of
                  Down -> k : pressedKeys uiState
                  Up -> delete k $ pressedKeys uiState
              }
        }
    EventKey (MouseButton LeftButton) Down _ mpos
      | GameScreen world@World {..} <- screen,
        isNothing jump,
        characterFloats characterStatus ->
          state
            { screen =
                GameScreen world {jump = Just InitJump {mousePoint = mpos}}
            }
    EventKey (MouseButton LeftButton) Up _ mpos
      | GameScreen world@World {..} <- screen,
        Just InitJump {..} <- jump,
        characterFloats characterStatus ->
          let
            -- TODO add minimum velocity and maximum velocity as variables
            rposx = fst mousePoint
            rposy = snd mousePoint
            mposx = fst mpos
            mposy = snd mpos
            vx = 100 * (rposx - mposx)
            vy = 100 * (rposy - mposx)
            v2 = vx * vx + vy * vy
            rv = sqrt $ v2 / max 1 (min v2 100)
            vx' = vx / rv
            vy' = vy / rv
          in
            state
              { screen =
                  GameScreen
                    world
                      { character =
                          character
                            { velocity = velocity character P.+ (vx', vy')
                            -- galilei
                            },
                        characterStatus = PlainCharacter,
                        jump = Nothing -- new Jump possible
                      }
              }
    _ -> state

-- (left/right, bottom), top unlimited
levelBoundary :: (Float, Float)
levelBoundary = (500, -500)

moveSpeed, floatSpeed, fallSpeed, vmax :: Float
moveSpeed = 300
floatSpeed = 60
fallSpeed = 200
vmax = 300

betweenSpeed :: Float -> Float -> Float
betweenSpeed vmax v = max (-vmax) (min vmax v)

update :: Float -> GlobalState -> GlobalState
update t state@GlobalState {..} =
  state
    { screen = case screen of
        StartScreen -> GameScreen initialWorld
        GameScreen world -> GameScreen $ updateWorld t uiState world
        HighScoreScreen -> StartScreen
    }

updateWorld :: Float -> UiState -> World -> World
updateWorld
  t
  UiState {..}
  world@World {character = me@(Object (x, y) (vx, vy)), ..} =
    let
      modifier
        | KeyLeft `elem` pressedKeys = -1
        | KeyRight `elem` pressedKeys = 1
        | otherwise = 0
      (vx', vy', updateCharacterStatus) = case characterStatus of
        CharacterInBalloon timer -> (vx, betweenSpeed vmax (vy + 2 * t * floatSpeed), characterInBalloon $ timerUpdate timer)
        CharacterInBubble timer -> (vx, betweenSpeed vmax (vy + t * floatSpeed), characterInBubble $ timerUpdate timer)
        PlainCharacter ->
          ( vx,
            betweenSpeed vmax (vy - t * fallSpeed),
            -- only care about first collision
            case listToMaybe newCollisions >>= \k -> M.lookup k objects of
              Nothing -> PlainCharacter
              Just (Bubble, _) -> CharacterInBubble 10
              Just (Balloon, _) -> CharacterInBalloon 5
          )

      timerUpdate = (- t)

      coordinateClamp (xCoord, yCoord) =
        ( if abs xCoord > fst levelBoundary then x else xCoord,
          if yCoord < snd levelBoundary then y else yCoord
        )
      -- CHANGE THIS
      collisionWithPlayer (_, Object {position = (oX, oY)}) =
        sqrt ((oX - x) ^ 2 + (oY - y) ^ 2) <= 40
      newCollisions = M.keys $ M.filter collisionWithPlayer objects
    in
      world
        { character =
            me
              { position =
                  coordinateClamp
                    ( x + moveSpeed * t * modifier + vx' * t,
                      y + vy' * t
                    ),
                velocity = (vx', vy')
              },
          collisions = newCollisions,
          characterStatus = updateCharacterStatus,
          -- remove objects colliding with player
          objects = M.filterWithKey (\k _ -> k `notElem` newCollisions) objects,
          -- TODO: use and increment or increment every update
          nextId = nextId
        }
