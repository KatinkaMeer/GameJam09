{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Controller where

import Data.List (delete, find, findIndex)
import Data.Map (lookup, member)
import Data.Maybe (isNothing, listToMaybe)
import Data.Tuple.Extra (first, second)
import Graphics.Gloss.Interface.Pure.Game (
  Event (EventKey, EventMotion),
  Key (MouseButton, SpecialKey),
  KeyState (Down, Up),
  MouseButton (LeftButton),
  SpecialKey (KeyDown, KeyEsc, KeyLeft, KeyRight, KeySpace, KeyUp),
  blank,
  circleSolid,
  color,
  red,
  yellow,
 )
import System.Exit (exitSuccess)

import Data.Map qualified as M
import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (+),
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
import Sound (
  playBalloonInflateSound,
  playBalloonPopSound,
  playBubblePopSound,
  playBubblesSound,
 )

handleInput :: Event -> GlobalState -> IO GlobalState
handleInput event state@GlobalState {..} =
  do
    setMousePosition (mousePosFromEvent event)
    <$> case event of
      EventKey (SpecialKey KeyEsc) Up _ _
        | StartScreen <- screen ->
            exitSuccess
        | otherwise ->
            pure state {screen = StartScreen}
      EventKey (SpecialKey KeySpace) Up _ _
        | StartScreen <- screen -> startGame
        | GameScreen {} <- screen -> startGame
      EventKey (SpecialKey k) action _ _ ->
        pure
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
            pure
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
              vx = 1000 * (rposx - mposx)
              vy = 1000 * (rposy - mposy)
              v2 = vx * vx + vy * vy
              rv = sqrt $ v2 / max 1 (min v2 1000000)
              vx' = vx / rv
              vy' = vy / rv
            in
              do
                case characterStatus of
                  CharacterAtBalloon {} -> playBalloonPopSound
                  CharacterInBubble {} -> playBubblePopSound
                  PlainCharacter -> pure ()
                pure
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
      _ -> pure state
  where
    startGame = do
      playBubblesSound
      pure state {screen = GameScreen initialWorld}
    setMousePosition position gState@GlobalState {..}
      | GameScreen world@World {..} <- screen = gState {screen = GameScreen world {mousePosition = position}}
      | otherwise = gState

-- (left/right, bottom), top unlimited
levelBoundary :: (Float, Float)
levelBoundary = (500, -500)

-- TODO should the initial game settings be resolved in another way?
moveSpeed, floatSpeed, fallSpeed, vMaxScale, vBalloonMax, vBubbleMax, vPlainCharacterMax :: Float
moveSpeed = 300
floatSpeed = 60
fallSpeed = 200
vMaxScale = 1
vBalloonMax = vMaxScale * 400
vBubbleMax = vMaxScale * 200
vPlainCharacterMax = vMaxScale * 600

betweenSpeed :: Float -> Float -> Float
betweenSpeed vmax v = max (-vmax) (min vmax v)

mousePosFromEvent :: Event -> (Float, Float)
mousePosFromEvent (EventKey _ _ _ pos) = pos
mousePosFromEvent (EventMotion pos) = pos
mousePosFromEvent _ = (0, 0)

update :: Float -> GlobalState -> IO GlobalState
update t state@GlobalState {..} = do
  nextScreen <- case screen of
    StartScreen -> pure StartScreen
    GameScreen world -> GameScreen <$> updateWorld t uiState world
    HighScoreScreen -> pure StartScreen
  pure $ state {screen = nextScreen}

updateWorld :: Float -> UiState -> World -> IO World
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
        CharacterAtBalloon timer -> (0.985 * betweenSpeed vBalloonMax vx, betweenSpeed vBalloonMax (vy + 2 * t * floatSpeed), characterInBalloon $ timerUpdate timer)
        CharacterInBubble timer -> (0.98 * betweenSpeed vBubbleMax vx, betweenSpeed vBubbleMax (vy + t * floatSpeed), characterInBubble $ timerUpdate timer)
        PlainCharacter ->
          ( 0.99 * betweenSpeed vPlainCharacterMax vx,
            betweenSpeed vPlainCharacterMax (vy - t * fallSpeed),
            -- only care about first collision
            case listToMaybe newCollisions >>= \k -> M.lookup k objects of
              Nothing -> PlainCharacter
              Just (Bubble, _) -> CharacterInBubble 10
              Just (Balloon, _) -> CharacterAtBalloon 5
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
      do
        nextJump <- case (characterStatus, updateCharacterStatus) of
          (CharacterAtBalloon {}, PlainCharacter) ->
            Nothing <$ playBalloonPopSound
          (PlainCharacter, CharacterAtBalloon {}) ->
            Nothing <$ playBalloonInflateSound
          (CharacterInBubble {}, PlainCharacter) ->
            Nothing <$ playBubblePopSound
          (PlainCharacter, CharacterInBubble {}) ->
            Nothing <$ playBubblePopSound
          _ -> pure jump
        pure
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
              jump = nextJump,
              -- remove objects colliding with player
              objects = M.filterWithKey (\k _ -> k `notElem` newCollisions) objects,
              -- TODO: use and increment or increment every update
              nextId = nextId
            }
