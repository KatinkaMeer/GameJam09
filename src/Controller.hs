{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Controller where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Fixed (mod')
import Control.Monad.Random.Class (MonadRandom, fromList, getRandomR, uniform)
import Data.List (delete, find, findIndex)
import Data.Map (lookup, member)
import Data.Maybe (isNothing, listToMaybe)
import Data.Ratio ((%))
import Data.Tuple.Extra (first, second)
import Graphics.Gloss.Data.ViewPort (ViewPort (..))
import Graphics.Gloss.Interface.Pure.Game (
  Event (EventKey, EventMotion, EventResize),
  Key (Char, MouseButton, SpecialKey),
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
import System.Random (Random (random))

import Data.Map qualified as M
import HighScore (logNewHighScore)
import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (*),
  (+),
 )

import Math
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
  setMousePosition (mousePosFromEvent event)
    <$> case event of
      EventKey (Char 'H') Up _ _
        | StartScreen <- screen ->
          pure state {screen = HighScoreScreen Nothing Nothing}
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
        | GameScreen world@World {viewport = ViewPort {..}, ..} <- screen,
          Just InitJump {..} <- jump,
          characterFloats characterStatus ->
            let
              -- TODO add minimum velocity and maximum velocity as variables
              rposx = fst mousePoint
              rposy = snd mousePoint
              mposx = fst mpos
              mposy = snd mpos
              vx = rposx - mposx
              vy = rposy - mposy
              direction = getNormVector (vx, vy)
              magnitude =
                (vMaxScale * 0.005 * scalarProduct (vx, vy) (vx, vy))
                  * ((1 - viewPortScale) + 1)
            in
              do
                case characterStatus of
                  CharacterAtBalloon {} -> playBalloonPopSound
                  CharacterInBubble {} -> playBubblePopSound
                  PlainCharacter {} -> pure ()
                pure
                  state
                    { screen =
                        GameScreen
                          world
                            { character =
                                character
                                  { velocity = velocity character P.+ (3 * magnitude P.* direction)
                                  -- galilei
                                  },
                              characterStatus = PlainCharacter 0,
                              jump = Nothing -- new Jump possible
                            }
                    }
      EventResize newSize ->
        pure
          state
            { uiState =
                uiState
                  { windowSize = bimap fromIntegral fromIntegral newSize
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
    GameScreen world@World {character = Object {..}, ..} ->
      ( case characterStatus of
          PlainCharacter timer
            | timer <= -5 || snd position <= (snd levelBoundary + 10)
            -> pure $ HighScoreScreen (Just $ bonusPoints + floor elapsedTime
               + floor (characterAltitude * 3)) (Just $ floor characterAltitude)
          _ -> GameScreen <$> updateWorld t uiState world
      )
    HighScoreScreen mScore mAltitude
      | Just score <- mScore
      , Just altitude <- mAltitude -> do
        logNewHighScore ("Kathy", (score, altitude))
        pure $ HighScoreScreen mScore mAltitude
      | otherwise ->
        pure $ HighScoreScreen Nothing Nothing
  pure $ state {screen = nextScreen}

updateWorld :: Float -> UiState -> World -> IO World
updateWorld
  t
  uiState@UiState {..}
  world@World {character = me@(Object (x, y) (vx, vy)), ..} =
    let
      modifier
        | KeyLeft `elem` pressedKeys = -1
        | KeyRight `elem` pressedKeys = 1
        | otherwise = 0
      (vx', vy', updateCharacterStatus) = case characterStatus of
        CharacterAtBalloon timer -> (0.985 * betweenSpeed vBalloonMax vx, betweenSpeed vBalloonMax (vy + 2 * t * floatSpeed), characterInBalloon $ timerUpdate timer)
        CharacterInBubble timer -> (0.98 * betweenSpeed vBubbleMax vx, betweenSpeed vBubbleMax (vy + t * floatSpeed), characterInBubble $ timerUpdate timer)
        PlainCharacter timer ->
          ( 0.99 * betweenSpeed vPlainCharacterMax vx,
            betweenSpeed vPlainCharacterMax (vy - t * fallSpeed),
            -- only care about first collision
            case listToMaybe newCollisions >>= \k -> M.lookup k objects of
              Nothing -> PlainCharacter $ timerUpdate timer
              Just (Bubble, _) -> CharacterInBubble 10
              Just (Balloon, _) -> CharacterAtBalloon 5
          )

      timerUpdate = (- t)

      -- can't think of a good way to make this more generic
      viewportScaling
        | y > 2000 = 0.125
        | y > 1000 = 0.25
        | y > 500 = 0.5
        | otherwise = 1

      coordinateClamp (xCoord, yCoord) =
        ( xCoord,
          if yCoord < snd levelBoundary then y else yCoord
        )
      -- CHANGE THIS
      collisionWithPlayer (_, Object {position = (oX, oY)}) =
        sqrt ((oX - x) ^ 2 + (oY - y) ^ 2) <= 40
      newCollisions = M.keys $ M.filter collisionWithPlayer objects
      updateMovement object@Object {position = (x, y), velocity = (vx, vy)} =
        object
          { position = (x + vx * t, y + vy * t)
          }
    in
      do
        (nextJump, newBonusPoints) <- case (characterStatus, updateCharacterStatus) of
          (CharacterAtBalloon {}, PlainCharacter {}) ->
            (Nothing, bonusPoints) <$ playBalloonPopSound
          (PlainCharacter {}, CharacterAtBalloon {}) ->
            (Nothing, bonusPoints) <$ playBalloonInflateSound
          (CharacterInBubble {}, PlainCharacter {}) ->
            (Nothing, bonusPoints) <$ playBubblePopSound
          (PlainCharacter {}, CharacterInBubble {}) ->
            (Nothing, bonusPoints + 20) <$ playBubblePopSound
          _ -> pure (jump, bonusPoints)
        randomNumber <- getRandomR (1 :: Int, 100)
        spawnedObjects <-
          if randomNumber < 10
          then zip [nextId ..]
            <$> mapM (const $ spawnObject uiState viewport) [1]
          else pure []
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
              nextId = nextId + fromIntegral (length spawnedObjects),
              -- remove objects colliding with player
              objects =
                M.union (M.fromList spawnedObjects)
                  $ M.map (second updateMovement) (M.filterWithKey (\k _ -> k `notElem` newCollisions) objects),
              -- TODO: use and increment or increment every update
              viewport = viewport {viewPortScale = viewportScaling},
              bonusPoints = newBonusPoints,
              elapsedTime = (+ t) elapsedTime
            }

scalingFactor :: Float
scalingFactor = 1

spawnObject
  :: MonadRandom m
  => UiState
  -> ViewPort
  -> m (ObjectType, Object)
spawnObject
  UiState {windowSize = (windowX, windowY)}
  ViewPort {viewPortTranslate = (shiftX, shiftY), ..} = do
    x <- (shiftX +) <$> getRandomR (halfWindowX * viewPortScale, maxX)
    y <- (shiftY +) <$> getRandomR (halfWindowY * viewPortScale, maxY)
    objectType <- fromList [(Bubble, 2 % 5), (Balloon, 3 % 5)]
    object <- case objectType of
      Bubble -> do
        position <-
          uniform
            [ (-x - halfCharacterSize, y + halfCharacterSize),
              (-x - halfCharacterSize, y - windowY * viewPortScale),
              (-x - halfCharacterSize, -y - halfCharacterSize),
              (x - windowX * viewPortScale, y + halfCharacterSize),
              (x - windowX * viewPortScale, -y - halfCharacterSize),
              (x + halfCharacterSize, y + halfCharacterSize),
              (x + halfCharacterSize, y - windowY * viewPortScale),
              (x + halfCharacterSize, -y - halfCharacterSize)
            ]
        vx <- getRandomR (-(vBubbleMax / 4), vBubbleMax / 4)
        vy <- getRandomR (-(vBubbleMax / 4), vBubbleMax / 4)
        pure $ Object {position = position, velocity = (vx, vy)}
      Balloon -> do
        position <-
          uniform
            [ (-x - halfCharacterSize, -y - halfCharacterSize),
              (x - windowX * viewPortScale, -y - halfCharacterSize),
              (x + halfCharacterSize, -y - halfCharacterSize)
            ]
        vx <- getRandomR (-(vBalloonMax / 4), vBalloonMax / 4)
        pure $ Object {position = position, velocity = (vx, vBalloonMax)}
    pure (objectType, object)
    where
      halfWindowX = windowX / 2
      halfWindowY = windowY / 2
      maxX = max (halfWindowX * viewPortScale * 3) (maxJumpDistance vPlainCharacterMax / 1000)
      maxY = max (halfWindowY * viewPortScale * 3) (maxJumpDistance vPlainCharacterMax / 1000)
      halfCharacterSize = 64
