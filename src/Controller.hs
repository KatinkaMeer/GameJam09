{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE RecordWildCards #-}

module Controller where

import Control.Monad.Random.Class (
  MonadRandom,
  fromList,
  getRandomR,
  uniform,
  weighted,
 )
import Data.Bifunctor (Bifunctor (bimap))
import Data.Fixed (mod')
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
  SpecialKey (..),
  blank,
  circleSolid,
  color,
  red,
  yellow,
 )
import System.Exit (exitSuccess)
import System.Random (Random (random))

import Data.Map qualified as M
import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (*),
  (+),
 )

import HighScore (loadHighScores, logNewHighScore)
import Math
import Model (
  Assets (..),
  CharacterStatus (..),
  GlobalState (..),
  HighScore (HighScore),
  Jump (..),
  NewHighScore (..),
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
      EventKey (SpecialKey KeyEsc) Up _ _
        | StartScreen <- screen ->
            exitSuccess
        | otherwise ->
            pure state {screen = StartScreen}
      EventKey (SpecialKey KeySpace) Up _ _
        | StartScreen <- screen -> startGame
        | GameScreen {} <- screen -> startGame
      EventKey (SpecialKey KeyEnter) Up _ _
        | HighScoreScreen highScores NewHighScore {..} <- screen ->
            pure
              state
                { screen =
                    HighScoreScreen
                      highScores
                      CompleteNewHighScore
                        { newPlayerName = incompleteNewPlayerName,
                          newScorePoints = newScorePoints,
                          newScoreAltitude = newScoreAltitude
                        }
                }
      EventKey (SpecialKey key) Up _ _
        | key `elem` [KeyBackspace, KeyDelete],
          HighScoreScreen highScores newHighScore@NewHighScore {..} <- screen ->
            pure
              state
                { screen =
                    HighScoreScreen
                      highScores
                      newHighScore
                        { incompleteNewPlayerName =
                            if incompleteNewPlayerName == "..."
                              || length incompleteNewPlayerName < 2
                              then "..."
                              else init incompleteNewPlayerName
                        }
                }
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
                                  { velocity =
                                      velocity character
                                        P.+ (3 * magnitude P.* direction)
                                        -- galilei
                                  },
                              characterStatus = PlainCharacter 0,
                              jump = Nothing -- new Jump possible
                            }
                    }
      EventKey (Char 'H') Up _ _
        | StartScreen <- screen -> do
            highScores <- loadHighScores
            pure state {screen = HighScoreScreen highScores NoNewHighScore}
      EventKey (Char c) Up _ _
        | HighScoreScreen highScores newHighScore@NewHighScore {..} <- screen ->
            pure
              state
                { screen =
                    HighScoreScreen
                      highScores
                      newHighScore
                        { incompleteNewPlayerName =
                            if incompleteNewPlayerName == "..."
                              then [c]
                              else incompleteNewPlayerName ++ [c]
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
      | GameScreen world@World {..} <- screen =
          gState {screen = GameScreen world {mousePosition = position}}
      | otherwise = gState

-- (left/right, bottom), top unlimited
levelBoundary :: (Float, Float)
levelBoundary = (500, -500)

-- | How many pixels correspond to one meter
oneMeter :: Float
oneMeter = 250

-- TODO should the initial game settings be resolved in another way?
moveSpeed, floatSpeed, fallSpeed :: Float
moveSpeed = 300
floatSpeed = 60
fallSpeed = 200

vMaxScale, vBalloonMax, vBubbleMax, vPlainCharacterMax :: Float
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
            | timer <= -10 || snd position <= (snd levelBoundary + 10) -> do
                highScores <- loadHighScores
                pure
                  . HighScoreScreen
                    highScores
                  $ NewHighScore
                    "..."
                    ( bonusPoints
                        + floor elapsedTime
                        + floor (characterAltitude * 3)
                    )
                    (floor characterAltitude)
          _ -> GameScreen <$> updateWorld t uiState world
      )
    HighScoreScreen highScores newScore
      | CompleteNewHighScore {..} <- newScore -> do
          logNewHighScore
            $ HighScore newPlayerName newScorePoints newScoreAltitude
          pure $ HighScoreScreen highScores NoNewHighScore
      | otherwise ->
          pure $ HighScoreScreen highScores newScore
  pure $ state {screen = nextScreen}

updateWorld :: Float -> UiState -> World -> IO World
updateWorld
  t
  uiState@UiState {..}
  world@World
    { character = me@(Object (x, y) (vx, vy)),
      viewport = viewport@ViewPort {..},
      ..
    } =
    let
      modifier
        | KeyLeft `elem` pressedKeys = -1
        | KeyRight `elem` pressedKeys = 1
        | otherwise = 0
      (vx', vy', updateCharacterStatus) = case characterStatus of
        CharacterAtBalloon timer ->
          ( 0.985 * betweenSpeed vBalloonMax vx,
            betweenSpeed vBalloonMax (vy + 2 * t * floatSpeed),
            characterInBalloon $ timerUpdate timer
          )
        CharacterInBubble timer ->
          ( 0.98 * betweenSpeed vBubbleMax vx,
            betweenSpeed vBubbleMax (vy + t * floatSpeed),
            characterInBubble $ timerUpdate timer
          )
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

      viewportScaling
        | step < 9 =
            smoothTransition
              (stepBase * step, 1 - step / 10)
              (stepBase * (step + 1), 1 - (step + 1) / 10)
              y
        | otherwise = 0.1
        where
          step = fromIntegral $ floor (y / stepBase)
          stepBase = snd windowSize * 10
          firstStep = stepBase
          secondStep = 2 * stepBase
          endStep = 3 * stepBase

      viewportTranslation
        | x < (fst viewPortTranslate + 128 - halfWidth) =
            first (const (-x - quarterWidth)) viewPortTranslate
        | x > (fst viewPortTranslate - 128 + halfWidth) =
            first (const (-x + quarterWidth)) viewPortTranslate
        | otherwise = viewPortTranslate
        where
          quarterWidth = fst windowSize / 4 / viewPortScale
          halfWidth = 2 * quarterWidth

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
            then
              zip [nextId ..]
                <$> mapM (const $ spawnObject uiState viewport) [1]
            else pure []
        pure
          world
            { character =
                me
                  { position =
                      coordinateClamp
                        ((x, y) P.+ (t P.* (moveSpeed * modifier + vx, vy'))),
                    velocity = (vx', vy')
                  },
              characterAltitude =
                -(snd levelBoundary / oneMeter) - (snd viewPortTranslate / oneMeter),
              collisions = newCollisions,
              characterStatus = updateCharacterStatus,
              jump = nextJump,
              nextId = nextId + fromIntegral (length spawnedObjects),
              -- remove objects colliding with player
              objects =
                M.union (M.fromList spawnedObjects)
                  $ M.map
                    (second updateMovement)
                    (M.filterWithKey (\k _ -> k `notElem` newCollisions) objects),
              -- TODO: use and increment or increment every update
              viewport =
                viewport
                  { viewPortScale = viewportScaling,
                    viewPortTranslate = second (const (-y)) viewportTranslation
                  },
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
    x <- (-shiftX +) <$> getRandomR (halfWindowX / viewPortScale, maxX)
    y <- (-shiftY +) <$> getRandomR (halfWindowY / viewPortScale, maxY)
    objectType <- fromList [(Bubble, 2 % 5), (Balloon, 3 % 5)]
    object <- case objectType of
      Bubble -> do
        position <-
          weighted
            [ ((-x - halfCharacterSize, y + halfCharacterSize), 3 % 16),
              ((-x - halfCharacterSize, y - windowY / viewPortScale), 2 % 16),
              ((-x - halfCharacterSize, -y - halfCharacterSize), 1 % 16),
              ((x - windowX / viewPortScale, y + halfCharacterSize), 3 % 16),
              ((x - windowX / viewPortScale, -y - halfCharacterSize), 1 % 16),
              ((x + halfCharacterSize, y + halfCharacterSize), 3 % 16),
              ((x + halfCharacterSize, y - windowY / viewPortScale), 2 % 16),
              ((x + halfCharacterSize, -y - halfCharacterSize), 1 % 16)
            ]
        vx <- getRandomR ((vBubbleMax / 4) P.* (-1, 1)) -- Why Quarters? What is vBubbleMax then for?
        vy <- getRandomR ((vBubbleMax / 4) P.* (-1, 1))
        pure $ Object {position = position, velocity = (vx, vy)}
      Balloon -> do
        position <-
          uniform
            [ (-x - halfCharacterSize, -y - halfCharacterSize),
              (x - windowX / viewPortScale, -y - halfCharacterSize),
              (x + halfCharacterSize, -y - halfCharacterSize)
            ]
        vx <- getRandomR ((vBalloonMax / 4) P.* (-1, 1))
        pure $ Object {position = position, velocity = (vx, vBalloonMax)}
    pure (objectType, object)
    where
      halfWindowX = windowX / 2
      halfWindowY = windowY / 2
      maxX = max (halfWindowX / viewPortScale * 3) (maxJumpDistance vPlainCharacterMax / 1000)
      maxY = max (halfWindowY / viewPortScale * 3) (maxJumpDistance vPlainCharacterMax / 1000)
      halfCharacterSize = 64
