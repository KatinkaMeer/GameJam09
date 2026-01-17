{-# LANGUAGE RecordWildCards #-}

module Model (
  CharacterStatus (..),
  GlobalState (..),
  Jump (..),
  Object (..),
  Screen (..),
  UiState (..),
  World (..),
  Assets (..),
  characterFloats,
  characterInBalloon,
  characterInBubble,
  initialGlobalState,
  initialWorld,
  objectDataToPicture,
)
where

import Graphics.Gloss (Picture (Pictures), Point, Vector, translate)
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

data GlobalState = GlobalState
  { uiState :: !UiState,
    screen :: !Screen
  }

initialGlobalState
  :: Assets
  -- ^ Pre-rendered pictures
  -> GlobalState
initialGlobalState assets =
  GlobalState
    { uiState =
        UiState
          { assets = assets,
            highScores = HighScores [],
            pressedKeys = [],
            windowSize = (800, 450)
          },
      screen = StartScreen
    }

data UiState = UiState
  { assets :: !Assets,
    highScores :: !HighScores,
    pressedKeys :: ![SpecialKey],
    windowSize :: !Vector
  }

data Screen
  = StartScreen
  | GameScreen !World
  | HighScoreScreen

newtype HighScores = HighScores
  { unHighScores :: [(String, Integer)]
  }

data Object = Object
  { position :: !Point,
    velocity :: !Vector
  }
  deriving Show

data ObjectType = Balloon | Bubble

data Jump
  = Jump
      { direction :: !Vector,
        speed :: !Float
      }
  | InitJump
      { mousePoint :: !Point
      }

data CharacterStatus
  = CharacterInBalloon Float
  | CharacterInBubble Float
  | PlainCharacter
  deriving Eq

characterInBubble :: Float -> CharacterStatus
characterInBubble t
  | t <= 0 = PlainCharacter
  | otherwise = CharacterInBubble t

characterInBalloon :: Float -> CharacterStatus
characterInBalloon t
  | t <= 0 = PlainCharacter
  | otherwise = CharacterInBalloon t

characterFloats :: CharacterStatus -> Bool
characterFloats (CharacterInBalloon _) = True
characterFloats (CharacterInBubble _) = True
characterFloats _ = False

data Assets = Assets
  { player :: !Picture,
    bubble :: !Picture,
    frogBody :: !Picture,
    frogEyesOpen :: !Picture,
    frogEyesClosed :: !Picture,
    frogMouth :: !Picture,
    cloud :: Picture
  }

data World = World
  { character :: !Object,
    characterStatus :: !CharacterStatus,
    collisionIndex :: !(Maybe Int),
    elapsedTime :: !Float,
    viewport :: !Object,
    jump :: !(Maybe Jump),
    objects :: ![(ObjectType, Object)]
  }

objectDataToPicture :: Assets -> (ObjectType, Object) -> Picture
objectDataToPicture Assets {..} (oType, Object {..}) = uncurry translate position $ case oType of
  Bubble -> bubble
  _ -> undefined

initialWorld :: World
initialWorld =
  World
    { character = Object (0, 0) (0, 0),
      characterStatus = CharacterInBubble 5,
      collisionIndex = Nothing,
      elapsedTime = 0,
      viewport = Object (0, 0) (0, 0),
      jump = Nothing,
      objects = [(Bubble, Object {position = (80, 40), velocity = (0, 0)})]
    }
