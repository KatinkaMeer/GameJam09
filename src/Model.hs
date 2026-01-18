module Model (
  CharacterStatus (..),
  GlobalState (..),
  Jump (..),
  Object (..),
  Screen (..),
  UiState (..),
  World (..),
  Assets (..),
  ObjectType (..),
  characterFloats,
  characterInBalloon,
  characterInBubble,
  initialGlobalState,
  initialWorld,
)
where

import Data.Map (Map)
import Graphics.Gloss (Picture (Pictures), Point, Vector, translate)
import Graphics.Gloss.Data.ViewPort (ViewPort (ViewPort, viewPortRotate, viewPortScale, viewPortTranslate))
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

import Data.Map qualified as M

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
            pressedKeys = [],
            windowSize = (800, 450)
          },
      screen = StartScreen
    }

data UiState = UiState
  { assets :: !Assets,
    pressedKeys :: ![SpecialKey],
    windowSize :: !Vector
  }

data Screen
  = StartScreen
  | GameScreen !World
  | HighScoreScreen !(Maybe Integer) !(Maybe Integer)

data Object = Object
  { position :: !Point,
    velocity :: !Vector
  }
  deriving (Eq, Show)

data ObjectType = Balloon | Bubble deriving Eq

data Jump
  = Jump
      { direction :: !Vector,
        speed :: !Float
      }
  | InitJump
      { mousePoint :: !Point
      }

data CharacterStatus
  = CharacterAtBalloon !Float
  | CharacterInBubble !Float
  | PlainCharacter !Float
  deriving Eq

characterInBubble :: Float -> CharacterStatus
characterInBubble t
  | t <= 0 = PlainCharacter 0
  | otherwise = CharacterInBubble t

characterInBalloon :: Float -> CharacterStatus
characterInBalloon t
  | t <= 0 = PlainCharacter 0
  | otherwise = CharacterAtBalloon t

characterFloats :: CharacterStatus -> Bool
characterFloats (CharacterAtBalloon _) = True
characterFloats (CharacterInBubble _) = True
characterFloats _ = False

data Assets = Assets
  { player :: !Picture,
    bubble :: !Picture,
    bubbleTimerAttention :: !Picture,
    bubbleTimerDanger :: !Picture,
    frogBodyRight :: !Picture,
    frogBodyLeft :: !Picture,
    frogEyesOpenRight :: !Picture,
    frogEyesOpenLeft :: !Picture,
    frogEyesClosedRight :: !Picture,
    frogEyesClosedLeft :: !Picture,
    frogMouthRight :: !Picture,
    frogMouthLeft :: !Picture,
    cloud :: !Picture,
    ballonBlue :: !Picture,
    ballonGreen :: !Picture,
    ballonPink :: !Picture,
    ballonRed :: !Picture,
    ballonYellow :: !Picture,
    ground :: !Picture,
    titleScreen :: !Picture
  }

data World = World
  { character :: !Object,
    characterAltitude :: Float,
    characterStatus :: !CharacterStatus,
    collisions :: ![Integer],
    elapsedTime :: !Float,
    viewport :: !ViewPort,
    jump :: !(Maybe Jump),
    mousePosition :: !(Float, Float),
    objects :: !(Map Integer (ObjectType, Object)),
    nextId :: !Integer,
    bonusPoints :: !Integer
  }

initialWorld :: World
initialWorld =
  World
    { character = Object (0, 0) (0, 0),
      characterAltitude = 0,
      characterStatus = CharacterInBubble 5,
      collisions = [],
      elapsedTime = 0,
      viewport =
        ViewPort
          { viewPortTranslate = (0.0, 0.0),
            viewPortRotate = 0,
            viewPortScale = 1.0
          },
      jump = Nothing,
      mousePosition = (0, 0),
      objects = M.singleton 1 (Bubble, Object {position = (80, 40), velocity = (50, 50)}),
      nextId = 2,
      bonusPoints = 0
    }
