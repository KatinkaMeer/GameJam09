module Model (
  CharacterStatus (..),
  Jump (..),
  Object (..),
  World (..),
  Assets (..),
  initialWorld,
)
where

import Graphics.Gloss (Picture, Point, Vector)
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

data Object = Object
  { position :: !Point,
    velocity :: !Vector
  }

data ObjectType = Balloon | Bubble

data Jump = Jump
  { direction :: !Vector,
    speed :: !Float
  }

data CharacterStatus
  = CharacterInBalloon
  | CharacterInBubble
  | PlainCharacter

data Assets = Assets
  { player :: !Picture,
    bubble :: !Picture
  }

data World = World
  { character :: !Object,
    characterStatus :: !CharacterStatus,
    viewport :: !Object,
    jump :: !(Maybe Jump),
    pressedKeys :: ![SpecialKey],
    objects :: ![(ObjectType, Object)],
    assets :: !Assets
  }

initialWorld :: Assets -> World
initialWorld assets =
  World
    { character = Object (0, 0) (0, 0),
      characterStatus = CharacterInBubble,
      viewport = Object (0, 0) (0, 0),
      jump = Nothing,
      pressedKeys = [],
      objects = [],
      assets = assets
    }
