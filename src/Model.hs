module Model (
  Jump (..),
  Object (..),
  World (..),
  Assets (..),
  initialWorld,
) where

import Graphics.Gloss (Point, Vector, Picture)
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

data MovingObject

data Assets = Assets
 {  player :: Picture,
    bubble :: Picture
  }

data World = World
  { character :: !Object,
    characterInBubble :: !Bool,
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
      characterInBubble = True,
      viewport = Object (0, 0) (0, 0),
      jump = Nothing,
      pressedKeys = [],
      objects = [],
      assets = assets
    }
