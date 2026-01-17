{-# LANGUAGE RecordWildCards #-}

module Model (
  CharacterStatus (..),
  Jump (..),
  Object (..),
  World (..),
  Assets (..),
  characterInBalloon,
  characterInBubble,
  initialWorld,
  objectDataToPicture,
)
where

import Graphics.Gloss (Picture (Pictures), Point, Vector, translate)
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

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
    viewport :: !Object,
    jump :: !(Maybe Jump),
    pressedKeys :: ![SpecialKey],
    objects :: ![(ObjectType, Object)],
    assets :: !Assets
  }

objectDataToPicture :: Assets -> (ObjectType, Object) -> Picture
objectDataToPicture Assets {..} (oType, Object {..}) = uncurry translate position $ case oType of
  Bubble -> bubble
  _ -> undefined

initialWorld :: Assets -> World
initialWorld assets =
  World
    { character = Object (0, 0) (0, 0),
      characterStatus = CharacterInBubble 5,
      viewport = Object (0, 0) (0, 0),
      jump = Nothing,
      pressedKeys = [],
      objects = [(Bubble, Object {position = (80, 40), velocity = (0, 0)})],
      assets = assets
    }
