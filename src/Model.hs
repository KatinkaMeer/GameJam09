{-# LANGUAGE RecordWildCards #-}

module Model (
  CharacterStatus (..),
  Jump (..),
  Object (..),
  World (..),
  Assets (..),
  characterFloats,
  characterInBalloon,
  characterInBubble,
  initialWorld,
  objectDataToPicture,
)
where

import Data.Map (Map, singleton)
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
    collisionIndex :: [Integer],
    viewport :: !Object,
    jump :: !(Maybe Jump),
    pressedKeys :: ![SpecialKey],
    objects :: !(Map Integer (ObjectType, Object)),
    assets :: !Assets,
    nextId :: !Integer
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
      objects = singleton initialId (Bubble, Object {position = (80, 40), velocity = (0, 0)}),
      assets = assets,
      nextId = succ initialId
    }

initialId :: Integer
initialId = 1
