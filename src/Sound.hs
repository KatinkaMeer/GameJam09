module Sound (
  module Sound.ALUT,
  playBubblesSound,
  playBalloonPopSound,
  playBalloonInflateSound,
  playBubblePopSound,
  playStartSound,
)
where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List
import Data.Map (Map)
import Sound.ALUT
import System.IO.Unsafe
import System.Random (randomRIO)

import Data.Map qualified as M

data SoundTrack = SoundTrack
  { ioRef :: !(IORef (Maybe Sound.ALUT.Buffer)),
    fileName :: !FilePath
  }

data SoundName
  = BalloonInflate
  | BalloonPop1
  | BalloonPop2
  | BubblePop1
  | BubblePop2
  | BubblePop3
  | BubblePop4
  | BubblePop5
  | BubblePop6
  | BubblePop7
  | BubblePop8
  | Bubbles
  | StartSound
  deriving (Bounded, Enum, Eq, Ord, Show)

allSoundTracks :: Map SoundName SoundTrack
{-# NOINLINE allSoundTracks #-}
allSoundTracks =
  unsafePerformIO
    $ foldM
      ( \sounds track -> do
          ioRef <- newIORef Nothing
          pure
            $ M.insert
              track
              SoundTrack
                { ioRef = ioRef,
                  fileName = "assets/sounds/" ++ show track ++ ".wav"
                }
              sounds
      )
      M.empty
      [minBound .. maxBound]

waitUntilSoundPlaybackFinished :: Source -> IO ()
waitUntilSoundPlaybackFinished source = do
  state <- get (sourceState source)
  case state of
    Playing -> do
      threadDelay 10000 -- 10ms
      waitUntilSoundPlaybackFinished source
    _ -> return ()

playFile :: SoundName -> IO ()
playFile name = do
  mBuf <- readIORef ioRef
  buf <- case mBuf of
    Just b -> return b
    Nothing -> do
      b <- createBuffer (File fileName)
      writeIORef ioRef (Just b)
      return b
  source <- genObjectName
  buffer source $= Just buf
  Sound.ALUT.play [source]
  void $ forkIO $ do
    -- threadDelay 1000000 -- 1s
    waitUntilSoundPlaybackFinished source
    deleteObjectNames [source]
  where
    SoundTrack {ioRef, fileName} = allSoundTracks M.! name

playBubblesSound :: IO ()
playBubblesSound = playFile Bubbles

playBubblePopSound :: IO ()
playBubblePopSound =
  playRandomFile [BubblePop1 .. BubblePop8]

playBalloonInflateSound :: IO ()
playBalloonInflateSound = playFile BalloonInflate

playBalloonPopSound :: IO ()
playBalloonPopSound =
  playRandomFile [BalloonPop1 .. BalloonPop2]

playStartSound :: IO ()
playStartSound = playFile StartSound

playRandomFile :: [SoundName] -> IO ()
playRandomFile names = do
  name <- pickRandom names
  playFile name

pickRandom :: [a] -> IO a
pickRandom xs = do
  idx <- randomRIO (0, length xs - 1)
  return (xs !! idx)
