module Sound (
  module Sound.ALUT,
  playBubblesSound,
  playBalloonPopSound,
  playBalloonInflateSound,
  playBubblePopSound,
)
where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List
import Sound.ALUT
import System.IO.Unsafe
import System.Random (randomRIO)

soundBuffer :: IORef (Maybe Sound.ALUT.Buffer)
{-# NOINLINE soundBuffer #-}
soundBuffer = unsafePerformIO $ newIORef Nothing

waitUntilSoundPlaybackFinished :: Source -> IO ()
waitUntilSoundPlaybackFinished source = do
  state <- get (sourceState source)
  case state of
    Playing -> do
      threadDelay 10000 -- 10ms
      waitUntilSoundPlaybackFinished source
    _ -> return ()

playFile :: FilePath -> IO ()
playFile fileName = do
  mBuf <- readIORef soundBuffer
  buf <- case mBuf of
    Just b -> return b
    Nothing -> do
      b <- createBuffer (File fileName)
      writeIORef soundBuffer (Just b)
      return b
  source <- genObjectName
  buffer source $= Just buf
  Sound.ALUT.play [source]
  void $ forkIO $ do
    -- threadDelay 1000000 -- 1s
    waitUntilSoundPlaybackFinished source
    deleteObjectNames [source]

playBubblesSound :: IO ()
playBubblesSound = playFile "assets/sounds/bubbles.wav"

playBubblePopSound :: IO ()
playBubblePopSound =
  playRandomFile
    [ "assets/sounds/bubble-pop1.wav",
      "assets/sounds/bubble-pop-2.wav",
      "assets/sounds/bubble-pop-3.wav",
      "assets/sounds/bubble-pop-4.wav",
      "assets/sounds/bubble-pop-5.wav",
      "assets/sounds/bubble-pop-6.wav",
      "assets/sounds/bubble-pop-7.wav",
      "assets/sounds/bubble-pop-8.wav"
    ]

playBalloonInflateSound :: IO ()
playBalloonInflateSound = playFile "assets/sounds/inflate.wav"

playBalloonPopSound :: IO ()
playBalloonPopSound =
  playRandomFile
    [ "assets/sounds/balloon-pop-1.wav",
      "assets/sounds/balloon-pop-2.wav"
    ]

playRandomFile :: [FilePath] -> IO ()
playRandomFile fileNames = do
  fileName <- pickRandom fileNames
  playFile fileName

pickRandom :: [a] -> IO a
pickRandom xs = do
  idx <- randomRIO (0, length xs - 1)
  return (xs !! idx)
