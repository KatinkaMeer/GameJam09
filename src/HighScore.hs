module HighScore (loadHighScores, logNewHighScore, readMaxAltitude, updateMaxAltitude,
 showHighScore) where

import Control.Monad (unless)
import Data.Bifunctor (second)
import Data.List (sortOn)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (dropFileName, (<.>), (</>))
import System.IO (writeFile)

import Data.ByteString.Char8 qualified as BS

type HighScore = (String, (Integer, Integer))

ensureFileExists :: FilePath -> IO ()
ensureFileExists path = do
  createDirectoryIfMissing True (dropFileName path)
  fileExists <- doesFileExist path
  unless fileExists $ BS.writeFile path (BS.pack "")

loadHighScores :: IO [HighScore]
loadHighScores = highScoreFile >>= readHighScores

showHighScore :: HighScore -> String
showHighScore (name, (points, altitude)) =
    name ++ ": " ++ show points ++ " points, " ++ show altitude ++ " meters"

readHighScores :: FilePath -> IO [HighScore]
readHighScores path = do
  ensureFileExists path
  fileContent <- BS.unpack <$> BS.readFile path
  let
    ls = lines fileContent
  pure [parseLine line | line <- ls, not (null line)]
  where
    parseLine :: String -> HighScore
    parseLine line = (name, (read points, read altitude))
      where
        (name, score) = second (drop 2) $ break (== ',') line
        (points, altitude) = second (drop 2) $ break (== ',') score

-- Example line:
-- Kati, 40000, 799089
-- name^^score^^meters

logNewHighScore :: HighScore -> IO ()
logNewHighScore highScore = do
  path <- highScoreFile
  highScores <- loadHighScores
  let
    sortedHighScores = take 10 $ sortOn (negate . fst . snd) (highScore : highScores)
  writeHighScores path sortedHighScores

writeHighScores :: FilePath -> [HighScore] -> IO ()
writeHighScores path scores = do
  ensureFileExists path
  let
    content = unlines [name ++ ", " ++ show points ++ ", " ++ show altitude | (name, (points, altitude)) <- scores]
  BS.writeFile path $ BS.pack content

highScoreFile :: IO FilePath
highScoreFile = getXdgDirectory XdgData $ "gamejam09" </> "highscores" <.> "csv"

maxAltitudeFile :: IO FilePath
maxAltitudeFile = getXdgDirectory XdgData $ "gamejam09" </> "max_altitude" <.> "txt"

readMaxAltitude :: IO Integer
readMaxAltitude = do
  path <- maxAltitudeFile
  ensureFileExists path
  content <- BS.unpack <$> BS.readFile path
  if null content
    then pure 0
    else pure (read content)

updateMaxAltitude :: Integer -> IO ()
updateMaxAltitude altitude = do
  path <- maxAltitudeFile
  writeFile path (show altitude)
