module HighScore (loadHighScores, logNewHighScore) where

import Control.Monad (unless)
import Data.List (sortOn)
import System.Directory (XdgDirectory (XdgData), doesFileExist, getXdgDirectory)
import System.FilePath ((<.>), (</>))
import System.IO (writeFile)

type HighScore = (String, Int)

ensureFileExists :: FilePath -> IO ()
ensureFileExists path = do
  fileExists <- doesFileExist path
  unless fileExists $ writeFile path ""

loadHighScores :: IO [HighScore]
loadHighScores = highScoreFile >>= readHighScores

readHighScores :: FilePath -> IO [HighScore]
readHighScores path = do
  ensureFileExists path
  fileContent <- readFile path
  let
    ls = lines fileContent
  pure [parseLine line | line <- ls, not (null line)]
  where
    parseLine :: String -> HighScore
    parseLine line =
      let
        (name, score) = break (== ',') line
      in
        (name, read (drop 2 score))

-- Kati, 40000
-- name^^score

logNewHighScore :: HighScore -> IO ()
logNewHighScore highScore = do
  path <- highScoreFile
  highScores <- loadHighScores
  let
    sortedHighScores = take 10 $ sortOn (negate . snd) (highScore : highScores)
  writeHighScores path sortedHighScores

writeHighScores :: FilePath -> [HighScore] -> IO ()
writeHighScores path scores = do
  ensureFileExists path
  let
    content = unlines [name ++ ", " ++ show score | (name, score) <- scores]
  writeFile path content

highScoreFile :: IO FilePath
highScoreFile = getXdgDirectory XdgData $ "gamejam09" </> "highscores" <.> "csv"
