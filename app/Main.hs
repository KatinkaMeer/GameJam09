{-# LANGUAGE LexicalNegation #-}

module Main where

import Graphics.Gloss
import Model
import View
import Controller

main :: IO ()
main =
  play
    (InWindow "GlossyGaming" (500, 500) (10, 10))
    white
    60
    initialWorld
    render
    handleInput
    update
