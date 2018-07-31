{-# LANGUAGE RankNTypes #-}

module GameState where

import System.Random

data Letter = Letter {
    letter :: String,
    disabled :: Bool
}

instance Show Letter where
    show = letter

data GameConfig = GameConfig {
    maxGuesses :: Int,
    wordListPath :: String
}

data GameState rng = GameState {
    targetWord :: String,
    guessedWords :: [String],
    letters :: [Letter],
    config :: GameConfig,
    randomGen :: (RandomGen rng) => rng
}

instance Show (GameState rg) where
    show gs = (unlines . concat)
        [
            ["Last words in play:"],
            concat $ replicate ((maxGuesses $ config gs) - (length $ guessedWords gs)) [
                concat $ replicate (length $ targetWord gs) "_ "
            ],
            guessedWords gs,
            [""],
            ["Letters in play:"],
            [unwords $ map show (letters gs)],
            [""],
            ["Enter your guess:"]
        ]
    