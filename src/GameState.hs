{-# LANGUAGE RankNTypes #-}

module GameState where

import System.Random
import Data.List

data Letter = Letter {
    letter :: Char,
    disabled :: Bool
}

instance Show Letter where
    show l = [letter l]

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

formatGuessedWord :: String -> String -> String
formatGuessedWord target guess = unwords $ map charAsBlueString guess
    where
        charAsBlueString char = concat ["\ESC[34m", [char], "\ESC[0m"]
        charAsYellowString char = concat ["\ESC[34m", [char], "\ESC[0m"]
        charAsRedString char = concat ["\ESC[34m", [char], "\ESC[0m"]

instance Show (GameState rg) where
    show gs = (intercalate "\n" . concat)
        [
            ["Last words in play:"],
            concat $ replicate (maxGuesses (config gs) - length (guessedWords gs)) [
                concat $ replicate (length $ targetWord gs) "_ "
            ],
            map (formatGuessedWord (targetWord gs)) (guessedWords gs),
            [""],
            ["Letters in play:"],
            [unwords $ map show (letters gs)],
            [""],
            ["Enter your guess:"]
        ]
    