{-# LANGUAGE RankNTypes #-}

module GameState where

import System.Random
import Data.List
import Debug.Trace
import qualified Data.Map.Strict as Map

data Letter = Letter {
    letter :: Char,
    disabled :: Bool
}

instance Show Letter where
    show Letter { letter = l, disabled = True } = concat ["\ESC[38;5;234m", [l], "\ESC[0m"]
    show Letter { letter = l, disabled = False } = [l]

data GameConfig = GameConfig {
    maxGuesses :: Int,
    wordList :: [String]
}

data GameState rng = GameState {
    targetWord :: String,
    guessedWords :: [String],
    letters :: Map.Map Char Letter,
    config :: GameConfig,
    randomGen :: (Random rng) => rng
}

-- THIS FUNCTION IS PARTIAL
formatGuessedWord :: String -> String -> String
formatGuessedWord target guess = unwords $ zipWith (curry charIndexToColor) guess [0..]
    where
        charAsBlueString char = concat ["\ESC[34m", [char], "\ESC[0m"]
        charAsYellowString char = concat ["\ESC[33m", [char], "\ESC[0m"]
        charAsGreenString char = concat ["\ESC[32m", [char], "\ESC[0m"]

        charIndexToColor :: (Char, Int) -> String
        charIndexToColor (char, index)
            | (target !! index) == char = charAsGreenString char
            | char `elem` target = charAsYellowString char
            | otherwise = charAsBlueString char

instance Show (GameState rg) where
    show gs = (intercalate "\n" . concat)
        [
            ["Last words in play:"],
            concat $ replicate (maxGuesses (config gs) - length (guessedWords gs)) [
                concat $ replicate (length $ targetWord gs) "_ "
            ],
            reverse $ map (formatGuessedWord (targetWord gs)) (guessedWords gs),
            [""],
            ["Letters in play:"],
            [unwords $ Map.elems $ fmap show (letters gs)],
            [""],
            ["Enter your guess:"]
        ]
    