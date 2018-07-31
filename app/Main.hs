module Main where

import System.Random
import Data.Char
import qualified GameState as GS

mkStdGameState :: RandomGen rng => rng -> GS.GameState rng
mkStdGameState rng = 
    GS.GameState {
        GS.targetWord = "WORD",
        GS.guessedWords = [],
        GS.letters = [],
        GS.config = GS.GameConfig {
            GS.maxGuesses = 6,
            GS.wordListPath = "/usr/share/dict/words"
        },
        GS.randomGen = rng
    }

makeGuess :: String -> GS.GameState rng -> GS.GameState rng
makeGuess guess gs = gs
    where
        formatGuess :: String -> String
        formatGuess = map toUpper  

main :: IO ()
main = do
    rng <- newStdGen
    let gs = mkStdGameState rng
    guess <- getLine
    print gs
    
