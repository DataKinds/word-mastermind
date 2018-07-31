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
            GS.wordListPath = "/usr/share/dict/cracklib-small"
        },
        GS.randomGen = rng
    }

checkValidity :: String -> GS.GameState rng -> Either String ()
checkValidity guess gs
    | length guess > length (GS.targetWord gs) = Left "Your word is too long!"
    | length guess < length (GS.targetWord gs) = Left "Your word is too short!"
--    | any $ map () guess = Left "Your word is too short!"
    | otherwise = Right ()

makeGuess :: String -> GS.GameState rng -> Either String (GS.GameState rng)
makeGuess guess gs = checkValidity guess gs >> Right (gs { GS.guessedWords = formatGuess guess : GS.guessedWords gs })
    where
        formatGuess :: String -> String
        formatGuess = map toUpper

gameLoop :: GS.GameState rng -> IO ()
gameLoop gs = do
    print gs
    guess <- getLine
    let gs' = makeGuess guess gs
    case gs' of
        Right gs'' -> gameLoop gs''
        Left err -> do
            putStrLn err
            gameLoop gs

main :: IO ()
main = do
    rng <- newStdGen
    let gs = mkStdGameState rng
    gameLoop gs
    
    
