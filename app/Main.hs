module Main where

import System.Random
import Data.Char
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified GameState as GS


mkStdGameState :: IO (GS.GameState StdGen)
mkStdGameState = do
    rng <- newStdGen
    wordList <- readFile "/usr/share/dict/words"
    let prunedList = pruneWordList [6] $ lines wordList
    let (wordIndex, rng') = randomR (0, length prunedList - 1) rng

    return GS.GameState {
        GS.targetWord = map toUpper (prunedList !! wordIndex),
        GS.guessedWords = [],
        GS.letters = Map.fromList $ zip ['A'..'Z'] [GS.Letter { GS.letter = l, GS.disabled = False } | l <- ['A' .. 'Z']],
        GS.config = GS.GameConfig {
            GS.maxGuesses = 10,
            GS.wordList = prunedList
        },
        GS.randomGen = rng'
    }
    where
        pruneWordList :: [Int] -> [String] -> [String]
        pruneWordList wordLengths wl = filter (\word -> length word `elem` wordLengths) $ (map . map) toUpper $ filter (all (\c -> c `elem` ['a'..'z'])) wl

isValidWord :: [String] -> String -> Bool
isValidWord wordList word = word `elem` wordList

isValidLetters :: Map.Map Char GS.Letter -> String -> Bool
isValidLetters letterMap word = 
    let letterString = Map.elems $ fmap GS.letter (Map.filter (not . GS.disabled) letterMap)
    in
        all (`elem` letterString) word

checkValidity :: String -> GS.GameState rng -> Either String ()
checkValidity guess gs
    | length guess > length (GS.targetWord gs) = Left "Your word is too long!"
    | length guess < length (GS.targetWord gs) = Left "Your word is too short!"
--    | not (isValidLetters (GS.letters gs) guess) = Left "Your word used letters that aren't in the letter bank!"
    | not (isValidWord (GS.wordList $ GS.config gs) guess) = Left (guess ++ " is not a word!")
    | otherwise = Right ()

makeGuess :: String -> GS.GameState rng -> Either String (GS.GameState rng)
makeGuess guess gs =
    checkValidity (formatGuess guess) gs >> Right (gs { 
        GS.guessedWords = formatGuess guess : GS.guessedWords gs, 
        GS.letters = rejectLetters (GS.letters gs) (GS.targetWord gs) guess
    })
    where
        formatGuess :: String -> String
        formatGuess = map toUpper

        disableLetters :: Map.Map Char GS.Letter -> String -> Map.Map Char GS.Letter
        disableLetters letterMap str = Map.mapWithKey (\char letter -> letter { GS.disabled = char `elem` map toUpper str }) letterMap

        rejectLetters :: Map.Map Char GS.Letter -> String -> String -> Map.Map Char GS.Letter
        rejectLetters letterMap targetWord word = 
            let lettersToReject = filter (`notElem` targetWord) word
            in
                disableLetters letterMap lettersToReject

gameLoop :: GS.GameState rng -> IO ()
gameLoop gs =
    if GS.maxGuesses (GS.config gs) < length (GS.guessedWords gs) then do
        putStrLn "\ESC[31mYou lose!"
        putStrLn ("The word was " ++ GS.targetWord gs ++ ".\ESC[0m")
    else do
        print gs
        guess <- getLine
        if map toUpper guess == GS.targetWord gs then do
            putStrLn "\ESC[32mYou win!"
            putStrLn ("It only took you " ++ show (length (GS.guessedWords gs)) ++ " guesses.\ESC[0m")
        else do
            let gs' = makeGuess guess gs
            case gs' of
                Right gs'' -> gameLoop gs''
                Left err -> do
                    putStrLn err
                    gameLoop gs

main :: IO ()
main = do
    gs <- mkStdGameState
    gameLoop gs
    
    
