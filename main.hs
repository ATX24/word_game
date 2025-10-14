module Main where

import System.IO (hFlush, stdout)
import Data.Char (toLower)

-- The game state: word to guess, guessed letters, and remaining attempts
data Game = Game {
    word :: String,
    guessed :: [Char],
    attemptsLeft :: Int
} deriving (Show)

-- Display the current state of the word with underscores for unguessed letters
displayWord :: Game -> String
displayWord (Game w g _) =
    [if toLower c `elem` g then c else '_' | c <- w]

-- Check if the game is won
isWon :: Game -> Bool
isWon (Game w g _) = all (`elem` g) (map toLower w)

-- Check if the game is lost
isLost :: Game -> Bool
isLost (Game _ _ a) = a <= 0

-- Make a guess and update the game state
makeGuess :: Game -> Char -> Game
makeGuess (Game w g a) c
    | toLower c `elem` w = Game w (toLower c : g) a
    | otherwise          = Game w g (a - 1)

-- Game loop
gameLoop :: Game -> IO ()
gameLoop gametg
    | isWon game = putStrLn $ "You won! The word was: " ++ word game
    | isLost game = putStrLn $ "Game over! The word was: " ++ word game
    | otherwise = do
        putStrLn $ "\nWord: " ++ displayWord game
        putStrLn $ "Attempts left: " ++ show (attemptsLeft game)
        putStr "Guess a letter: "
        hFlush stdout
        guess <- getLine
        if null guess
            then gameLoop game
            else gameLoop (makeGuess game (head guess))

main :: IO ()
main = do
    putStrLn "Welcome to the Haskell Word Guessing Game!"
    let secretWord = "Haskell"
    let initialGame = Game secretWord [] 6
    gameLoop initialGame