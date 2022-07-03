module Main where

import System.Console.ANSI
import System.IO
import System.Random
import Data.String (IsString)
import qualified Data.Char as C
import Data.List
import System.Directory
import Control.Monad.State
import Data.Char (Char)

-- Fixed number of how many guesses in total
totalGuesses = 8

-- Filename of the file containing the words
wordFileName = "words.txt"

type SecretWord = String
type GuessedLetters = String
type IncorrectGuesses = Int

data GameState = GameState
 { 
   secret :: SecretWord,
   guessed :: GuessedLetters,
   incorrectGuesses :: IncorrectGuesses
 }

main :: IO ()
main = do
  showIntroScreen
  promptForGameType

showIntroScreen :: IO ()
showIntroScreen = do
  clearScreen
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "-------------------"
  putStrLn "Welcome to Hangman!"
  putStrLn "-------------------"
  setSGR [Reset]

promptForGameType :: IO ()
promptForGameType = do
    putStrLn "1. Enter a word."
    putStrLn "2. Select a random word."
    putStrLn "3. Quit."
    putStrLn "Press 1, 2 or 3."
    hFlush stdout
    hSetBuffering stdin NoBuffering 
    hSetEcho stdin False
    a <- C.toUpper <$> getChar
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering 
    case a of
        '1'  -> promptForUserWord
        '2'  -> loadRandomWord
        '3'  -> return ()
        _    -> promptForGameType

promptForUserWord :: IO ()
promptForUserWord = do
  putStrLn "Enter a word at least 3 letters long then press Enter (input is hidden)."
  hFlush stdout
  -- Turn off echo
  hSetEcho stdin False
  -- Get input from user
  w <- getLine
  -- Turn echo back on
  hSetEcho stdin True
  if length w < 3 then
    promptForUserWord
  else do
    startState <- getInitialState w
    evalStateT startGame startState

loadRandomWord :: IO ()
loadRandomWord = do
  currentDir <- getCurrentDirectory
  let filePath = currentDir ++ "/" ++ wordFileName
  fileExists <- doesFileExist filePath
  if fileExists then do
    -- Read the entire file
    fileContents <- readFile filePath
    -- Get each line from the file
    let linesOfWords = lines fileContents
    let iLength = length linesOfWords
    -- Get a random index in the valid range
    iRandomIndex <-randomRIO (0, iLength -1)
    -- Set the random word
    let sWord = linesOfWords !! iRandomIndex
    putStrLn $ "A random word from " ++ show iLength ++ " available words has been selected."
    startState <- getInitialState sWord
    evalStateT startGame startState
  else do
    putStrLn $ "Word file was not found: " ++ filePath

getInitialState :: SecretWord -> IO GameState
getInitialState s = do
   return $ GameState { secret = map C.toUpper s, guessed = "",  incorrectGuesses = 0 }

startGame :: StateT GameState IO ()
startGame = do
  liftIO (hSetBuffering stdin NoBuffering)
  showMaskedWord
  makeGuess

makeGuess :: StateT GameState IO ()
makeGuess = do
  progress <- get
  liftIO (putStrLn "Guess a letter or '?' to quit:")
  liftIO (hFlush stdout)
  liftIO (hSetEcho stdin False)
  c <- C.toUpper <$> liftIO getChar
  liftIO (hSetEcho stdin True)

  if c == '?' then
    return ()
  else do
    game <- get
    let guessesremaining = totalGuesses - incorrectGuesses game

    -- Check if the guess is valid
    let validationMessage = getInputValidation c (guessed game)

    liftIO clearScreen

    if validationMessage /= "" then do
      -- Do not increment the incorrect guess count, just ask again for input
      showMaskedWord
      drawHangman
      liftIO (hFlush stdout)
      liftIO (putStrLn validationMessage)
      makeGuess
    else do
      let newguess = c:[] ++ guessed game
      put $ game { guessed = newguess}

      showMaskedWord

      let inWord = c `elem` secret game
      if inWord then do
        -- The letter is in the word
        if wordFound newguess (secret game) then do
          showGameWin
        else do
          drawHangman
          makeGuess
      else do
        --The letter is not in the word
        let iRemaining = guessesremaining - 1
        put $ game { incorrectGuesses = incorrectGuesses game + 1, guessed = newguess}
        drawHangman
        if iRemaining == 0 then
          do
            showGameOver
            else
              do
                liftIO (putStrLn $ "The letter " ++ show c ++ " is not in the word. " ++ getGuessRemainingText iRemaining)
                makeGuess

getGuessRemainingText :: Int -> String
getGuessRemainingText 1 = "1 guess remaining."
getGuessRemainingText x = show x ++ " guesses remaining."

showGameWin :: StateT GameState IO ()
showGameWin = do
  game <- get
  liftIO (setSGR [SetColor Foreground Vivid Green])
  liftIO (putStrLn $ "Congratulations, you guessed the word '" ++ secret game ++ "'!")
  liftIO (setSGR [Reset])

showGameOver :: StateT GameState IO ()
showGameOver = do
  game <- get
  liftIO (setSGR [SetColor Foreground Vivid Red])
  liftIO (putStrLn "GAME OVER!")
  liftIO (putStrLn ("The word was: " ++ show (secret game)))
  liftIO (setSGR [Reset])

getInputValidation :: Char -> GuessedLetters -> String
getInputValidation a guessed = do
  if not (C.isLetter a) then
    "Please enter a letter."
  else do
     if a `elem` guessed then
       "You have already guessed the letter " ++ show a
      else
         ""

showMaskedWord :: StateT GameState IO ()
showMaskedWord = do
  game <- get
  let currentGuessed = guessed game
  let currentSecret = secret game
  liftIO (putStrLn "\n")
  liftIO (setSGR [SetColor Foreground Vivid Blue])
  -- Get the masked word and add spaces
  let sMasked = intersperse ' ' (getMaskedWord currentGuessed currentSecret)
  liftIO (putStrLn sMasked)
  liftIO (putStrLn $ show (length currentSecret) ++ " letter word")
  liftIO (setSGR [Reset])
  liftIO (putStrLn " ")

getMaskedWord :: GuessedLetters -> SecretWord -> String
getMaskedWord guessed secret =
  [if x `elem` guessed then x else '_' | x <- secret]

wordFound :: GuessedLetters -> SecretWord -> Bool
wordFound guess secret = all (`elem` guess) secret

drawHangman :: StateT GameState IO ()
drawHangman = do
   game <- get
   liftIO (setSGR [SetColor Foreground Vivid Yellow])
   liftIO (putStrLn $ getHangman (incorrectGuesses game))
   liftIO (setSGR [Reset])

getHangman :: IncorrectGuesses -> String
getHangman guesses = case guesses of
  0 -> ""
  1 -> "--------------|\n" ++
       "              |\n" ++
       "              |\n" ++
       "              |\n" ++
       "              |\n"
  2 -> "--------------|\n" ++
       "              |\n" ++
       "   O          |\n" ++
       "              |\n" ++
       "              |\n"
  3 -> "--------------|\n" ++
       "              |\n" ++
       "   O          |\n" ++
       "   |          |\n" ++
       "              |\n"
  4 -> "--------------|\n" ++
       "              |\n" ++
       "   O          |\n" ++
       "  /|          |\n" ++
       "              |\n"
  5 -> "--------------|\n" ++
       "              |\n" ++
       "   O          |\n" ++
       "  /|\\         |\n" ++
       "              |\n"
  6 -> "--------------|\n" ++
       "              |\n" ++
       "   O          |\n" ++
       "  /|\\         |\n" ++
       "  /           |\n"
  7 -> "--------------|\n" ++
       "              |\n" ++
       "   O          |\n" ++
       "  /|\\         |\n" ++
       "  / \\         |\n"
  8->  "--------------|\n" ++
       "   |          |\n" ++
       "   O          |\n" ++
       "  /|\\         |\n" ++
       "  / \\         |\n"