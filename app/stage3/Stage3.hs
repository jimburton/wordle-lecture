{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-|
Module      : Stage3
Description : Stage 3 of the Wordle solver
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

The third stage of the Wordle solver, containing the first version of
an interactive game.
-}
module Stage3 where

import           System.Console.Haskeline
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Control.Monad.IO.Class (liftIO)
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro ((&), (.~), (%~), (^.), (?~))
import           System.Random (getStdRandom, randomR)
import           Data.List (intercalate, foldl')
import           Data.Functor ((<&>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Maybe (fromJust, listToMaybe)

data CharInfo = Green (Set Int)    -- ^ Char is at these indices.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)

type Guess = [(Char, CharInfo)]

data Game = Game
  { _word     :: Text              -- ^ The word to guess.
  , _numAttempts :: Int            -- ^ The number of attempts.
  , _attempts :: [Guess]           -- ^ Previous attempts.
  , _info     :: Map Char CharInfo -- ^ Info on previous guesses.
  , _guess    :: Maybe Text        -- ^ The latest guess.
  , _done     :: Bool              -- ^ game over flag.
  , _success  :: Bool              -- ^ Game was won.
  } deriving (Show)

$(makeLenses ''Game)

-- * Stage 3

filepathToDict :: FilePath -> IO (Vector Text)
filepathToDict fp = V.map T.toUpper . V.fromList . T.lines <$> TIO.readFile fp
  
dict :: IO (Vector Text)
dict = filepathToDict "etc/long.txt"

targets :: IO (Vector Text)
targets = filepathToDict "etc/short.txt"

-- | Get a word to be the target for a game.
getTarget :: IO Text
getTarget = do
  flw <- targets
  getStdRandom (randomR (0, length flw)) <&> (V.!) flw

initGame :: IO Game
initGame = getTarget <&> gameWithWord

-- | Print a message when the game is over.
gameOver :: Game -> IO ()
gameOver g = if g ^. success
             then TIO.putStrLn "Well done!"
             else TIO.putStrLn $ "Hard luck! The word was " <> (g ^. word)

-- | Draw the game grid.
drawGrid :: Game -> IO ()
drawGrid g = do
  let as = reverse $ g ^. attempts
  TIO.putStrLn hline
  drawLines as 0
  where hline  = "+-------------------+"
        iline  = "|   |   |   |   |   |"
        line a = T.pack $ "|" <> intercalate "|"
          (map (\(c,i) -> " "<>colour i<>[c]<>def<>" ") a) <> "|"
        colour (Green _)  = green
        colour Black      = red
        colour (Yellow _) = yellow
        drawLines _  6 = pure ()
        drawLines as n = do if n < length as
                              then TIO.putStrLn $ line (as!!n)
                              else TIO.putStrLn iline
                            TIO.putStrLn hline 
                            drawLines as (n+1)
        red    = "\ESC[31m"
        green  = "\ESC[32m"
        yellow = "\ESC[33m"
        def    = "\ESC[0m"

-- | Explain the colours to the user.
helpText :: Text
helpText = "\ESC[32mchar in right place.\ESC[0m\n"
           <> "\ESC[33mchar in word but wrong place.\ESC[0m\n"
           <> "\ESC[31mchar not in word.\ESC[0m"

-- | Play the game by querying the user for words until they guess the word or have
-- | used their six guesses.
playGame :: Game -> IO ()
playGame g = runInputT defaultSettings loop
 where
   loop :: InputT IO ()
   loop = do
     if g ^. done
       then liftIO $ gameOver g
       else do mLn <- getInputLine ("Enter a five letter word [Attempt "
                                    <> show (1 + length (g ^. attempts))
                                    <> "]\n")
               case mLn of
                 Nothing -> loop
                 Just wdStr -> do
                   let attempt = T.toUpper $ T.pack wdStr
                   if T.length attempt /= 5
                     then liftIO $ do
                     TIO.putStrLn "Not a five letter word"
                     playGame g
                     else do
                     dw <- liftIO $ isDictWord attempt
                     if not dw
                       then liftIO $ do
                       TIO.putStrLn "Not a word from the dictionary"
                       playGame g
                       else liftIO $ do
                       let g' = doGuess g attempt
                       drawGrid g'
                       playGame g'

-- | Is a word in the dictionary?
isDictWord :: Text -> IO Bool
isDictWord t = dict <&> elem t 

doGuess :: Game -> Text -> Game
doGuess g attempt = let sc = score attempt (g ^. word) 
                        wn = correct sc 
                        dn = wn || (g ^. numAttempts) == 5 in
  g & numAttempts %~ (+1)
    & attempts %~ (sc:) 
    & guess ?~ attempt
    & done .~ dn
    & success .~ wn

-- * Stage 2

isGreen :: CharInfo -> Bool
isGreen (Green _) = True
isGreen _         = False

correct :: Guess -> Bool
correct = all (isGreen . snd)

emptyGame :: Game
emptyGame = Game {
    _word = ""
  , _numAttempts = 0
  , _attempts = []
  , _guess = Nothing
  , _done = False
  , _success = False
  , _info = M.empty
  }

gameWithWord :: Text -> Game
gameWithWord secret = emptyGame { _word = secret }

-- * Stage 1

score :: Text  -- ^ The attempt.
      -> Text  -- ^ The target word.
      -> Guess -- ^ The scored attempt.
score attempt target = 
  zipWith (\(c,d) i -> if c==d 
                       then (c, Green (S.singleton i))
                       else if T.elem c target
                            then (c, Yellow (S.singleton i))
                            else (c, Black))  (T.zip attempt target) [0..]
