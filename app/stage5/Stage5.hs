{-# LANGUAGE OverloadedStrings, TemplateHaskell, TupleSections #-}
{-|
Module      : Stage5
Description : Stage 5 of the Wordle solver
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

The fifth stage of the Wordle solver, introducing the automated
solver.
-}
module Stage5 where

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
import           Data.List (intercalate, foldl', sortBy)
import           Data.Functor ((<&>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Maybe (fromJust, listToMaybe)
import           System.IO

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
  , _blacklist :: [Text]           -- ^ Words that proved to be bad guesses.
  } deriving (Show)

$(makeLenses ''Game)

emptyGame :: Game
emptyGame = Game {
    _word = ""
  , _numAttempts = 0
  , _attempts = []
  , _guess = Nothing
  , _done = False
  , _success = False
  , _info = M.empty
  , _blacklist = []
  }

-- * Stage 5

-- | Best starting word? 
firstWord :: Text
firstWord = "SOARE"

-- | Apply the fixed first word for automated games.
firstGuess :: Game -> Game
firstGuess = flip doGuess firstWord

-- | Find words based on a number of constraints.
findWords :: [(Char, CharInfo)] -- ^ Chars that are in the words, either at an exact index or not in any of a list of indices.
          -> [Text]             -- ^ A list of words that must not be in the result. 
          -> Vector Text        -- ^ A list of words to search.
          -> Vector Text        -- ^ The matching words.
findWords inf bl =
  V.filter (\t ->
              t `notElem` bl
              && all (\(c,pos) ->
                        case pos of
                          (Green is)  -> all (\i -> T.index t i == c) (S.elems is)
                          (Yellow os) -> T.elem c t && fromJust (T.findIndex (==c) t) `S.notMember` os
                          Black       -> not $ c `T.elem` t) inf)

-- | Start a game with a random target and a solver.
solve :: Handle -> IO Game
solve h = do
  g <- initGame
  solveTurn (firstGuess g) h

-- | Start a game with a given word and a solver.
solveWithWord :: Handle -> Text -> IO Game
solveWithWord h w = solveTurn (firstGuess $ gameWithWord w) h

-- | Allow the AI solver to take guesses until the game is over.
solveTurn :: Game -> Handle -> IO Game
solveTurn g h = do
  -- drawGrid g
  if g ^. done
    then do let t = "WORD: "<>g ^. word<>", SUCCESS: "<>T.pack (show $ g ^. success)<>", GUESSES: "<>T.pack (show (g ^. numAttempts))
            TIO.hPutStrLn h t
            hFlush h
            pure g
    else do
    ht <- hint g
    case ht of
      Nothing  -> solveTurn (backtrack g) h
      (Just t) -> solveTurn (doGuess g t) h

-- | Take a step backward in the game. Used by the solver.
backtrack :: Game -> Game
backtrack g =
  case g ^. guess of
    Nothing -> g
    Just _  -> let b  = if null $ g ^. attempts then [] else head $ g ^. attempts
                   b' = if length (g ^. attempts) > 1
                        then Just (T.pack (map fst (head (tail $ g ^. attempts))))
                        else Nothing in
      endGame $ g & info %~ (\m -> foldl'
                              (\acc ((d,s),i) ->
                                  case s of -- move the info map back to previous state 
                                    Black    -> M.delete d acc
                                    Yellow j -> if S.size j == 1
                                                then M.delete d acc
                                                else M.insert d (Yellow $ S.delete i j) acc
                                    Green _  -> if any (\(d',s') -> d'==d && isGreen s') (concat $ tail $ g ^. attempts)
                                                then acc
                                                else M.delete d acc) m (zip b [0..]))
      & attempts  %~ tail
      & blacklist %~ (T.pack (map fst b):)
      & guess     .~ b'

-- * Stage 4

-- | Get all hints based on the constraints. 
hints :: Game -> IO (Vector Text)
hints g = findWords (M.toList $ g ^. info) (g ^. blacklist) <$> dict

-- | Get a single hint based on the constraints.
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- hints g
  let possibleGames = V.map (\t -> (t, doGuess g t)) hs
  reds' <- mapM (\(t,g') -> hints g' <&> (t,) . length) possibleGames
  let res = sortBy (\(_,l1) (_,l2) -> l1 `compare` l2) $ V.toList reds'
  pure $ fst <$> listToMaybe res

-- | Set the booleans that determine whether the game is over.
endGame :: Game -> Game
endGame g = let won = not (null $ g ^. attempts) && all (isGreen . snd) (head (g ^. attempts)) in
              g & success .~ won
                & done .~ (won || (g ^. numAttempts) == 6)

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
                   if attempt == ":HINT"
                     then liftIO $ do
                       showHint g
                       playGame g
                     else if T.length attempt /= 5
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

-- | Suggest some words based on the state of the game.
showHints :: Game -> IO ()
showHints g = hints g >>= mapM_ TIO.putStrLn

-- | Suggest a single word based on the state of the game.
showHint :: Game -> IO ()
showHint g = hint g >>= mapM_ TIO.putStrLn

doGuess :: Game -> Text -> Game
doGuess g attempt = let sc = score attempt (g ^. word) 
                        wn = correct sc 
                        dn = wn || (g ^. numAttempts) == 5 in
  g & numAttempts %~ (+1)
    & attempts %~ (sc:) 
    & guess ?~ attempt
    & done .~ dn
    & success .~ wn
    & info %~ updateMapWithAttempt sc

-- | Update the info map with new constraints.
updateMapWithAttempt :: Guess -> Map Char CharInfo -> Map Char CharInfo
updateMapWithAttempt a m =
  foldl' (\acc (d,s) ->
             case s of
               (Yellow os) -> M.insertWith
                              (\(Yellow new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Yellow o) -> Yellow (S.union o new)
                                    -- was previously Green, keep it that way and ignore the new info.
                                    o'         -> o') d (Yellow os) acc
               (Green is) -> M.insertWith
                              (\(Green new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Green o) -> Green (S.union o new)
                                    -- was previously Yellow, overwrite.
                                    _         -> Green new) d (Green is) acc
               -- chars which are incorrect
               s'          -> M.insert d s' acc) m a

-- * Stage 2

isGreen :: CharInfo -> Bool
isGreen (Green _) = True
isGreen _         = False

correct :: Guess -> Bool
correct = all (isGreen . snd)

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
