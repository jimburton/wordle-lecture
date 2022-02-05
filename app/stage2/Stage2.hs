{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-|
Module      : Stage2
Description : Stage 2 of the Wordle solver
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

The second stage of the Wordle solver, introducing the Game record.
-}
module Stage2 where

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Set as S
import           Data.Set (Set)
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro ((&), (.~), (%~), (^.), (?~))

data CharInfo = Green (Set Int)    -- ^ Char is at these indices.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)

type Guess = [(Char, CharInfo)]

data Game = Game
  { _word        :: Text       -- ^ The word to guess.
  , _numAttempts :: Int        -- ^ The number of attempts.
  , _attempts    :: [Guess]    -- ^ Previous attempts.
  , _guess       :: Maybe Text -- ^ The latest guess.
  , _done        :: Bool       -- ^ game over flag.
  , _success     :: Bool       -- ^ Game was won.
  } deriving (Show)

$(makeLenses ''Game)

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
  }

gameWithWord :: Text -> Game
gameWithWord secret = emptyGame { _word = secret }

doGuess :: Game -> Text -> Game
doGuess g attempt = let sc = score attempt (g ^. word) 
                        wn = correct sc 
                        dn = wn || (g ^. numAttempts) == 6 in
  g & numAttempts %~ (+1)
    & attempts %~ (sc:)
    & guess ?~ attempt
    & done .~ dn
    & success .~ wn 

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
