{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Stage1
Description : Stage 1 of the Wordle solver
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

The first stage of the Wordle solver, containing the initial data type
and a function for scoring guesses against a secret word.
-}
module Stage1 where

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Set as S
import           Data.Set (Set)

-- * Stage 1

data CharInfo = Green (Set Int)    -- ^ Char is at these indices.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)

type Guess = [(Char, CharInfo)]

score :: Text  -- ^ The attempt.
      -> Text  -- ^ The target word.
      -> Guess -- ^ The scored attempt.
score attempt target = 
  zipWith (\(c,d) i -> if c==d 
                       then (c, Green (S.singleton i))
                       else if T.elem c target
                            then (c, Yellow (S.singleton i))
                            else (c, Black))  (T.zip attempt target) [0..]
