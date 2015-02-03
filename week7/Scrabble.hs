{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

score :: Char -> Score
score c
    | c !> "aeilnorstu" = Score 1
    | c !> "dg" = Score 2
    | c !> "bcmp" = Score 3
    | c !> "fhvwy" = Score 4
    | c !> "k" = Score 5
    | c !> "jx" = Score 8
    | c !> "qz" = Score 10
    | otherwise = Score 0

(!>) :: Char -> [Char] -> Bool
c !> cs = elem (toLower c) cs

scoreString :: String -> Score
scoreString = mconcat . map score
