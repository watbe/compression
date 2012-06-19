-- Author: Wayne Tsai u5027622@anu.edu.au
{-# LANGUAGE BangPatterns #-}

module Histogram
    ( Histogram
    , create_histogram
    , empty_histogram
    , get_histogram_pairs
    )
    where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, insert, empty, assocs)

type Histogram a = Map a Int


create_histogram :: Ord a => [a] -> Histogram a -> Histogram a
create_histogram values histo = case values of
    []      -> histo
    [x]     -> increment_histogram_frequency x histo
    (x:xs)  -> create_histogram xs (increment_histogram_frequency x histo)


increment_histogram_frequency :: Ord a => a -> Histogram a -> Histogram a
increment_histogram_frequency x !histo = case lookup x histo of
    Nothing   -> insert x 1 histo
    Just !n   -> insert x (n+1) histo


empty_histogram :: Ord a => Histogram a
empty_histogram = empty

get_histogram_pairs :: Histogram a -> [(a,Int)]
get_histogram_pairs histo = assocs histo

------------------------------------ Tests -----------------------------------
--------------------- Put any tests you write below here ---------------------





------------------------------------------------------------------------------
