-- Author: Wayne Tsai u5027622@anu.edu.au
module BWT where

import Data.Binary
import Data.List
import Test.QuickCheck

data BWTValue a
    = EOS       -- End of string
    | Value a   -- Wraps a character from the original string.
    deriving (Ord,Eq)

-- Transform a string into a string with the `special end character'
mark_end :: [a] -> [BWTValue a]
mark_end inputs = map to_BWT_value inputs ++ [EOS]

-- Helper function to make mapping easy!
to_BWT_value :: a -> BWTValue a
to_BWT_value input = Value input

-- Produce all rotations of the input string.
make_rotations :: [BWTValue a] -> [[BWTValue a]]
make_rotations inputs = add_rotations inputs []

-- Add rotations to the list of rotations recursively. Stop
-- when the first character is EOS
add_rotations :: [BWTValue a] -> [[BWTValue a]] -> [[BWTValue a]]
add_rotations (EOS:chars) rotations     = rotations ++ [(EOS:chars)]
add_rotations (char:chars) rotations    = add_rotations (chars ++ [char]) (rotations ++ [char:chars])

-- Sort the list of rotated 
sort_rotations :: Ord a => [[BWTValue a]] -> [[BWTValue a]]
sort_rotations xs = sort xs

-- Retrive the last BWTValue a from each of the sorted rotations
get_lasts :: [[BWTValue a]] -> [BWTValue a]
get_lasts []                    = []
get_lasts (rotation:rotations)  = (last rotation : get_lasts rotations)

-- Join it all together
bwt_encode :: Ord a =>  [a] -> [BWTValue a]
bwt_encode inputs = get_lasts (sort_rotations (make_rotations (mark_end inputs))) 

-- Decoding steps functions
initialise_table :: Ord a => [BWTValue a] -> [[BWTValue a]]
initialise_table []         = []
initialise_table (en:ens)   = ([en]:initialise_table ens)

-- Initialise the BWT table
make_bwt_table :: Ord a => [[BWTValue a]] -> [[BWTValue a]]
make_bwt_table input = add_columns (length input) input input

-- Add columns to the BWT table
add_columns :: Ord a => Int -> [[BWTValue a]] -> [[BWTValue a]] -> [[BWTValue a]]
add_columns 1 initial inputs        = inputs
add_columns length initial inputs   = add_columns (length - 1) initial (apply_func_over_list concatenate_BWT_string initial (sort_rotations inputs))

-- Join the columns together for rows
concatenate_BWT_string :: [BWTValue a] -> [BWTValue a] -> [BWTValue a]
concatenate_BWT_string first second = first ++ second

-- First class function for mapping a binary function. Could probably use map.
apply_func_over_list :: (a -> a -> b) -> [a] -> [a] -> [b] 
apply_func_over_list _ [] _             = []
apply_func_over_list _ _ []             = []
apply_func_over_list func (x:xs) (y:ys) = (func x y : apply_func_over_list func xs ys)

-- Remove the BWTValue from a value
remove_BWTValues :: [BWTValue a] -> [a]
remove_BWTValues []             = []
remove_BWTValues (input:inputs) = case input of
    EOS     -> remove_BWTValues inputs
    Value a -> (a:remove_BWTValues inputs)

-- Find the BWTValue entry with an EOS at the end
find_EOS_line :: [[BWTValue a]] -> [BWTValue a]
find_EOS_line (input:inputs) = case last input of
    EOS     -> input
    _       -> find_EOS_line inputs

-- Decode BWT by combining the previous functions
bwt_decode :: Ord a => [BWTValue a] -> [a]
bwt_decode encoded = remove_BWTValues (find_EOS_line (make_bwt_table (initialise_table encoded)))


------------------------------------ Tests -----------------------------------
--------------------- Put any tests you write below here ---------------------

-- Ensure losslessness
prop_lossless_bwt :: String -> Bool
prop_lossless_bwt string = bwt_decode (bwt_encode string) == string

-- Test that BWT conversion works properly
prop_bwtvalue_conversion :: String -> Bool
prop_bwtvalue_conversion string = remove_BWTValues (mark_end string) == string

-- Unit test to ensure that EOS is removed from output
test_EOS :: Bool
test_EOS = case remove_BWTValues [EOS] of
    []  -> True
    _   -> False

------------------------------------------------------------------------------
    
instance Show a => Show (BWTValue a) where
    show EOS = "@"
    show (Value x) = show x


instance Binary a => Binary (BWTValue a) where
    put EOS = putWord8 0
    put (Value x) = putWord8 1 >> put x
    get = do
        n <- getWord8
        case n of
            0 -> return EOS
            1 -> fmap Value get
            _ -> error $ "get(BWTValue a): unexpected tag: " ++ show n

instance Functor BWTValue where
    fmap _ EOS = EOS
    fmap f (Value x) = Value (f x)
