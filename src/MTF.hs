-- Author: Wayne Tsai u5027622@anu.edu.au
module MTF where

import Data.List (elemIndex, delete)
import Data.Maybe
import Test.QuickCheck
import Data.Word
import BWT

-- The first list of a's is the dictionary of values to lookup
mtf_encode :: Eq a => [a] -> [a] -> [Int]
mtf_encode _ []                 = []
mtf_encode dict [input]         = [fromJust (elemIndex input dict)]
mtf_encode dict (input:inputs)  = (fromJust (elemIndex input dict) : mtf_encode (input:(delete input (dict))) inputs)

mtf_decode :: Eq a => [a] -> [Int] -> [a]
mtf_decode _ []                 = []
mtf_decode dict [input]         = [dict!!input]
mtf_decode dict (input:inputs)  = (char: mtf_decode (char:(delete char (dict))) inputs)
    where
        char = dict!!input

-- This may be useful in implementing mtf_decode
safeIndex :: Int -> [a] -> Maybe a
safeIndex n _      | n < 0 = Nothing
safeIndex 0 (x:xs) = Just x
safeIndex n (_:xs) = safeIndex (n-1) xs
safeIndex _ []     = Nothing


------------------------------------ Tests -----------------------------------
--------------------- Put any tests you write below here ---------------------

prop_lossless_mtf :: String -> Bool
prop_lossless_mtf string = mtf_decode dict (mtf_encode dict string) == string
    where dict = remove_BWTValues mtf_char_dict

mtf_char_dict :: [BWTValue Char]
mtf_char_dict = EOS : map Value [minBound .. maxBound]

------------------------------------------------------------------------------
