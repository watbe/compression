-- Author: Wayne Tsai u5027622@anu.edu.au
module RunLengthEncode (
    rle_encode,
    rle_decode
    ) where

import Data.List
import Test.QuickCheck

-- I use a 'backwards' method of encoding and thus decoding to avoid
-- costly methods of returning the last values. This means that while
-- the encoding/decoding will be faster, things encoded by this method
-- might not be decoded by a third party program, nor will this be able
-- to decode other implementations of RLE.
rle_encode :: Eq a => [a] -> [(a,Int)]
rle_encode []       = []
rle_encode chars    = rle_step [] chars

-- The recursive function used to encode the list of symbols
rle_step :: Eq a => [(a,Int)] -> [a] -> [(a,Int)]
rle_step _ []               = [] 
rle_step [] [char]          = [(char,1)]
rle_step [] (char:chars)    = rle_step [(char,1)] chars
rle_step partial [char]
    | previous_value == 255 || char /= previous_char    = ((char,1):partial)
    | char == previous_char                             = ((char, previous_value + 1):sequences)
        where 
            previous_char           = fst sequence
            previous_value          = snd sequence
            (sequence:sequences)    = partial
rle_step partial (char:chars)
    | previous_value == 255 || char /= previous_char    = rle_step ((char,1):partial) chars 
    | char == previous_char                             = rle_step ((char, previous_value + 1):sequences) chars 
        where 
            previous_char           = fst sequence
            previous_value          = snd sequence
            (sequence:sequences)    = partial

-- Recursive function to decode RLE ecoding
rle_decode :: [(a,Int)] -> [a]
rle_decode []               = []
rle_decode [(value, 1)]     = [value]
rle_decode (input:inputs)   = case input of 
    (value, 1)      -> rle_decode inputs ++ [value]
    (value, count)  -> rle_decode ((value, count - 1):inputs) ++ [value]

------------------------------------ Tests -----------------------------------
--------------------- Put any tests you write below here ---------------------

prop_lossless_rle :: String -> Bool
prop_lossless_rle string = rle_decode (rle_encode string) == string

unitTest :: Bool
unitTest = rle_encode (replicate 500 'a') == [('a',245), ('a',255)]

------------------------------------------------------------------------------
