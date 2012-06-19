-- Author: Wayne Tsai u5027622@anu.edu.au
module LZW (
    lzw_compress,
    lzw_decompress
    ) where

import TernaryTree

import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Test.QuickCheck

data LZWState a = LZWState { lzwNextCode :: Int, lzwKnownPrefixes :: TernaryTree a}
    deriving Show

-- Initialises a LZWState from a given TernaryTree. This will
-- ensure that the lzeNextCode value is set correctly.
make_lzw_state :: TernaryTree a -> LZWState a
make_lzw_state tree = LZWState ((+1) . maximum . map snd . get_ternaryTree_mappings $ tree) tree

-- Manages the current state of the LZW encoding step. Used by look for each 
-- string it encounters.
lzw_state_machine :: Ord a => [a] -> [a] -> LZWState a -> (LZWState a, Maybe Int,[a])
lzw_state_machine [] prefix (LZWState nextCode tree) = ((LZWState nextCode tree), lookup_prefix prefix tree, [])
lzw_state_machine (input:inputs) prefix (LZWState nextCode tree)
    | lookup_prefix new_prefix tree /= Nothing  = lzw_state_machine inputs new_prefix (LZWState nextCode tree)
    | otherwise                                 = ((LZWState (nextCode + 1) new_tree), lookup_prefix prefix tree, (input:inputs))
        where 
            new_prefix  = prefix ++ [input]
            new_tree    = insert_prefix new_prefix nextCode tree

-- Look up the required characters and returns its index         
lzw_loop :: Ord a => [a] -> LZWState a -> [Int]
lzw_loop [] state       = []
lzw_loop [input] state  = [fromJust output]
        where (new_state, output, remaining) = lzw_state_machine [input] [] state 
lzw_loop inputs state   = (fromJust output : lzw_loop remaining new_state)
        where (new_state, output, remaining) = lzw_state_machine inputs [] state 


-- Combines the functions to apply LZW compression
lzw_compress :: Ord a => TernaryTree a -> [a] -> [Int]
lzw_compress initial_tree inputs = lzw_loop inputs (make_lzw_state initial_tree)


-- A tree used for strings that contain nothing but the letters a-z.
-- This might be useful to see if your encoder matches the Wikipedia
-- example.
az_tree :: TernaryTree Char
az_tree = make_initial_tree (\x -> ord x - ord 'a') 'a' 'z'

-- This tree contains all ASCII values, so should work with most text you
-- encounter.
ascii_tree :: TernaryTree Char
ascii_tree = make_initial_tree ord '\0' '\255'

-----------------------------------------------------------------------------
-- The code below is the code used to decompress a LZW encoded stream of data
-- You might find it useful to try to understand some of it while implementing
-- the encoder. You should make sure that if you can compress something
-- using: 
--      lzw_compress some_tree xs
-- then decompressing the result using
--      lzw_decompress some_tree <result>
-- is equal to xs.

data LZWDecodeState a = LZWDecodeState Int (M.Map Int [a]) deriving Show

make_lzw_decode_state :: Ord a => TernaryTree a -> LZWDecodeState a
make_lzw_decode_state tree =
    LZWDecodeState 
        ((+1) . maximum . map snd . get_ternaryTree_mappings $ tree)
        (M.fromList . map (\(x,n) -> (n,x)) . get_ternaryTree_mappings $ tree)


lzw_decompress_state_machine :: LZWDecodeState a -> [a] -> Int -> (LZWDecodeState a, [a])
lzw_decompress_state_machine state prev input =
    let new_dict  = M.insert n (prev ++ take 1 entry) mp
        new_state = LZWDecodeState (n+1) new_dict
    in  (new_state, entry)
    where LZWDecodeState n mp = state
          entry = case M.lookup input mp of
            Just str -> str
            Nothing | input == n -> prev ++ take 1 prev
                    | otherwise  -> error $ "lzw_decompress_state_machine: unknown code: " ++ show input


lzw_decompress_loop :: LZWDecodeState a -> [a] -> [Int] -> [a]
lzw_decompress_loop state prev coded = case coded of
    [] -> []
    (x:xs) -> case lzw_decompress_state_machine state prev x of
        (_,[]) -> []
        (new_state,str) -> str ++ lzw_decompress_loop new_state str xs


lzw_decompress :: Ord a => TernaryTree a -> [Int] -> [a]
lzw_decompress tree encoded = let state@(LZWDecodeState _ mp) = make_lzw_decode_state tree
    in case encoded of
        [] -> []
        (x:xs) -> let Just first = (M.lookup x mp)
                  in  first ++ lzw_decompress_loop state first xs


-- The following two functions do almost the same thing as the above two
-- except that it will split the output into the chunks that are decompressed
-- by each Int passed in.
lzw_decompress_loop' :: LZWDecodeState a -> [a] -> [Int] -> [[a]]
lzw_decompress_loop' state prev coded = case coded of
    [] -> []
    (x:xs) -> case lzw_decompress_state_machine state prev x of
        (_,[]) -> []
        (new_state,str) -> str : lzw_decompress_loop' new_state str xs

lzw_decompress' :: Ord a => TernaryTree a -> [Int] -> [[a]]
lzw_decompress' tree encoded = let state@(LZWDecodeState _ mp) = make_lzw_decode_state tree
    in case encoded of
        [] -> []
        (x:xs) -> let Just first = (M.lookup x mp)
                  in  first : lzw_decompress_loop' state first xs

------------------------------------ Tests -----------------------------------
--------------------- Put any tests you write below here ---------------------

-- Ensure losslessness 
prop_lossless_lzw :: String -> Bool
prop_lossless_lzw string = lzw_decompress ascii_tree (lzw_compress ascii_tree string) == string

------------------------------------------------------------------------------

-- An example from the assignment spec.
example_test :: Bool
example_test = lzw_compress az_tree "aaaabaaab"
             == [0,26,0,1,27,1]

-- The example from Wikipedia, but in lowercase (the results should
-- be exactly the same however).
test_string :: String
test_string = "tobeornottobeortobeornot"
simple_test :: Bool
simple_test = lzw_compress az_tree test_string
            ==  [19,14,1,4,14,17,13,14,19,26,28,30,35,29,31,33]


