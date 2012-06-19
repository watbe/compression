module Huffman (
    huffman_compress
    )
    where

import Histogram
import FreqTree
import qualified Data.Map as M
import Test.QuickCheck

-- Apply the compression partial functions based on input
huffman_compress :: Ord a => [a] -> (FreqTree a, [Bit])
huffman_compress []         = (Empty,[])
huffman_compress inputs     = (freqTree, create_bit_sequence (freqTree_to_bitmap freqTree) inputs )
    where
        freqTree = build_freqTree (create_histogram inputs empty_histogram)


-- Create a frequency tree from a histogram
build_freqTree :: Histogram a -> FreqTree a
build_freqTree histo    = merge_freqTrees (reverse (sort_trees (histogram_to_freqTrees histo)))


-- Merge two frequency trees together maintaining frequencies
merge_freqTrees :: [FreqTree a] -> FreqTree a
merge_freqTrees []      = error "empty list!"
merge_freqTrees [tree]  = tree
merge_freqTrees trees   = merge_freqTrees (merge_least_frequent trees)


-- Merge the least frequent trees (should be last in the list)
merge_least_frequent :: [FreqTree a] -> [FreqTree a]
merge_least_frequent [tree]     = error "should not need to merge_least_frequent a list of one tree!"
merge_least_frequent [t1, t2]   = [merge_trees t1 t2]
merge_least_frequent trees      = ((merge_trees lastly secondly) : reverse restly)
    where
        (lastly:secondly:restly) = reverse trees

------------------------------------ Tests -----------------------------------
--------------------- Put any tests you write below here ---------------------

prop_lossless_huff :: String -> Bool
prop_lossless_huff string = decode_using_tree (huffman_compress string) == string

------------------------------------------------------------------------------
