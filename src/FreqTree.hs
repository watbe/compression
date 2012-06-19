-- Author: Wayne Tsai u5027622@anu.edu.au
module FreqTree (
    FreqTree(..),
    Bit(..),
    frequency,
    merge_trees,
    create_histogram,
    sort_trees,
    extract_bit_prefixes,
    histogram_to_freqTrees,
    freqTree_to_bitmap,
    create_bit_sequence,
    decode_using_tree
    ) where


-- You must complete the Histogram module before this one will work.
import Histogram

-- Some imports used in code provided to you, you can ignore them.
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List (sortBy)
import Data.Ord  (comparing)
import Data.Char (isDigit)
import qualified Data.Map as M


-- The binary tree which will form the basis of S-F and Huffman coding.
-- Values of this type will either be:
-- - a Leaf,  representing a single value and its frequency
-- - a Node, the combination of two other FreqTrees. Its frequency should
--   always be the sum of the left and right frequencies. (see merge_trees)
data FreqTree a
    = Leaf Int a
    | Node Int (FreqTree a) (FreqTree a)
    | Empty
    deriving (Eq)
    -- deriving (Eq, Show)

-- Represents a binary digit.
data Bit = Zero | One
         deriving (Show, Eq)


-- Return the combined frequency of a freqTree
frequency :: FreqTree a -> Int
frequency tree = case tree of
    Leaf freq _     -> freq
    Node freq _ _   -> freq


-- Merge two freqTrees together
merge_trees :: FreqTree a -> FreqTree a -> FreqTree a
merge_trees t1 t2 = Node (frequency t1 + frequency t2) t1 t2
 

-- Return the value of the leaf based on a sequence of bits,
-- returning a tuple of the value and remaining bits.
decode_freqTree_bits :: FreqTree a -> [Bit] -> (a,[Bit])
decode_freqTree_bits tree []            = case tree of
    Leaf _ a    -> (a, [])
    Node _ _ _  -> error "Not enough bits!"
decode_freqTree_bits tree [bit]         = case tree of
    Node _ a b 
        | bit == Zero   -> decode_freqTree_bits a []
        | otherwise     -> decode_freqTree_bits b []
    Leaf _ a    -> (a, [bit])
decode_freqTree_bits tree (bit:bits)    = case tree of 
    Node _ a b 
        | bit == Zero   -> decode_freqTree_bits a bits
        | otherwise     -> decode_freqTree_bits b bits
    Leaf _ a    -> (a, (bit:bits))


-- Manages the decoding process by calling decode_freqTree_bits
-- based on the return values.
decode_using_tree :: (FreqTree a, [Bit]) -> [a]
decode_using_tree (Empty,_)                 = []
decode_using_tree ((Leaf 0 value),[])       = []
decode_using_tree ((Leaf count value), [])  = (value: decode_using_tree ((Leaf (count - 1) value),[]))
decode_using_tree (_, [])                   = []
decode_using_tree (tree, [bit])             = [fst (decode_freqTree_bits tree [bit])]
decode_using_tree (tree, bits)              = (fst result : decode_using_tree (tree, (snd result)))
    where 
        result = decode_freqTree_bits tree bits

-- Convert Histograms to FreqTrees
histogram_to_freqTrees :: Histogram a -> [FreqTree a]
histogram_to_freqTrees histo = histogram_pairs_to_freqTrees (get_histogram_pairs histo)


-- Converts Histogram pairs to FreqTrees
histogram_pairs_to_freqTrees :: [(a,Int)] -> [FreqTree a]
histogram_pairs_to_freqTrees []     = []
histogram_pairs_to_freqTrees [x]    = [Leaf (snd x) (fst x)]
histogram_pairs_to_freqTrees (x:xs) = (Leaf (snd x) (fst x):histogram_pairs_to_freqTrees xs)

--
-- The following are functions you may need to use to complete this assignment
-- 

-- This function will take a list of FreqTrees and sort them in
-- ascending order of frequency. Do not modify it.
sort_trees :: [FreqTree a] -> [FreqTree a]
sort_trees xs = sortBy (comparing frequency) xs

-- Creates a reverse mapping from a value in a FreqTree to the
-- bit sequence which you'd traverse to find it.
freqTree_to_bitmap :: Ord a => FreqTree a -> M.Map a [Bit]
freqTree_to_bitmap tree = M.fromList (extract_bit_prefixes tree)

-- Converts a given mapping from values to their bit sequence,
-- and a list of values, into a list of bits.
create_bit_sequence :: Ord a => M.Map a [Bit] -> [a] -> [Bit]
create_bit_sequence mp lst = lst >>= (mp M.!)


-- Used to find the bit sequences to encode each value in a FreqTree.
-- This can be ignored, it is used in the code for writing data to disk.
extract_bit_prefixes :: FreqTree a -> [(a,[Bit])]
extract_bit_prefixes tree = case tree of
      (Leaf _ x)   -> [(x,[])]
      (Node _ l r) ->
            map (\(x,bits) -> (x,Zero:bits)) (extract_bit_prefixes l) ++
            map (\(x,bits) -> (x, One:bits)) (extract_bit_prefixes r)


------------------------------------ Tests -----------------------------------
--------------------- Put any tests you write below here ---------------------





------------------------------------------------------------------------------


bad_example_test :: Bool
bad_example_test
    =   sort_trees (histogram_to_freqTrees (create_histogram "Hello" M.empty)) 
        == sort_trees [Leaf 1 'H',Leaf 1 'e',Leaf 2 'l',Leaf 1 'o']
    && sort_trees (histogram_to_freqTrees $ create_histogram "The quick brown fox jumped over the lazy dog." M.empty)
        == sort_trees [ Leaf 1 'T', Leaf 1 'q', Leaf 1 'i', Leaf 1 'c', Leaf 1 'k', Leaf 1 'b',
                       Leaf 1 'w', Leaf 1 'n', Leaf 1 'f', Leaf 1 'x', Leaf 1 'j', Leaf 1 'm',
                       Leaf 1 'p', Leaf 1 'v', Leaf 1 't', Leaf 1 'l', Leaf 1 'a', Leaf 1 'z',
                       Leaf 1 'y', Leaf 1 'g', Leaf 1 '.', Leaf 2 'h', Leaf 2 'u', Leaf 2 'r',
                       Leaf 2 'd', Leaf 4 'e', Leaf 4 'o', Leaf 8 ' ']


--------------------------------------------------------------------------------
--------------------- Everything below here you can ignore ---------------------
--------------------------------------------------------------------------------

-- Makes trees print out in a pretty way
instance Show a => Show (FreqTree a) where
    show (Leaf n x) = let xstr = show x in pad (length xstr) ' ' (show n) ++ "\n" ++ xstr
    show (Node n l r) = let
        llines = lines (show l)
        lmax   = maximum (map length llines)
        rlines = lines (show r)
        rmax   = maximum (map length rlines)
        nstr   = show n
        nwidth = length nstr
        depth  = length llines `max` length rlines
        spaces n     = replicate n ' '
        joined = zipWith3 (\left mid right -> left ++ mid ++ right)
                    (llines ++ repeat (spaces lmax))
                    (replicate depth (spaces nwidth))
                    (rlines ++ repeat (spaces rmax))
        
        nLine = spaces lmax ++ nstr ++ spaces rmax
        (barLine1, rest1) = break isDigit (head joined) -- find first digit
        (barLine2, rest2) = break (not . isDigit) rest1 -- skip digits
        (barLine3, rest3) = break isDigit rest2         -- find next digit
        (barLine4       ) = map (const ' ') rest3       -- fill the rest with spaces
        barLine = barLine1 ++ " " ++
                  "/" ++
                  map (const '-') (drop 2 $ barLine2 ++ barLine3) ++
                  "\\" ++
                  drop 1 barLine4
        in unlines (nLine:barLine:joined) 

pad :: Int -> Char -> String -> String
pad width char str =
    let len = length str
        lenHalf = (width - len) `div` 2
    in replicate (width - len - lenHalf) char ++ str ++ replicate lenHalf char
 
instance Binary a => Binary (FreqTree a) where
    put (Leaf n x) = putWord8 0 >> putWord32be (fromIntegral n) >> put x
    put (Node _ l r) = putWord8 1 >> put l >> put r
    get = do
        b <- getWord8
        case b of
            0 -> do
                n <- getWord32be
                x <- get
                return $ Leaf (fromIntegral n) x
            1 -> do
                l <- get
                r <- get
                return $ merge_trees l r
            _ -> error $ "get(FreqTree): Unexpected tag: " ++ show b

instance Functor FreqTree where
    fmap f (Leaf n x) = Leaf n (f x)
    fmap f (Node n l r) = Node n (fmap f l) (fmap f r)
