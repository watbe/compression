-- Author: Wayne Tsai u5027622@anu.edu.au
module TernaryTree ( TernaryTree
                   , insert_prefix
                   , lookup_prefix
                   , get_ternaryTree_mappings
                   , make_initial_tree
                   , empty_ternary
                   ) where

import Text.Printf (printf)
import Data.List (sort)


data TernaryTree a
    = Empty | Node a Int (TernaryTree a) (TernaryTree a) (TernaryTree a)
    deriving (Eq)


lookup_prefix :: Ord a => [a] -> TernaryTree a -> Maybe Int
lookup_prefix [] _ = Nothing
lookup_prefix _ Empty = Nothing
lookup_prefix (key:keys) tree = case (key:keys, tree) of 
    ((key:keys), Node element value left middle right)
        | null keys && key == element   -> Just value
        | key < element                 -> lookup_prefix (key:keys) left
        | key > element                 -> lookup_prefix (key:keys) right
        | key == element                -> lookup_prefix keys middle


insert_prefix :: Ord a => [a] -> Int -> TernaryTree a -> TernaryTree a 
insert_prefix [key] value Empty = Node key value Empty Empty Empty
insert_prefix keys _ Empty
    | null keys == False = error "Error! Cannot insert value if smaller string does not exist! (1)"
insert_prefix (key:keys) value tree = case tree of 
    Node element tree_value left middle right
        | key < element                         -> Node element tree_value (insert_prefix (key:keys) value left) middle right
        | key > element                         -> Node element tree_value left middle (insert_prefix (key:keys) value right)
        | key == element && null keys == False  -> Node element tree_value left (insert_prefix keys value middle) right
        | key == element && null keys           -> error "Error! Cannot insert value if smaller string does not exist! (2) "

empty_ternary :: TernaryTree a
empty_ternary = Empty

------------------------------------ Tests -----------------------------------
--------------------- Put any tests you write below here ---------------------





------------------------------------------------------------------------------


-- get_ternaryTree_keys :: TernaryTree a -> [[a]]
-- get_ternaryTree_keys tree = map fst (get_ternaryTree_mappings tree)
-- 
get_ternaryTree_mappings :: TernaryTree a -> [([a], Int)]
get_ternaryTree_mappings tree = case tree of
    Empty -> []
    (Node x n l e h) -> get_ternaryTree_mappings l
                     ++ [([x],n)]
                     ++ map (\(xs,m) -> (x:xs,m)) (get_ternaryTree_mappings e)
                     ++ get_ternaryTree_mappings h

make_initial_tree :: (Ord a, Enum a) => (a -> Int) -> a -> a -> TernaryTree a
make_initial_tree f a b = make_balanced_tree $ map (\x -> (x,f x)) [a..b]



instance Show a => Show (TernaryTree a) where
    show = unlines . show_ternary_tree
       
-- This code generously provided by Edward Kmett
show_ternary_tree :: Show a => TernaryTree a -> [String]
show_ternary_tree = go " " "+" " " where
  go _ m _ Empty = [m ++ "*"]
  go l m r (Node a n lt mt rt) =
       go (l ++ spaces) (l ++ node) (l ++ bar) lt
    ++ [l ++ bar]
    -- ++ go (l ++ bar) (m ++ "+ " ++ label ++ " --") (r ++ bar) mt
    ++ (if empty mt then [m ++ "+ " ++ label ++ " -*"]
                    else go (l ++ bar ++ "   ") (m ++ "+ " ++ label ++ " -") (r ++ bar ++ "   ") mt)
    ++ [r ++ bar]
    ++ go (r ++ bar) (r ++ node) (r ++ spaces) rt
    where ll = length label
          bar    = "   " ++ '|' : replicate (ll-3) ' '
          spaces = replicate (ll+1) ' '
          node   = "   " ++ '+' : replicate (ll-3) '-'
          label = printf "(%d) %s" n (show a)

empty :: TernaryTree a -> Bool
empty tree = case tree of
    Empty -> True
    _     -> False

make_balanced_tree :: Ord a => [(a,Int)] -> TernaryTree a
make_balanced_tree pairs = go (sort pairs) where
    go xs = case xs of
        [] -> Empty
        [(x,n)] -> Node x n Empty Empty Empty
        _ -> case splitAt (len `div` 2) xs of
            -- ([],(x,n):ys) -> Node x n Empty Empty (go ys)
            (ys,(x,n):zs) -> Node x n (go ys) Empty (go zs)
            _ -> Empty
        where len = length xs

