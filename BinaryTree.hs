{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module BinaryTree where

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x Nil Nil

insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = singleton x
insert x (Node y l r)
  | x == y = Node x l r
  | x < y = Node y (insert x l) r
  | otherwise = Node y l (insert x r)

search :: Ord t => t -> Tree t -> Maybe t
search _ Nil = Nothing
search x (Node y l r)
  | x == y = Just y
  | x < y = search x l
  | otherwise = search x r

searchMin :: Tree a -> Maybe a
searchMin Nil = Nothing
-- 左にノードが無ければ最小
searchMin (Node x Nil _) = Just x
-- 左にノードがあればsearchMinに渡す
searchMin (Node _ l _) = searchMin l

searchMax Nil = Nothing
-- 右にノードが無ければ最大
searchMax (Node x _ Nil) = Just x
-- 右にノードがあればsearchMaxに渡す
searchMax (Node _ _ r) = searchMax r

--リストから二分木を作る
-- foldl (二項演算子) リストが空の時の数値 リスト
-- 例えば foldl (+) 0 [1,2,4,5]
--       -> (5+(4+(2+(1+(0))))))
fromList :: (Foldable t, Ord a) => t a -> Tree a
fromList xs = foldl (flip insert) Nil xs

deleteMin :: Tree a -> Tree a
deleteMin Nil = Nil
deleteMin (Node _ Nil r) = r
deleteMin (Node x l r) = Node x (deleteMin l) r

deleteMax :: Tree a -> Tree a
deleteMax Nil = Nil
deleteMax (Node _ l Nil) = l
deleteMax (Node x l r) = Node x l (deleteMax r)

delete :: Ord a => a -> Tree a -> Tree a
delete x Nil = Nil
delete x (Node y l r)
  | x < y = Node y (delete x l) r
  | x > y = Node y l (delete x r)
  | otherwise = delete' l r
  where
    delete' Nil r = r
    delete' l Nil = l
    delete' l r = Node x' l (deleteMin r)
      where
        Just x' = searchMin r

-- 二分技をリストに変換する（通りがけ順)
toList :: Tree a -> [a]
toList Nil = []
toList (Node x l r) = toList l ++ [x] ++ toList r
