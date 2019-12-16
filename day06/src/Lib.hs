module Lib
    ( countOrbits
    , orbitalTransfers
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative
import Data.Maybe

data Tree a = Tree a [Tree a] deriving (Eq, Show)

countOrbits :: Text -> Int
countOrbits = score . parseTree

parseTree :: Text -> Tree Text
parseTree = buildTree . map parseBranch . T.lines

parseBranch :: Text -> (Text, Text)
parseBranch txt = (parent, child)
  where
    parent:child:_ = T.splitOn ")" txt

buildTree :: (Eq a, Ord a) => [(a, a)] -> Tree a
buildTree branches = addAll (Tree (findRoot branchSet) []) branchSet
  where
    branchSet = S.fromList branches

addAll :: (Eq a, Ord a) => Tree a -> Set (a, a) -> Tree a
addAll tree branches
    | S.null branches = tree
    | otherwise = addAll newTree restOfBranches
  where
    newTree = S.foldl add tree addableBranches
    (addableBranches, restOfBranches) = S.partition (addableTo tree) branches

add :: (Eq a) => Tree a -> (a, a) -> Tree a
add (Tree root subTrees) branch@(parent, child)
    | root == parent = Tree root $ Tree child []:subTrees
    | otherwise = Tree root (map (`add` branch) subTrees)

addableTo :: (Ord a) => Tree a -> (a, a) -> Bool
addableTo tree (parent, _) = S.member parent (leaves tree)

leaves :: (Ord a) => Tree a -> Set a
leaves (Tree root []) = S.singleton root
leaves (Tree root subTrees) = foldr (S.union . leaves) S.empty subTrees

findRoot :: (Eq a, Ord a) => Set (a, a) -> a
findRoot branches = S.findMin $ S.difference (S.map fst branches) (S.map snd branches)

score :: Tree a -> Int
score (Tree root []) = 0
score tree@(Tree root subTrees) = numberOfChildNodes tree + sum (map score subTrees)

numberOfChildNodes :: Tree a -> Int
numberOfChildNodes (Tree root []) = 0
numberOfChildNodes (Tree root subTrees) = length subTrees + sum (map numberOfChildNodes subTrees)

orbitalTransfers :: Text -> Maybe Int
orbitalTransfers txt = do
    yourDistance <- distanceFromRoot smallestParentTree "YOU"
    santasDistance <- distanceFromRoot smallestParentTree "SAN"
    return $ yourDistance + santasDistance - 2
  where
    smallestParentTree = smallesSubtreeContainingLeaves (parseTree txt) ["YOU", "SAN"]

smallesSubtreeContainingLeaves :: (Eq a)=> Tree a -> [a] -> Tree a
smallesSubtreeContainingLeaves tree@(Tree _ subTrees) leaves =
    if null subtreesContaningLeaves
        then tree
        else smallesSubtreeContainingLeaves (head subtreesContaningLeaves) leaves
  where
    subtreesContaningLeaves = filter (`containsLeaves` leaves) subTrees
    
containsLeaves :: (Eq a) => Tree a -> [a] -> Bool
containsLeaves tree = all (containsLeaf tree)

containsLeaf :: (Eq a) => Tree a -> a -> Bool
containsLeaf tree leaf = isJust $ distanceFromRoot tree leaf

distanceFromRoot :: (Eq a) => Tree a -> a -> Maybe Int
distanceFromRoot (Tree root []) leaf
    | root == leaf = Just 0
    | otherwise = Nothing
distanceFromRoot (Tree _ subTrees) leaf = do
    distance <- foldr ((<|>) . (`distanceFromRoot` leaf)) Nothing subTrees
    return $ distance + 1
