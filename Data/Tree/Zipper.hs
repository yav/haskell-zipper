--
-- Copyright (c) Krasimir Angelov 2008.
-- Copyright (c) Iavor S. Diatchki 2008.
--
-- Generic zipper implementation for Data.Tree
--
--

module Data.Tree.Zipper
  ( TreeLoc(..)

  -- * Conversions
  , fromTree
  , fromForest
  , toForest
  , toTree

  -- * Moving around
  , parent
  , root
  , getChild
  , findChild
  , firstChild
  , lastChild
  , left
  , right

  -- * Node classification
  , isRoot
  , isFirst
  , isLast
  , isLeaf
  , isChild
  , hasChildren
  , getNodeIndex

  -- * Tree-specific mutation
  , insertLeft
  , insertRight
  , insertDownFirst
  , insertDownLast
  , insertDownAt
  , delete

  -- * Working with the current tree
  , setTree
  , modifyTree
  , modifyLabel
  , setLabel
  , getLabel
  ) where

import Data.Tree

-- | A position within a 'Tree'.
data TreeLoc a  = Loc
  { tree    :: Tree a       -- ^ The currently selected tree.
  , lefts   :: Forest a     -- ^ Siblings on the left, closest first.
  , rights  :: Forest a     -- ^ Siblings on the right, closest first.
  , parents :: [(Forest a, a, Forest a)]
      -- ^ The contexts of the parents for this location.
  } deriving (Read,Show,Eq)

-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: TreeLoc a -> Maybe (TreeLoc a)
parent loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Loc { tree = Node v (combChildren (lefts loc) (tree loc) (rights loc))
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing


-- | The top-most parent of the given location.
root :: TreeLoc a -> TreeLoc a
root loc = maybe loc root (parent loc)


-- | The left sibling of the given location.
left :: TreeLoc a -> Maybe (TreeLoc a)
left loc =
  case lefts loc of
    t : ts -> Just loc { tree = t, lefts = ts, rights = tree loc : rights loc }
    []     -> Nothing

-- | The right sibling of the given location.
right :: TreeLoc a -> Maybe (TreeLoc a)
right loc =
  case rights loc of
    t : ts -> Just loc { tree = t, lefts = tree loc : lefts loc, rights = ts }
    []     -> Nothing


-- | The first child of the given location.
firstChild :: TreeLoc a -> Maybe (TreeLoc a)
firstChild loc =
  case subForest (tree loc) of
    t : ts -> Just
      Loc { tree = t, lefts = [], rights = ts , parents = downParents loc }
    [] -> Nothing

-- | The last child of the given location.
lastChild :: TreeLoc a -> Maybe (TreeLoc a)
lastChild loc =
  case reverse (subForest (tree loc)) of
    t : ts -> Just
      Loc { tree = t, lefts = ts, rights = [], parents = downParents loc }
    [] -> Nothing

-- | The child with the given index (starting from 0).
getChild :: Int -> TreeLoc a -> Maybe (TreeLoc a)
getChild n loc =
  do (ls,t,rs) <- splitChildren (subForest (tree loc)) n
     return Loc { tree = t, lefts = ls, rights = rs, parents = downParents loc }

-- | The first child that satisfies a predicate.
findChild :: (Tree a -> Bool) -> TreeLoc a -> Maybe (TreeLoc a)
findChild p loc =
  do (ls,t,rs) <- split [] (subForest (tree loc))
     return Loc { tree = t, lefts = ls, rights = rs, parents = downParents loc }

  where split acc (x:xs) | p x  = Just (acc,x,xs)
        split acc (x:xs)        = split (x:acc) xs
        split _ []              = Nothing

-- | private: computes the parent for "down" operations.
downParents :: TreeLoc a -> [(Forest a, a, Forest a)]
downParents loc = (lefts loc, rootLabel (tree loc), rights loc) : parents loc



-- Conversions -----------------------------------------------------------------

-- | A location corresponding to the root of the given tree.
fromTree :: Tree a -> TreeLoc a
fromTree t = Loc { tree = t, lefts = [], rights = [], parents = [] }

-- | The location of the first tree in a forest.
fromForest :: Forest a -> Maybe (TreeLoc a)
fromForest (t:ts) = Just Loc { tree = t, lefts = [], rights = ts, parents = [] }
fromForest []     = Nothing

-- | Computes the tree containing this location.
toTree :: TreeLoc a -> Tree a
toTree loc = tree (root loc)

-- | Computes the forest containing this location.
toForest :: TreeLoc a -> Forest a
toForest loc = let r = root loc in combChildren (lefts r) (tree r) (rights r)


-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the tree?
isRoot :: TreeLoc a -> Bool
isRoot loc = null (parents loc)

-- | Are we at the left end of the the tree?
isFirst :: TreeLoc a -> Bool
isFirst loc = null (lefts loc)

-- | Are we at the right end of the tree?
isLast :: TreeLoc a -> Bool
isLast loc = null (rights loc)

-- | Are we at the bottom of the tree?
isLeaf :: TreeLoc a -> Bool
isLeaf loc = null (subForest (tree loc))

-- | Do we have a parent?
isChild :: TreeLoc a -> Bool
isChild loc = not (isRoot loc)

-- | Get the node index inside the sequence of children
getNodeIndex :: TreeLoc a -> Int
getNodeIndex loc = length (lefts loc)

-- | Do we have children?
hasChildren :: TreeLoc a -> Bool
hasChildren loc = not (isLeaf loc)


-- The current tree -----------------------------------------------------------


-- | Change the current tree.
setTree :: Tree a -> TreeLoc a -> TreeLoc a
setTree t loc = loc { tree = t }

-- | Modify the current tree.
modifyTree :: (Tree a -> Tree a) -> TreeLoc a -> TreeLoc a
modifyTree f loc = setTree (f (tree loc)) loc

-- | Modify the label at the current node.
modifyLabel :: (a -> a) -> TreeLoc a -> TreeLoc a
modifyLabel f loc = setLabel (f (getLabel loc)) loc

-- | Change the label at the current node.
setLabel :: a -> TreeLoc a -> TreeLoc a
setLabel v loc = modifyTree (\t -> t { rootLabel = v }) loc

-- Get the current label.
getLabel :: TreeLoc a -> a
getLabel loc = rootLabel (tree loc)


--------------------------------------------------------------------------------

-- | Insert a tree to the left of the current position.
-- The new tree becomes the current tree.
insertLeft :: Tree a -> TreeLoc a -> TreeLoc a
insertLeft t loc = loc { tree = t, rights = tree loc : rights loc }

-- | Insert a tree to the right of the current position.
-- The new tree becomes the current tree.
insertRight :: Tree a -> TreeLoc a -> TreeLoc a
insertRight t loc = loc { tree = t, lefts = tree loc : lefts loc }

-- | Insert a tree as the first child of the current position.
-- The new tree becomes the current tree.
insertDownFirst :: Tree a -> TreeLoc a -> TreeLoc a
insertDownFirst t loc =
  loc { tree = t, lefts = [], rights = subForest (tree loc)
      , parents = downParents loc }

-- | Insert a tree as the last child of the current position.
-- The new tree becomes the current tree.
insertDownLast :: Tree a -> TreeLoc a -> TreeLoc a
insertDownLast t loc =
  loc { tree = t, lefts = reverse (subForest (tree loc)), rights = []
      , parents = downParents loc }

-- | Insert a tree as a particular child of the current location.
-- Children are numbered from 0.
-- The new tree becomes the current tree.
insertDownAt :: Int -> Tree a -> TreeLoc a -> Maybe (TreeLoc a)
insertDownAt n t loc =
  do (ls,x,rs) <- splitChildren (subForest (tree loc)) n
     return Loc { tree = t, lefts = ls, rights = x : rs
                , parents = downParents loc }

-- | Delete the current node.  The new position is:
--   * the right sibling, or if none
--   * the left sibling, or if none
--   * the parent.
delete :: TreeLoc a -> Maybe (TreeLoc a)
delete loc =
  case rights loc of
    t : ts -> Just loc { tree = t, rights = ts }
    _ -> case lefts loc of
           t : ts -> Just loc { tree = t, lefts = ts }
           _ -> do loc1 <- parent loc
                   return $ modifyTree (\t -> t { subForest = [] }) loc1


-- | private: Gets the 'n'th element of a list.
-- -- Also return the preceeding element
-- (reversed) and the folloing elements.
splitChildren :: [a] -> Int -> Maybe ([a],a,[a])
splitChildren _ n | n < 0 = Nothing
splitChildren cs pos = loop [] cs pos
  where loop acc (x:xs) 0 = Just (acc,x,xs)
        loop acc (x:xs) n = loop (x:acc) xs $! n-1
        loop _ _ _        = Nothing

-- | private: combChildren ls x ys = reverse ls ++ [x] ++ ys
combChildren :: [a] -> a -> [a] -> [a]
combChildren ls t rs = foldl (flip (:)) (t:rs) ls

