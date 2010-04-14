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
data TreePos ptr a  = Loc
  { content :: ptr a        -- ^ The currently selected tree.
  , lefts   :: Forest a     -- ^ Siblings on the left, closest first.
  , rights  :: Forest a     -- ^ Siblings on the right, closest first.
  , parents :: [(Forest a, a, Forest a)]
      -- ^ The contexts of the parents for this location.
  } deriving (Read,Show,Eq)

data Empty a    = E
newtype Full a  = F { unF :: Tree a }

tree x = unF (content x)


type TreeLoc = TreePos Full

-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: TreePos Full a -> Maybe (TreePos Full a)
parent loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Loc { content = F $ Node v (combChildren (lefts loc) (tree loc) (rights loc))
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing


-- | The top-most parent of the given location.
root :: TreePos Full a -> TreePos Full a
root loc = maybe loc root (parent loc)


-- | The left sibling of the given location.
left :: TreePos Full a -> Maybe (TreePos Full a)
left loc =
  case lefts loc of
    t : ts -> Just loc { content = F t, lefts = ts, rights = tree loc : rights loc }
    []     -> Nothing

-- | The right sibling of the given location.
right :: TreePos Full a -> Maybe (TreePos Full a)
right loc =
  case rights loc of
    t : ts -> Just loc { content = F t, lefts = tree loc : lefts loc, rights = ts }
    []     -> Nothing


-- | The first child of the given location.
firstChild :: TreePos Full a -> Maybe (TreePos Full a)
firstChild loc =
  case subForest (tree loc) of
    t : ts -> Just
      Loc { content = F t, lefts = [], rights = ts , parents = downParents loc }
    [] -> Nothing

-- | The last child of the given location.
lastChild :: TreePos Full a -> Maybe (TreePos Full a)
lastChild loc =
  case reverse (subForest (tree loc)) of
    t : ts -> Just
      Loc { content = F t, lefts = ts, rights = [], parents = downParents loc }
    [] -> Nothing

-- | The child with the given index (starting from 0).
getChild :: Int -> TreePos Full a -> Maybe (TreePos Full a)
getChild n loc =
  do (ls,t,rs) <- splitChildren (subForest (tree loc)) n
     return Loc { content = F t, lefts = ls, rights = rs, parents = downParents loc }

-- | The first child that satisfies a predicate.
findChild :: (Tree a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findChild p loc =
  do (ls,t,rs) <- split [] (subForest (tree loc))
     return Loc { content = F t, lefts = ls, rights = rs, parents = downParents loc }

  where split acc (x:xs) | p x  = Just (acc,x,xs)
        split acc (x:xs)        = split (x:acc) xs
        split _ []              = Nothing

-- | private: computes the parent for "down" operations.
downParents :: TreePos Full a -> [(Forest a, a, Forest a)]
downParents loc = (lefts loc, rootLabel (tree loc), rights loc) : parents loc



-- Conversions -----------------------------------------------------------------

-- | A location corresponding to the root of the given tree.
fromTree :: Tree a -> TreePos Full a
fromTree t = Loc { content = F t, lefts = [], rights = [], parents = [] }

-- | The location of the first tree in a forest.
fromForest :: Forest a -> Maybe (TreePos Full a)
fromForest (t:ts) = Just Loc { content = F t, lefts = [], rights = ts, parents = [] }
fromForest []     = Nothing

-- | Computes the tree containing this location.
toTree :: TreePos Full a -> Tree a
toTree loc = tree (root loc)

-- | Computes the forest containing this location.
toForest :: TreePos Full a -> Forest a
toForest loc = let r = root loc in combChildren (lefts r) (tree r) (rights r)


-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the tree?
isRoot :: TreePos Full a -> Bool
isRoot loc = null (parents loc)

-- | Are we at the left end of the the tree?
isFirst :: TreePos Full a -> Bool
isFirst loc = null (lefts loc)

-- | Are we at the right end of the tree?
isLast :: TreePos Full a -> Bool
isLast loc = null (rights loc)

-- | Are we at the bottom of the tree?
isLeaf :: TreePos Full a -> Bool
isLeaf loc = null (subForest (tree loc))

-- | Do we have a parent?
isChild :: TreePos Full a -> Bool
isChild loc = not (isRoot loc)

-- | Get the node index inside the sequence of children
getNodeIndex :: TreePos Full a -> Int
getNodeIndex loc = length (lefts loc)

-- | Do we have children?
hasChildren :: TreePos Full a -> Bool
hasChildren loc = not (isLeaf loc)


-- The current tree -----------------------------------------------------------


-- | Change the current tree.
setTree :: Tree a -> TreePos Full a -> TreePos Full a
setTree t loc = loc { content = F t }

-- | Modify the current tree.
modifyTree :: (Tree a -> Tree a) -> TreePos Full a -> TreePos Full a
modifyTree f loc = setTree (f (tree loc)) loc

-- | Modify the label at the current node.
modifyLabel :: (a -> a) -> TreePos Full a -> TreePos Full a
modifyLabel f loc = setLabel (f (getLabel loc)) loc

-- | Change the label at the current node.
setLabel :: a -> TreePos Full a -> TreePos Full a
setLabel v loc = modifyTree (\t -> t { rootLabel = v }) loc

-- Get the current label.
getLabel :: TreePos Full a -> a
getLabel loc = rootLabel (tree loc)


--------------------------------------------------------------------------------

-- | Insert a tree to the left of the current position.
-- The new tree becomes the current tree.
insertLeft :: Tree a -> TreePos Full a -> TreePos Full a
insertLeft t loc = loc { content = F t, rights = tree loc : rights loc }

-- | Insert a tree to the right of the current position.
-- The new tree becomes the current tree.
insertRight :: Tree a -> TreePos Full a -> TreePos Full a
insertRight t loc = loc { content = F t, lefts = tree loc : lefts loc }

-- | Insert a tree as the first child of the current position.
-- The new tree becomes the current tree.
insertDownFirst :: Tree a -> TreePos Full a -> TreePos Full a
insertDownFirst t loc =
  loc { content = F t, lefts = [], rights = subForest (tree loc)
      , parents = downParents loc }

-- | Insert a tree as the last child of the current position.
-- The new tree becomes the current tree.
insertDownLast :: Tree a -> TreePos Full a -> TreePos Full a
insertDownLast t loc =
  loc { content = F t, lefts = reverse (subForest (tree loc)), rights = []
      , parents = downParents loc }

-- | Insert a tree as a particular child of the current location.
-- Children are numbered from 0.
-- The new tree becomes the current tree.
insertDownAt :: Int -> Tree a -> TreePos Full a -> Maybe (TreePos Full a)
insertDownAt n t loc =
  do (ls,x,rs) <- splitChildren (subForest (tree loc)) n
     return Loc { content = F t, lefts = ls, rights = x : rs
                , parents = downParents loc }

-- | Delete the current node.  The new position is:
--   * the right sibling, or if none
--   * the left sibling, or if none
--   * the parent.
delete :: TreePos Full a -> Maybe (TreePos Full a)
delete loc =
  case rights loc of
    t : ts -> Just loc { content = F t, rights = ts }
    _ -> case lefts loc of
           t : ts -> Just loc { content = F t, lefts = ts }
           _ -> do loc1 <- parent loc
                   return $ modifyTree (\t -> t { subForest = [] }) loc1


-- | private: Gets the 'n'th element of a list.
-- Also returns the preceeding elements (reversed) and the folloing elements.
splitChildren :: [a] -> Int -> Maybe ([a],a,[a])
splitChildren _ n | n < 0 = Nothing
splitChildren cs pos = loop [] cs pos
  where loop acc (x:xs) 0 = Just (acc,x,xs)
        loop acc (x:xs) n = loop (x:acc) xs $! n-1
        loop _ _ _        = Nothing

-- | private: combChildren ls x ys = reverse ls ++ [x] ++ ys
combChildren :: [a] -> a -> [a] -> [a]
combChildren ls t rs = foldl (flip (:)) (t:rs) ls

