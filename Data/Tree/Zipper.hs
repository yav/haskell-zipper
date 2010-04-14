--
-- Copynext (c) Krasimir Angelov 2008.
-- Copynext (c) Iavor S. Diatchki 2008.
--
-- Generic zipper implementation for Data.Tree
--
--

module Data.Tree.Zipper
  ( TreePos
  , TreePtr, Empty, Full

  -- * Context
  , before, after, forest, tree, label, parents

  -- * Conversions
  , fromTree
  , fromForest
  , toForest
  , toTree

  -- * Moving around
  , parent
  , root
  , prevSpace, prevTree, prev, first
  , nextSpace, nextTree, next, last
  , children, firstChild, lastChild

  -- * Node classification
  , isRoot
  , isFirst
  , isLast
  , isLeaf
  , isContained
  , hasChildren

  -- * Working with the current tree
  , insert
  , delete
  , setTree
  , modifyTree
  , modifyLabel
  , setLabel
  ) where

import Data.Tree
import Prelude hiding (last)

-- | A position within a 'Tree'.
data TreePos ptr a  = Loc
  { content   :: ptr a        -- ^ The currently selected tree.
  , before    :: Forest a     -- ^ Siblings before this position, closest first.
  , after     :: Forest a     -- ^ Siblings after this position, closest first.
  , parents   :: [(Forest a, a, Forest a)]
      -- ^ The contexts of the parents for this position.
  } deriving (Read,Show,Eq)

-- | Position which does not point to a tree (e.g., between two trees).
data Empty a    = E

-- | Position which points to a tree.
newtype Full a  = F { unF :: Tree a }


-- | Operations that work on both full and empty positions.
class TreePtr ptr where

  -- | The prev sibling of the given location.
  prev    :: TreePos ptr a -> Maybe (TreePos ptr a)

  -- | The next sibling of the given location.
  next    :: TreePos ptr a -> Maybe (TreePos ptr a)

  -- | The surrounding forest of this position (i.e., our neighbours).
  forest  :: TreePos ptr a -> Forest a



instance TreePtr Full where
  prev        = prevTree . prevSpace
  next       = nextTree . nextSpace
  forest loc  = foldl (flip (:)) (tree loc : after loc) (before loc)

instance TreePtr Empty where
  prev        = fmap prevSpace . prevTree
  next       = fmap nextSpace . nextTree
  forest loc  = foldl (flip (:)) (after loc) (before loc)





-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: TreePtr ptr => TreePos ptr a -> Maybe (TreePos Full a)
parent loc =
  case parents loc of
    (ls,a,rs) : ps -> Just
      Loc { content = F (Node a (forest loc))
          , before   = ls
          , after  = rs
          , parents = ps
          }
    [] -> Nothing


-- | The top-most parent of the given location.
root :: TreePos Full a -> TreePos Full a
root loc = maybe loc root (parent loc)

-- | The space to the prev of the current location.
prevSpace :: TreePos Full a -> TreePos Empty a
prevSpace loc = loc { content = E, after = tree loc : after loc }

-- | The tree to the prev of the current space, if any.
prevTree :: TreePos Empty a -> Maybe (TreePos Full a)
prevTree loc =
  case before loc of
    t : ts -> Just loc { content = F t, before = ts }
    []     -> Nothing


-- | The space to the next of the current location.
nextSpace :: TreePos Full a -> TreePos Empty a
nextSpace loc = loc { content = E, before = tree loc : before loc }


-- | The tree to the next of the current space, if any.
nextTree :: TreePos Empty a -> Maybe (TreePos Full a)
nextTree loc =
  case after loc of
    t : ts -> Just loc { content = F t, after = ts }
    []     -> Nothing


children :: TreePos Full a -> TreePos Empty a
children loc =
  Loc { content = E
      , before   = []
      , after  = subForest (tree loc)
      , parents = (before loc, rootLabel (tree loc), after loc)
                : parents loc
      }

first :: TreePos Empty a -> TreePos Empty a
first loc = loc { content = E
                   , before   = []
                   , after  = reverse (before loc) ++ after loc
                   }



last :: TreePos Empty a -> TreePos Empty a
last loc = loc { content = E
                    , before   = reverse (after loc) ++ before loc
                    , after  = []
                    }


-- | The first child of the given location.
firstChild :: TreePos Full a -> Maybe (TreePos Full a)
firstChild = nextTree . children

-- | The last child of the given location.
lastChild :: TreePos Full a -> Maybe (TreePos Full a)
lastChild = prevTree . last . children






-- Conversions -----------------------------------------------------------------

-- | A location corresponding to the root of the given tree.
fromTree :: Tree a -> TreePos Full a
fromTree t = Loc { content = F t, before = [], after = [], parents = [] }

-- | The location at the beginning of the a forest.
fromForest :: Forest a -> TreePos Empty a
fromForest ts = Loc { content = E, before = [], after = ts, parents = [] }

-- | Computes the tree containing this location.
toTree :: TreePos Full a -> Tree a
toTree loc = tree (root loc)

-- | Computes the forest containing this location.
toForest :: TreePtr ptr => TreePos ptr a -> Forest a
toForest loc = case parent loc of
                 Nothing -> forest loc
                 Just p  -> toForest p -- polymprphic recursion


-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the tree?
isRoot :: TreePtr ptr => TreePos ptr a -> Bool
isRoot loc = null (parents loc)

-- | Are we the first position (of its kind) in a forest.
isFirst :: TreePos ptr a -> Bool
isFirst loc = null (before loc)

-- | Are we the last position (of its kind) in a forest.
isLast :: TreePos ptr a -> Bool
isLast loc = null (after loc)

-- | Are we at the bottom of the tree?
isLeaf :: TreePos Full a -> Bool
isLeaf loc = null (subForest (tree loc))

-- | Do we have a parent?
isContained :: TreePtr ptr => TreePos ptr a -> Bool
isContained loc = not (isRoot loc)

-- | Do we have children?
hasChildren :: TreePos Full a -> Bool
hasChildren loc = not (isLeaf loc)


-- The current tree -----------------------------------------------------------


-- | The selected tree.
tree :: TreePos Full a -> Tree a
tree x = unF (content x)

-- | The current label.
label :: TreePos Full a -> a
label loc = rootLabel (tree loc)

-- | Insert a new tree at the current position.
insert :: Tree a -> TreePos Empty a -> TreePos Full a
insert t loc = loc { content = F t }

-- | Remove the tree at the current position.
delete :: TreePos Full a -> TreePos Empty a
delete loc = loc { content = E }



-- | Change the current tree.
setTree :: Tree a -> TreePos Full a -> TreePos Full a
setTree t loc = loc { content = F t }

-- | Modify the current tree.
modifyTree :: (Tree a -> Tree a) -> TreePos Full a -> TreePos Full a
modifyTree f loc = setTree (f (tree loc)) loc

-- | Modify the label at the current node.
modifyLabel :: (a -> a) -> TreePos Full a -> TreePos Full a
modifyLabel f loc = setLabel (f (label loc)) loc

-- | Change the label at the current node.
setLabel :: a -> TreePos Full a -> TreePos Full a
setLabel v loc = modifyTree (\t -> t { rootLabel = v }) loc


--------------------------------------------------------------------------------





