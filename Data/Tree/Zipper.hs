--
-- Copynext (c) Krasimir Angelov 2008.
-- Copynext (c) Iavor S. Diatchki 2008.
--
-- Generic zipper implementation for Data.Tree
--
--

module Data.Tree.Zipper
  ( TreePos
  , PosType, Empty, Full

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
-- The parameter 't' inidcates if the position is pointing to
-- a specific tree (if 't' is 'Full'), or if it is pointing in-between
-- trees (if 't' is 'Empty').
data TreePos t a  = Loc
  { _content   :: t a        -- ^ The currently selected tree.
  , _before    :: Forest a
  , _after     :: Forest a
  , _parents   :: [(Forest a, a, Forest a)]
  } deriving (Read,Show,Eq)


-- | Siblings before this position, closest first.
before         :: PosType t => TreePos t a -> Forest a
before          = _before

-- | Siblings after this position, closest first.
after          :: PosType t => TreePos t a -> Forest a
after           = _after

-- | The contexts of the parents for this position.
parents        :: PosType t => TreePos t a -> [(Forest a, a, Forest a)]
parents         = _parents

-- | Position which does not point to a tree (e.g., it is between two trees).
data Empty a    = E deriving (Read,Show,Eq)

-- | Position which points to a tree.
newtype Full a  = F { unF :: Tree a } deriving (Read,Show,Eq)


-- | Positions may be either 'Full' or 'Empty'.
class PosType t where
  _prev      :: TreePos t a -> Maybe (TreePos t a)
  _next      :: TreePos t a -> Maybe (TreePos t a)
  _forest    :: TreePos t a -> Forest a


instance PosType Full where
  _prev       = prevTree . prevSpace
  _next       = nextTree . nextSpace
  _forest loc = foldl (flip (:)) (tree loc : after loc) (before loc)

instance PosType Empty where
  _prev       = fmap prevSpace . prevTree
  _next       = fmap nextSpace . nextTree
  _forest loc = foldl (flip (:)) (after loc) (before loc)




-- XXX: We do this because haddock insist on placing methods
-- in the class...

-- | The sibling before this location.
prev    :: PosType t => TreePos t a -> Maybe (TreePos t a)
prev     = _prev

-- | The sibling after this location.
next     :: PosType t => TreePos t a -> Maybe (TreePos t a)
next      = _next

-- | All trees at this location
-- (i.e., the current tree---if any---and its siblings).
forest   :: PosType t => TreePos t a -> Forest a
forest    = _forest





-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: PosType t => TreePos t a -> Maybe (TreePos Full a)
parent loc =
  case parents loc of
    (ls,a,rs) : ps -> Just
      Loc { _content  = F (Node a (forest loc))
          , _before   = ls
          , _after    = rs
          , _parents  = ps
          }
    [] -> Nothing


-- | The top-most parent of the given location.
root :: TreePos Full a -> TreePos Full a
root loc = maybe loc root (parent loc)

-- | The space immediately before this location.
prevSpace :: TreePos Full a -> TreePos Empty a
prevSpace loc = loc { _content = E, _after = tree loc : after loc }

-- | The tree before this location, if any.
prevTree :: TreePos Empty a -> Maybe (TreePos Full a)
prevTree loc =
  case before loc of
    t : ts -> Just loc { _content = F t, _before = ts }
    []     -> Nothing


-- | The space immediately after this location.
nextSpace :: TreePos Full a -> TreePos Empty a
nextSpace loc = loc { _content = E, _before = tree loc : before loc }


-- | The tree after this location, if any.
nextTree :: TreePos Empty a -> Maybe (TreePos Full a)
nextTree loc =
  case after loc of
    t : ts -> Just loc { _content = F t, _after = ts }
    []     -> Nothing


-- | The location at the beginning of the forest of children.
children :: TreePos Full a -> TreePos Empty a
children loc =
  Loc { _content  = E
      , _before   = []
      , _after    = subForest (tree loc)
      , _parents  = (before loc, rootLabel (tree loc), after loc)
                  : parents loc
      }

-- | The first space in the current forest.
first :: TreePos Empty a -> TreePos Empty a
first loc = loc { _content  = E
                , _before   = []
                , _after    = reverse (before loc) ++ after loc
                }

-- | The last space in the current forest.
last :: TreePos Empty a -> TreePos Empty a
last loc = loc { _content = E
               , _before  = reverse (after loc) ++ before loc
               , _after   = []
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
fromTree t = Loc { _content = F t, _before = [], _after = [], _parents = [] }

-- | The location at the beginning of the forest.
fromForest :: Forest a -> TreePos Empty a
fromForest ts = Loc { _content = E, _before = [], _after = ts, _parents = [] }

-- | The tree containing this location.
toTree :: TreePos Full a -> Tree a
toTree loc = tree (root loc)

-- | The forest containing this location.
toForest :: PosType t => TreePos t a -> Forest a
toForest loc = case parent loc of
                 Nothing -> forest loc
                 Just p  -> toForest p -- polymprphic recursion


-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the tree?
isRoot :: PosType t => TreePos t a -> Bool
isRoot loc = null (parents loc)

-- | Are we the first position (of its kind) in a forest.
isFirst :: PosType t => TreePos t a -> Bool
isFirst loc = null (before loc)

-- | Are we the last position (of its kind) in a forest.
isLast :: PosType t => TreePos t a -> Bool
isLast loc = null (after loc)

-- | Are we at the bottom of the tree?
isLeaf :: TreePos Full a -> Bool
isLeaf loc = null (subForest (tree loc))

-- | Do we have a parent?
isContained :: PosType t => TreePos t a -> Bool
isContained loc = not (isRoot loc)

-- | Do we have children?
hasChildren :: TreePos Full a -> Bool
hasChildren loc = not (isLeaf loc)


-- The current tree -----------------------------------------------------------


-- | The selected tree.
tree :: TreePos Full a -> Tree a
tree x = unF (_content x)

-- | The current label.
label :: TreePos Full a -> a
label loc = rootLabel (tree loc)

-- | Insert a new tree at the current position.
insert :: Tree a -> TreePos Empty a -> TreePos Full a
insert t loc = loc { _content = F t }

-- | Remove the tree at the current position.
delete :: TreePos Full a -> TreePos Empty a
delete loc = loc { _content = E }



-- | Change the current tree.
setTree :: Tree a -> TreePos Full a -> TreePos Full a
setTree t loc = loc { _content = F t }

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





