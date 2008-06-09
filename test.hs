import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Test.QuickCheck
import System.Random
import Text.Show.Functions

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree
    where
      arbTree n = do lbl <- arbitrary
                     children <- resize (n-1) arbitrary
                     return (Node lbl children)

  coarbitrary t =
    coarbitrary (rootLabel t) .
    variant (length (subForest t)) .
    flip (foldr coarbitrary) (subForest t)

instance Arbitrary a => Arbitrary (TreeLoc a) where
  arbitrary = do
    tree    <- resize 8 arbitrary
    lefts   <- resize 5 arbitrary
    rights  <- resize 5 arbitrary
    parents <- resize 3 arbitrary
    return (Loc tree lefts rights parents)

prop_LeftRight :: TreeLoc Int -> Property
prop_LeftRight loc = label "prop_LeftRight" $
  case left loc of
    Just lloc -> right lloc == Just loc
    Nothing   -> True

prop_LeftFirst :: TreeLoc Int -> Property
prop_LeftFirst loc = label "prop_LeftFirst" $
  isFirst loc ==> left loc == Nothing

prop_RightLast :: TreeLoc Int -> Property
prop_RightLast loc = label "prop_RightLast" $
  isLast loc ==> right loc == Nothing

prop_RootParent :: TreeLoc Int -> Property
prop_RootParent loc = label "prop_RootParent" $
  isRoot loc ==> parent loc == Nothing

prop_FirstChild :: TreeLoc Int -> Property
prop_FirstChild loc = label "prop_FirstChild" $
  case firstChild loc of
    Just floc -> parent floc == Just loc && left floc == Nothing
    Nothing   -> isLeaf loc

prop_LastChild :: TreeLoc Int -> Property
prop_LastChild loc = label "prop_LastChild" $
  case lastChild loc of
    Just lloc -> parent lloc == Just loc && right lloc == Nothing
    Nothing   -> isLeaf loc

prop_FindChild :: TreeLoc Int -> Property
prop_FindChild loc = label "prop_FindChild" $
  forAll arbitrary (\f -> maybe True (\sloc -> f (tree sloc) && parent sloc == Just loc) (findChild f loc))

prop_SetGetLabel :: TreeLoc Int -> Property
prop_SetGetLabel loc = label "prop_SetGetLabel" $
  forAll arbitrary (\x -> getLabel (setLabel x loc) == x)

prop_ModifyLabel :: TreeLoc Int -> Property
prop_ModifyLabel loc = label "prop_ModifyLabel" $
  forAll arbitrary (\f -> getLabel (modifyLabel f loc) == f (getLabel loc))

prop_UpDown :: TreeLoc Int -> Property
prop_UpDown loc = label "prop_UpDown" $
  case parent loc of
    Just ploc -> getChild (getNodeIndex loc) ploc == Just loc
    Nothing   -> True

prop_RootChild :: TreeLoc Int -> Property
prop_RootChild loc = label "prop_RootChild" $
  isChild loc == not (isRoot loc)

prop_ChildrenLeaf :: TreeLoc Int -> Property
prop_ChildrenLeaf loc = label "prop_ChildrenLeaf" $
  hasChildren loc == not (isLeaf loc)

prop_FromToTree :: TreeLoc Int -> Property
prop_FromToTree loc = label "prop_FromToTree" $
  tree (fromTree (toTree loc)) == tree (root loc)

prop_FromToForest :: TreeLoc Int -> Property
prop_FromToForest loc = label "prop_FromToForest" $
  isFirst (root loc) ==> fromForest (toForest loc) == Just (root loc)

prop_FromTree :: Tree Int -> Property
prop_FromTree tree = label "prop_FromTree" $
  left   (fromTree tree) == Nothing &&
  right  (fromTree tree) == Nothing &&
  parent (fromTree tree) == Nothing

prop_FromForest :: Property
prop_FromForest = label "prop_FromForest" $
  fromForest ([] :: Forest Int) == Nothing

prop_InsertLeft :: TreeLoc Int -> Property
prop_InsertLeft loc = label "prop_InsertLeft" $
  forAll (resize 10 arbitrary) $ \t ->
    tree (insertLeft t loc) == t && 
    rights (insertLeft t loc) == tree loc : rights loc

prop_InsertRight :: TreeLoc Int -> Property
prop_InsertRight loc = label "prop_InsertRight" $
  forAll (resize 10 arbitrary) $ \t ->
    tree (insertRight t loc) == t && 
    lefts (insertRight t loc) == tree loc : lefts loc

prop_InsertDownFirst :: TreeLoc Int -> Property
prop_InsertDownFirst loc = label "prop_InsertDownFirst" $
  forAll (resize 10 arbitrary) $ \t ->
    tree (insertDownFirst t loc) == t && 
    left (insertDownFirst t loc) == Nothing &&
    fmap getLabel (parent (insertDownFirst t loc)) == Just (getLabel loc) &&
    fmap tree (right (insertDownFirst t loc)) == fmap tree (firstChild loc)

prop_InsertDownLast :: TreeLoc Int -> Property
prop_InsertDownLast loc = label "prop_InsertDownLast" $
  forAll (resize 10 arbitrary) $ \t ->
    tree (insertDownLast t loc) == t && 
    right (insertDownLast t loc) == Nothing &&
    fmap getLabel (parent (insertDownLast t loc)) == Just (getLabel loc) &&
    fmap tree (left (insertDownLast t loc)) == fmap tree (lastChild loc)

prop_InsertDownAt :: TreeLoc Int -> Property
prop_InsertDownAt loc = label "prop_InsertDownAt" $
  forAll (resize 10 arbitrary) $ \t ->
  forAll (resize 10 arbitrary) $ \n ->
    maybe t tree (insertDownAt n t loc) == t && 
    fmap lefts  (getChild n loc) == fmap lefts  (insertDownAt n t loc) &&

    fmap tree   (getChild n loc) == fmap tree   (insertDownAt n t loc >>= right) &&
    fmap rights (getChild n loc) == fmap rights (insertDownAt n t loc >>= right) &&
    maybe (getLabel loc) getLabel (insertDownAt n t loc >>= parent) == getLabel loc

prop_Delete :: TreeLoc Int -> Property
prop_Delete loc = label "prop_Delete" $
  if not (isLast loc)
    then fmap (\loc -> tree loc : rights loc) (delete loc) == Just (rights loc) &&
         fmap lefts (delete loc) == Just (lefts loc)
    else if not (isFirst loc)
           then fmap rights (delete loc) == Just (rights loc) &&
                fmap (\loc -> tree loc : lefts loc) (delete loc) == Just (lefts loc)
           else fmap (insertDownFirst (tree loc)) (delete loc) == Just loc
  

main = do
  testProp prop_LeftRight
  testProp prop_LeftFirst
  testProp prop_RightLast
  testProp prop_RootParent
  testProp prop_FirstChild
  testProp prop_LastChild
  testProp prop_FindChild
  testProp prop_SetGetLabel
  testProp prop_ModifyLabel
  testProp prop_UpDown
  testProp prop_RootChild
  testProp prop_ChildrenLeaf
  testProp prop_FromToTree
  testProp prop_FromToForest 
  testProp prop_FromTree
  testProp prop_FromForest
  testProp prop_InsertLeft
  testProp prop_InsertRight
  testProp prop_InsertDownFirst
  testProp prop_InsertDownLast
  testProp prop_InsertDownAt

  testProp prop_Delete

testProp :: Testable a => a -> IO ()
testProp = check (defaultConfig{-configEvery = \n args -> ""-})
