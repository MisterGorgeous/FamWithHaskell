data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
					| Leaf2 
					deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a 
treeFind2 t (Node2 v l r) = case compare t v of 
							EQ -> Just v 
							LT -> treeFind2 t l 
							GT -> treeFind2 t r 
treeFind2 _ Leaf2 = Nothing

treeInsert :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert t Leaf2 = Node2 t Leaf2 Leaf2
treeInsert t n@(Node2 v l r) = case compare t v of
								EQ -> n
								GT -> Node2 v l (treeInsert t r) 
								LT -> Node2 v (treeInsert t l) r
								
treeUnion :: Ord a => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
treeUnion x (Node2 a l r) = treeUnion (treeUnion (treeInsert a x) l) r
treeUnion x  Leaf2        = x
treeUnion Leaf2 x         = x

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)   
                  | Leaf3                     
				  deriving (Show, Eq, Ord) 		
treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c 
treeInsert3 v c (Node3 v2 c2 l r) = case compare v v2 of                       
									EQ -> Node3 v2 c2 l r                     
									LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r                
									GT -> Node3 v2 (min c c2) l (treeInsert3 v c r) 
treeInsert3 v c Leaf3             = Node3 v c Leaf3 Leaf3

newtype	MyMaybe a = MyMaybe (Maybe a)
					deriving Show

instance Functor MyMaybe where
	fmap f (MyMaybe Nothing) = MyMaybe Nothing 
	fmap f (MyMaybe (Just a)) = MyMaybe (Just (f a))

{-instance Functor BinaryTree2 where 
	--fmap :: (a -> b) -> f a -> f b
	fmap f Leaf2 = Leaf2
	fmap f (Node2 v l r) = treeUnion (Node2 (f v) Leaf2 Leaf2) (treeUnion  (fmap f l) (fmap f r))-}

instance Foldable Maybe  where
		foldr (\acc (Maybe a) -> if Nothing then acc + 0
												 else acc + a) 0 
		