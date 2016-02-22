data Client = GovOrg String
			| Company String Integer Person String 
			| Individual Person
			deriving Show
data Person = Person String String Gender
			deriving Show
data Gender = Male 
			| Female 
			| Unknown			
			deriving Show				

--index [x] = [(0,x)]
swapTriple (x,y,z) = (y,z,x)
index (x:xs) = let indexed@((n,_):_) = index xs
			    in (n+1,x):indexed	
--filterOnes ::  ([a] -> b) -> [a]
filterOnes n = filter (\x -> not (x == n))
filterIndiv = filter (\(Individual (Person _ _ g)) -> case g of  
															Male -> True
															_    -> False)
--7x^2+20*x-18 	
(***) :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
f *** g = \(x,y) -> (f x , g y)

cury :: ((a,b) -> c) -> a -> b -> c
cury f = \x y -> f (x,y)

uncury :: (a -> b -> c) -> (a,b) -> c
uncury v = \(x,y) -> v x y

duplicate :: a -> (a,a)
duplicate x = (x,x) 

poly = uncurry (+) . (((*7) . (^2)) *** (*20)) . duplicate
														
formula1 :: Integer -> Integer 
formula1 = uncurry (+) . ( ((*7) . (+2)) *** (*3) ) . duplicate														