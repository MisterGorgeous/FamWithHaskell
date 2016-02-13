f :: [[Char]] -> Bool
f l = if null l then True else False
--lst +++ lst1 :: ([[Char]] -> [[Char]]) -> [[Char]] 
lst +++ lst1 = if null lst {-Check for emptyness-}
			    then lst1 
				else (head lst) : (tail lst +++ lst1)
reverse2 :: [Int] -> [Int]
reverse2 list = if null list {-Check for emptyness-}
			    then []
				else reverse2 (tail list) +++ [head list]
maxmin list2 = let h = head list2
			in if null (tail list2)
			   then (h,h)
			   else ( if h > t_max then h else t_max
					 ,if h < t_min then h else t_min )
					 where { t = maxmin (tail list2) ;
						     t_max = fst t ;
						     t_min = snd t }
data Client = GovOrg String
			| Company String Integer Person String 
			| Individual Person Gender
			deriving Show
data Person = String String
			deriving Show
data Gender = Male 
			| Female 
			| Unknown			
			deriving Show			
clientName :: Client -> String
clientName client = case client of
		   GovOrg name                 -> name
		   Company name id person resp -> name
	       Individual (Person fName lName _) _ -> fName ++ " " ++ lName