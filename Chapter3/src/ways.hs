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
			
clientName :: Client -> String
clientName (GovOrg name) = name 
clientName (Company name _ _ _) = name 
clientName (Individual (Person fName lName _)) = fName ++ " " ++ lName

prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * ( prod xs )
		   
shortName :: [Client] -> String
shortName [] = ""
shortName (x:[]) = clientName x
shortName (x:xs) = min (clientName x)  (shortName xs)

conAll :: [Bool] -> Bool
conAll [] = False
conAll (x:[]) = x
conAll (x:xs) = x && (conAll xs)

prod1 :: Num a => [a] -> a
prod1 = foldr (*) 1

shortName1 :: [Client] -> String
shortName1  = foldr (\x acc -> min (clientName x) acc) "Sergei Slovo" 

conAll1 :: [Bool] -> Bool
conAll1 = foldr (&&) True