data Gender = Male 
			| Female 
			| Unknown			
			deriving Show	
data Client = GovOrg String
			| Company String Integer Person String 
			| Individual Person
			deriving Show
data Person = Person String String Gender
			deriving Show
		
clientName :: Client -> String
clientName (GovOrg name) = name 
clientName (Company name _ _ _) = name 
clientName (Individual (Person fName lName _)) = fName ++ " " ++ lName
	
value list = let h = (0,0)
			in if null (tail list)
			   then incr (head list)
			   else (incr (head list)) +++ (value (tail list)) 
			   
incr :: Client -> (Integer,Integer)
incr (Individual (Person _ _ Male)) = (1,0)
incr (Individual (Person _ _ Female)) = (0,1)
incr (Individual (Person _ _ Unknown)) = (0,0)  	

(a,b) +++ (c,d) = (a + c , b + d)				 