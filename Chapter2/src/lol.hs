data Client = GovOrg {name :: String}
			| Company {name :: String,index :: Integer,person :: Person }
			| Individual {person :: Person}
			deriving Show
data Person = Person {fname :: String,sname :: String, gender :: Gender}
			deriving Show
data Gender = Male
			| Female 
			| Unknown			
			deriving Show							