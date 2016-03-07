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
			
instance Eq Person where
(x {fname = xf,sname = xs,gender = xg}) == (y {fname = yf,sname = ys,gender = yg}) = if ((xf == yf) && (xs == ys) && (xg == yg))
																				  then True
																		  else False


instance Eq Gender where
	Male == Male = True
	Female == Female = True
	_ == _ = False