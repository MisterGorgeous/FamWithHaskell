{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
data TimeMachine = TimeMachine { producer :: String, 
								 model :: Integer, 
								 name:: String, 
								 ability :: Bool, 
								 price :: Double }	
				deriving Show							
--disc :: [a] -> [a]
disc list = case list of
			[] -> []
			(x:xs) -> (change(x)):(disc(xs))
			--(x:[]) -> change(x):[]
change :: TimeMachine -> TimeMachine
change t@(TimeMachine {price = n}) = let newprice = n * 0.8
								     in t{price = newprice}		
									 