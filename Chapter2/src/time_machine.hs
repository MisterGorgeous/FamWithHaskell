{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
data TimeMachine = TimeMachine { producer :: String, 
								 model :: Integer, 
								 name:: String, 
								 ability :: Bool, 
								 priceT :: Double }	
				deriving (Show,Eq)					
--disc :: [a] -> [a]
disc list = case list of
			[] -> []
			(x:xs) -> (change(x)):(disc(xs))
			--(x:[]) -> change(x):[]
change :: TimeMachine -> TimeMachine
change t@(TimeMachine {priceT = n}) = let newprice = n * 0.8
								     in t{priceT = newprice}		
									 
data Guides = Guides {content :: String,
					  priceG :: Double }
					  deriving (Show,Eq)
data Service = Service { description :: String,
						 priceS :: Double}
						 deriving (Show,Eq)
class Priceable a where
	getPrice ::  a -> Double


instance Priceable TimeMachine where
	getPrice = priceT						 

instance Priceable Guides where
	getPrice = priceG	

instance Priceable Service where
	getPrice = priceS	

totalPrice :: Priceable p => [p] -> Double
totalPrice [] = 0
totalPrice (x:xs) = (getPrice x) + (totalPrice xs)