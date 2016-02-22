import Data.List
elemFind :: Eq a => a -> [a] -> Bool
elemFind x xs = case res of
			    Nothing -> False
			    _       -> True
			  where res = find (== x) xs