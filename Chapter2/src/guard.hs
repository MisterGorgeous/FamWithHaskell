{-ifibonacci :: Integer -> Maybe Integer 
ifibonacci n = if n < 0               
               then Nothing               
               else case n of                   
					0 -> Just 0                    
					1 -> Just 1                    
					n -> let Just f1 = ifibonacci (n-1),           
							 Just f2 = ifibonacci (n-2)                        
						 in Just (f1 + f2)
-}						 
ifibonacci n | n < 0     = Nothing 
ifibonacci 0             = Just 0 
ifibonacci 1             = Just 1 
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))  
                           in Just (f1 + f2)			 
{-akerman 0 n                = Just (n + 1)
akerman m 0 | m > 0        =  akerman ((m - 1),1)
akerman m n | m > 0, n > 0 = akerman (m - 1, akerman (m,n - 1))
akerman m n | otherwise    = Nothing-}

unoz list =
	  case list of
	  []     -> ([],[])	  
	  (x:xs) -> ((fst x):(fst (unoz(xs))),(snd x) :(snd (unoz(xs))))
	  (x:[]) -> ([fst x],[snd x])
l :: a -> [a]
l y = [y]