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

--akerman :: Integer -> Integer -> Integer
akerman 0 0 = 1
akerman m 0 | m > 0 = akerman (m - 1,0)
akerman m n | m > 0, n > 0 = akerman (m - 1, akerman (m, n - 1))
--akerman m n | otherwise = Nothing
 