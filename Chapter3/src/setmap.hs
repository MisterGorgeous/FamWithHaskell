data A = A { nameA :: String } deriving (Show, Eq)
data B = B { nameB :: String } deriving (Show, Eq)

class Named a where
    getName :: a -> String

instance Named A where
    getName = nameA

instance Named B where
    getName = nameB
