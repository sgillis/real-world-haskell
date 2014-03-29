import qualified Data.List

len :: [a] -> Int
len (x:xs) = 1 + len xs
len [] = 0

mean :: [Double] -> Double
mean xs = (sum xs) / (fromIntegral (length xs))

createPalindrome :: [a] -> [a]
createPalindrome xs = xs ++ (reverse xs)

verifyPalindrome :: (Eq a) => [a] -> Bool
verifyPalindrome (x:xs)
    | x == last xs   = True && (verifyPalindrome (init xs))
    | otherwise      = False
verifyPalindrome []  = True

verifyPalindrome' :: (Eq a) => [a] -> Bool
verifyPalindrome' xs = xs == reverse xs

compareListLength :: [a] -> [a] -> Ordering
compareListLength xs ys = compare (length xs) (length ys)

sortByListLength :: [[a]] -> [[a]]
sortByListLength xs = Data.List.sortBy compareListLength xs

intersperse :: [a] -> [[a]] -> [a]
intersperse seperator [] = []
intersperse seperator [xs] = xs
intersperse seperator (xs:xss) = xs ++ seperator ++ intersperse seperator xss

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ Empty Empty) = 1
treeHeight (Node x y z) = 1 + max (treeHeight y) (treeHeight z)

data Direction = RightTurn | LeftTurn | Straight
                 deriving (Show)

data Point = Point { x :: Double,
                     y :: Double }
                   deriving (Show)

type Path = [Point]

turnMade :: Point -> Point -> Point -> Direction
turnMade (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | z > 0     = LeftTurn
    | z < 0     = RightTurn
    | otherwise = Straight
    where z = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

path = [(Point (-1.0) 0.0),
        (Point 0.0 0.0),
        (Point 0.5 0.25),
        (Point 1.0 0.5),
        (Point 0.0 1.0),
        (Point 0.0 2.0),
        (Point (-2.0) 0.5),
        (Point (-3.0) (-1.0))]

pathTurns :: Path -> [Direction]
pathTurns (p1:p2:p3:ps) = (turnMade p1 p2 p3) : pathTurns (p2:p3:ps)
pathTurns _ = []
