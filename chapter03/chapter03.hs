data Direction = RightTurn | LeftTurn | Straight
                 deriving (Show)

data Point = Point { x :: Double,
                     y :: Double }
                   deriving (Show)

turnMade :: Point -> Point -> Point -> Direction
turnMade (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | z > 0     = LeftTurn
    | z < 0     = RightTurn
    | otherwise = Straight
    where z = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

main = do
    let p1 = Point (-1.0) 0.0
    let p2 = Point 0.0 0.0
    let p3 = Point (0.5) (1.0)

    print $ turnMade p1 p2 p3
