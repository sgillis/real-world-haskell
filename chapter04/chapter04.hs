import Data.Char (digitToInt, isSpace)

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail [] = Nothing

safeLast :: [a] -> Maybe a
safeLast xs = safeHead $ reverse xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs
    | null snd  = [fst]
    | otherwise = fst : splitWith f (tail snd)
    where (fst, snd) = break f xs

asInt :: String -> Int
asInt "" = 0
asInt ('-':xs) = -(asInt xs)
asInt xs = foldl step 0 xs
    where step acc x = acc*10 + digitToInt x

concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x       = x : takeWhile' f xs
    | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr step [] xs
    where step x acc | f x       = x : acc
                     | otherwise = []

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy f xs = foldl step [] xs
    where step [] x  = [[x]]
          step acc x | f x (head $ last acc) = (init acc) ++ [(last acc ++ [x])]
                     | otherwise             = acc ++ [[x]]

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr (||) False (map p xs)

words' :: String -> [String]
words' x = fst(foldr step ([], False) x)
    where step x (acc, True)  | isSpace x = (acc, True)
                              | otherwise = ([x] : acc, False)
          step x ([], False)  | isSpace x = ([], True)
                              | otherwise = ([[x]], False)
          step x (acc, False) | isSpace x = (acc, True)
                              | otherwise = ((x : (head acc)) : tail acc, False)

unlines' :: [String] -> String
unlines' xs = foldr step "" xs
    where step x acc = x ++ "\n" ++ acc
