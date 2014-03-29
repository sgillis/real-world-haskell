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
