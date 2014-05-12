import Control.Monad

data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
} deriving (Show)

lookup' key alist = join $ lookup key alist

review alist = liftM3 MovieReview (lookup' "title" alist)
                                  (lookup' "user" alist)
                                  (lookup' "review" alist)
