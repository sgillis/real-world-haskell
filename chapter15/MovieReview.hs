import Control.Monad

data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
} deriving (Show)

lookup' key alist = join $ lookup key alist

review alist = MovieReview `liftM` (lookup' "title" alist)
                           `ap` (lookup' "user" alist)
                           `ap` (lookup' "review" alist)
