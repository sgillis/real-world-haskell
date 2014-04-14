import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Word (Word8)

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
        Nothing -> bail "no more input"
        Just (byte, remainder) ->
            putState newState ==> \_ -> identity byte
          where newState = initState { string = remainder, offset = newOffset }
                newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
            "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
    case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result
