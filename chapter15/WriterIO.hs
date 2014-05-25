{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             MultiParamTypeClasses,
             GeneralizedNewtypeDeriving #-}

import Control.Monad.Writer
import MonadHandle
import MonadHandleIO
import System.IO

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Monad, MonadWriter [Event])

instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return ""

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW
