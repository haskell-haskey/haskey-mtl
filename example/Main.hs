{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ((>=>))
import Control.Monad.Haskey
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.BTree.Impure (Tree, insertTree)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.BTree.Impure as Tree

type Root = Tree Int32 ByteString

newtype App a = AppT { runApp :: HaskeyT Root IO a }
              deriving (Functor, Applicative, Monad, MonadIO,
                        MonadHaskey Root)

main :: IO ()
main = do
    let db = "/tmp/mtl-example.haskey"
    putStrLn $ "Using " ++ db
    main' db

main' :: FilePath -> IO ()
main' fp = do
    db <- flip runFileStoreT defFileStoreConfig $
        openConcurrentDb hnds >>= \case
            Nothing -> createConcurrentDb hnds Tree.empty
            Just db -> return db

    runFileStoreHaskeyT (runApp app) db defFileStoreConfig
  where
    hnds = concurrentHandles fp

app :: App ()
app = do
    writeValues [(1, "Hello!"), (2, "World!")]
    readAndPrint

writeValues :: [(Int32, ByteString)] -> App ()
writeValues = mapM_ tx
  where
    tx :: (Int32, ByteString) -> App ()
    tx (k, v) = transact_ $ insertTree k v >=> commit_

readAndPrint :: App ()
readAndPrint = do
    values <- transactReadOnly Tree.toList
    liftIO $ print values
