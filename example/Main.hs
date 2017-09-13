{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Applicative (Applicative, (<$>))
import Control.Lens (Lens', lens, (^.), (%%~))
import Control.Monad.Haskey
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.BTree.Alloc (AllocM, AllocReaderM)
import Data.BTree.Impure (Tree, insertTree, lookupTree, toList)
import Data.BTree.Primitives (Value)
import Data.Binary (Binary)
import Data.Foldable (foldlM)
import Data.Int (Int64)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)
import qualified Data.BTree.Impure as Tree

import Database.Haskey.Alloc.Concurrent (Root)

import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Our application monad transformer stack, includes the HaskeyT monad
-- transformer.
--------------------------------------------------------------------------------

newtype App a = AppT (ReaderT String (HaskeyT Schema IO) a)
              deriving (Functor, Applicative, Monad, MonadIO,
                        MonadHaskey Schema, MonadReader String)

runApp :: App a
       -> String
       -> ConcurrentDb Schema
       -> FileStoreConfig
       -> IO a
runApp (AppT m) r = runHaskeyT (runReaderT m r)

--------------------------------------------------------------------------------
-- Definition of our custom schema. As well as query and modify functions.
--------------------------------------------------------------------------------

data Tweet = Tweet {
    tweetUser :: !Text
  , tweetContent :: !Text
  } deriving (Generic, Show, Typeable)

instance Binary Tweet
instance Value Tweet

data User = User {
    userName :: !Text
  , userEmail :: !Text
  } deriving (Generic, Show, Typeable)

instance Binary User
instance Value User

data Schema = Schema {
    _schemaTweets :: Tree Int64 Tweet
  , _schemaUsers :: Tree Text User
  } deriving (Generic, Show, Typeable)

instance Binary Schema
instance Value Schema
instance Root Schema

emptySchema :: Schema
emptySchema = Schema Tree.empty Tree.empty

schemaTweets :: Lens' Schema (Tree Int64 Tweet)
schemaTweets = lens _schemaTweets $ \s x -> s { _schemaTweets = x }

schemaUsers :: Lens' Schema (Tree Text User)
schemaUsers = lens _schemaUsers $ \s x -> s { _schemaUsers = x }

-- | Insert or update a tweet.
insertTweet :: AllocM n => Int64 -> Tweet -> Schema -> n Schema
insertTweet k v = schemaTweets %%~ insertTree k v

-- | Query all tweets.
queryAllTweets :: AllocReaderM n => Schema -> n [(Int64, Tweet)]
queryAllTweets root = toList (root ^. schemaTweets)

-- | Query a tweet.
queryTweet :: AllocReaderM n => Int64 -> Schema -> n (Maybe Tweet)
queryTweet k root = lookupTree k (root ^. schemaTweets)

-- | Insert a new user.
insertUser :: AllocM n => Text -> User -> Schema -> n Schema
insertUser k v = schemaUsers %%~ insertTree k v

-- | Quer a user.
queryUser :: AllocReaderM n => Text -> Schema -> n (Maybe User)
queryUser userId root = lookupTree userId (root ^. schemaUsers)

--------------------------------------------------------------------------------
-- Our main application.
--------------------------------------------------------------------------------
main :: IO ()
main = do
    let db = "/tmp/mtl-example.haskey"
    putStrLn $ "Using " ++ db
    main' db

main' :: FilePath -> IO ()
main' fp = do
    db <- flip runFileStoreT defFileStoreConfig $
        openConcurrentDb hnds >>= \case
            Nothing -> createConcurrentDb hnds emptySchema
            Just db -> return db

    runApp app "Hello World!" db defFileStoreConfig
  where
    hnds = concurrentHandles fp

app :: App ()
app = insertDefaultTweets >> printTweetsWithUser

insertDefaultTweets :: App ()
insertDefaultTweets = do
    transact_ $ \schema ->
        foldlM (flip $ uncurry insertUser) schema users
        >>= commit_

    transact_ $ \schema ->
        foldlM (flip $ uncurry insertTweet) schema tweets
        >>= commit_
  where
    users = [("foo", User "Foo" "foo@example.org"),
             ("bar", User "Bar" "bar@example.org")]
    tweets = [(1, Tweet "foo" "Hey, I'm Foo!"),
              (2, Tweet "bar" "Hey, I'm Bar!"),
              (3, Tweet "foo" "I like you, Bar!")]

printTweetsWithUser :: App ()
printTweetsWithUser = do
    tweets <- map snd <$> transactReadOnly queryAllTweets
    users  <- mapM (\t -> transactReadOnly $ queryUser (tweetUser t)) tweets
    mapM_ print' $ zip users tweets
  where
    print' (Just user, tweet) = liftIO . putStrLn $ unpack (userName user) ++ ": " ++ unpack (tweetContent tweet)
    print' (Nothing  , tweet) = liftIO . putStrLn $ "?: " ++ unpack (tweetContent tweet)
