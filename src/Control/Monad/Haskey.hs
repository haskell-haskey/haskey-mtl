{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
-- | A monad transformer supporting Haskey transactions.
module Control.Monad.Haskey (
  -- * Re-exports
  module Database.Haskey.Alloc.Transaction

  -- * Monad
, MonadHaskey(..)
, HaskeyT
, runFileStoreHaskeyT

  -- * Open and create (re-exports)
, FileStoreT
, runFileStoreT
, defFileStoreConfig
, concurrentHandles
, openConcurrentDb
, createConcurrentDb
) where

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.BTree.Alloc (AllocM, AllocReaderM)
import Database.Haskey.Alloc.Concurrent (ConcurrentDb, Root, Transaction,
                                         concurrentHandles,
                                         openConcurrentDb, createConcurrentDb)
import Database.Haskey.Alloc.Transaction
import Database.Haskey.Store.File (FileStoreT, runFileStoreT,
                                   FileStoreConfig, defFileStoreConfig)
import qualified Database.Haskey.Alloc.Concurrent as D

-- | A monad supporting database transactions.
--
-- The type @root@ is the data type holding the roots of the database trees.
class MonadHaskey root m | m -> root where
    transact :: Root root
             => (forall n. (AllocM n, MonadMask n) => root -> n (Transaction root a))
             -> m a

    transact_ :: Root root
              => (forall n. (AllocM n, MonadMask n) => root -> n (Transaction root ()))
              -> m ()

    transactReadOnly :: Root root
                     => (forall n. (AllocReaderM n, MonadMask n) => root -> n a)
                     -> m a

-- | A monad transformer that is an instance of 'MonadHaskey'.
--
-- The @root@ is the data type holding the roots of the database trees.
newtype HaskeyT root m a = HaskeyT { fromHaskeyT :: ReaderT (ConcurrentDb root, FileStoreConfig) m a }
                         deriving (Functor, Applicative, Monad, MonadIO,
                                   MonadThrow, MonadCatch, MonadMask)

instance (Root root, MonadMask m, MonadIO m) => MonadHaskey root (HaskeyT root m) where
    transact tx = askDb >>= runFileStoreT' . D.transact tx
    transact_ tx = askDb >>= runFileStoreT' . D.transact_ tx
    transactReadOnly tx = askDb >>= runFileStoreT' . D.transactReadOnly tx

instance MonadTrans (HaskeyT root) where
    lift = HaskeyT . lift

-- | Run Haskey transactions, backed by a file store.
runFileStoreHaskeyT :: (Root root, MonadMask m, MonadIO m)
                    => HaskeyT root m a
                    -> ConcurrentDb root
                    -> FileStoreConfig
                    -> m a
runFileStoreHaskeyT m db config = runReaderT (fromHaskeyT m) (db, config)

runFileStoreT' :: (MonadIO m, MonadMask m)
               => FileStoreT FilePath (HaskeyT root m) a
               -> HaskeyT root m a
runFileStoreT' m = askCfg >>= runFileStoreT m

askDb :: Monad m => HaskeyT root m (ConcurrentDb root)
askDb = HaskeyT $ asks fst

askCfg :: Monad m => HaskeyT root m FileStoreConfig
askCfg = HaskeyT $ asks snd
