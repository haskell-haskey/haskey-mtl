{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A monad transformer supporting Haskey transactions.
--
-- See <https://github.com/haskell-haskey/haskey-mtl/blob/master/example/Main.hs>
-- for a complete example.
module Control.Monad.Haskey (
  -- * Re-exports
  module Database.Haskey.Alloc.Transaction

  -- * Monad
, MonadHaskey(..)
, HaskeyT
, runHaskeyT

  -- * Open and create (re-exports)
, FileStoreT
, FileStoreConfig
, runFileStoreT
, defFileStoreConfig
, ConcurrentDb
, concurrentHandles
, openConcurrentDb
, createConcurrentDb
) where

import Control.Applicative (Applicative)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Trans.Class (MonadTrans(..))

import Control.Monad.Base (MonadBase(..))
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl(..),
                                    ComposeSt(..), defaultLiftBaseWith,
                                    defaultRestoreM, defaultLiftWith,
                                    defaultRestoreT)
import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Monad.State.Lazy as StateL
import qualified Control.Monad.State.Strict as StateS
import qualified Control.Monad.Writer.Lazy as WriterL
import qualified Control.Monad.Writer.Strict as WriterS

import Data.BTree.Alloc (AllocM, AllocReaderM)
import Data.Monoid (Monoid)

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
class Monad m => MonadHaskey root m | m -> root where
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

instance (Root root, Applicative m, MonadMask m, MonadIO m) => MonadHaskey root (HaskeyT root m) where
    transact tx = askDb >>= runFileStoreT' . D.transact tx
    transact_ tx = askDb >>= runFileStoreT' . D.transact_ tx
    transactReadOnly tx = askDb >>= runFileStoreT' . D.transactReadOnly tx

instance MonadTrans (HaskeyT root) where
    lift = HaskeyT . lift

deriving instance MonadBase IO m => MonadBase IO (HaskeyT root m)

instance MonadTransControl (HaskeyT root) where
    type StT (HaskeyT root) a = StT (ReaderT (ConcurrentDb root, FileStoreConfig)) a
    liftWith = defaultLiftWith HaskeyT fromHaskeyT
    restoreT = defaultRestoreT HaskeyT

instance MonadBaseControl IO m => MonadBaseControl IO (HaskeyT root m) where
    type StM (HaskeyT root m) a = ComposeSt (HaskeyT root) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

-- | Run Haskey transactions, backed by a file store.
runHaskeyT :: (Root root, MonadMask m, MonadIO m)
                    => HaskeyT root m a
                    -> ConcurrentDb root
                    -> FileStoreConfig
                    -> m a
runHaskeyT m db config = runReaderT (fromHaskeyT m) (db, config)

runFileStoreT' :: (MonadIO m, MonadMask m)
               => FileStoreT FilePath (HaskeyT root m) a
               -> HaskeyT root m a
runFileStoreT' m = askCfg >>= runFileStoreT m

askDb :: Monad m => HaskeyT root m (ConcurrentDb root)
askDb = HaskeyT $ asks fst

askCfg :: Monad m => HaskeyT root m FileStoreConfig
askCfg = HaskeyT $ asks snd

--------------------------------------------------------------------------------
-- Some definitions of mtl monad transformers below.
--------------------------------------------------------------------------------

instance MonadReader r m => MonadReader r (HaskeyT root m) where
    ask = lift ask
    reader = lift . reader
    local f (HaskeyT (ReaderT m)) =  HaskeyT . ReaderT $ \r -> local f (m r)

instance MonadHaskey root m => MonadHaskey root (ReaderT r m) where
    transact tx = lift $ transact tx
    transact_ tx = lift $ transact_ tx
    transactReadOnly tx = lift $ transactReadOnly tx

instance MonadState s m => MonadState s (HaskeyT root m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadHaskey root m => MonadHaskey root (StateL.StateT s m) where
    transact tx = lift $ transact tx
    transact_ tx = lift $ transact_ tx
    transactReadOnly tx = lift $ transactReadOnly tx

instance MonadHaskey root m => MonadHaskey root (StateS.StateT s m) where
    transact tx = lift $ transact tx
    transact_ tx = lift $ transact_ tx
    transactReadOnly tx = lift $ transactReadOnly tx

instance MonadWriter w m => MonadWriter w (HaskeyT root m) where
    writer = lift . writer
    tell = lift . tell
    listen (HaskeyT (ReaderT m)) = HaskeyT . ReaderT $ \r -> listen (m r)
    pass (HaskeyT (ReaderT m)) = HaskeyT . ReaderT $ \r -> pass (m r)

instance (Monoid w, MonadHaskey root m) => MonadHaskey root (WriterL.WriterT w m) where
    transact tx = lift $ transact tx
    transact_ tx = lift $ transact_ tx
    transactReadOnly tx = lift $ transactReadOnly tx

instance (Monoid w, MonadHaskey root m) => MonadHaskey root (WriterS.WriterT w m) where
    transact tx = lift $ transact tx
    transact_ tx = lift $ transact_ tx
    transactReadOnly tx = lift $ transactReadOnly tx

instance MonadRWS r w s m => MonadRWS r w s (HaskeyT root m) where

instance (Monoid w, MonadHaskey root m) => MonadHaskey root (RWSL.RWST r w s m) where
    transact tx = lift $ transact tx
    transact_ tx = lift $ transact_ tx
    transactReadOnly tx = lift $ transactReadOnly tx

instance (Monoid w, MonadHaskey root m) => MonadHaskey root (RWSS.RWST r w s m) where
    transact tx = lift $ transact tx
    transact_ tx = lift $ transact_ tx
    transactReadOnly tx = lift $ transactReadOnly tx
--------------------------------------------------------------------------------
