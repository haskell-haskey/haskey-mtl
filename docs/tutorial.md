Beginner Tutorial on Using Haskey
=================================

Haskey is an ACID compliant embedded key-value store entirely written in Haskell. It supports sequential writers and non-blocking concurrent readers readers from within the same process, much like [LMDB][lmdb-homepage].

  [lmdb-homepage]: https://symas.com/lightning-memory-mapped-database/

This tutorial gives an introduction on how to use Haskey in your Haskell application.

_If you just want to jump to the full code example [click here](#full-code-example)!_

## The Haskey libraries

The [Haskey project][haskey-org] is composed of 3 libraries:

  - [haskey-btree]: Implements the B<sup>+</sup>-tree data structure, which holds all data, and operations. The data structure and operations abstract over a storage back-end.

  - [haskey]: Implements various storage back-ends for the haskey-btree libraries. The most important back-end is of course the file based back-end that supports sequential writers and concurrent readers.

  - [haskey-mtl]: Exposes the [`HaskeyT`][HaskeyT] monad transformer, which can be used to embed database access in your monad stack.

  [haskey-org]: https://github.com/haskell-haskey
  [haskey-btree]: https://github.com/haskell-haskey/haskey-btree
  [haskey]: https://github.com/haskell-haskey/haskey
  [haskey-mtl]: https://github.com/haskell-haskey/haskey-mtl

## B<sup>+</sup>-tree data structure and operations

The [haskey-btree] package includes two separate implementations of B<sup>+</sup>-trees. Only the one in [Data.BTree.Impure] is interesting for this tutorial<sup>1</sup>.

Take a look at this module. We notice functions with the following type signatures:

```haskell
lookup :: (AllocReaderM m, Key key, Value val)
       => key
       -> Tree key val
       -> m (Maybe val)

insert :: (AllocM m, Key key, Value val)
       => key
       -> val
       -> Tree key val
       -> m (Tree key val)

delete :: (AllocM m, Key key, Value val)
       => key
       -> Tree key val
       -> m (Tree key val)
```

We can see that the functions operate in either the [`AllocReaderM`][AllocReaderM] or the [`AllocM`][AllocM] monad. Let's take a closer look at the definitions of these type classes:

```haskell
class Monad m => AllocReaderM m where
  readNode :: (Key key, Value val)
           => Height height
           -> NodeId height key val
           -> m (Node height key val)

  readOverflow :: Value val => OverflowId -> m val

-- Not all functions included...
class AllocReaderM m => AllocM m where
  allocNode :: (Key key, Value val)
            => Height height
            -> Node height key val
            -> m (NodeId height key val)
  freeNode :: Height height -> NodeId height key val -> m ()

  allocOverflow :: Value val => val -> m OverflowId
  freeOverflow :: OverflowId -> m ()
```

We can see that the [`AllocReaderM`][AllocReaderM] type class supports operations to read B<sup>+</sup>-tree nodes and overflow values with a certain ID, while the [`AllocM`][AllocM] type class supports operations to write and free B<sup>+</sup>-tree nodes and overflow values. How do we get such an allocator? Well, the [haskey] library contains the most useful one. It implements a node allocator that supports sequential writers and non-blocking concurrent readers.

<sup>1</sup> _The first one can be found in the [Data.BTree.Pure] module and contains a pure, standard textbook implementation of an in-memory B<sup>+</sup>-tree. It is a standalone implementation, not used anywhere else in the code base, but still a nice thing to have._

## Robust, thread-safe allocator

The [haskey] library builds upon the [haskey-btree] library by providing a robust, thread-safe page allocator with transaction support. It allows for multiple concurrent readers which do not block each other, and serialized write access. Furthermore, readers do not block writers, and vice versa.

The actual allocator is implemented in an internal module called [Database.Haskey.Alloc.Concurrent.Internal.Monad], but you don’t really need to know about it. The exported functions in [Database.Haskey.Alloc.Concurrent] use this monad internally, and they are sufficient to use the allocator. The following functions are particularly interesting:

```haskell
-- Simplified type signatures...
createConcurrentDb :: (Root root, ConcurrentMetaStoreM m)
                   => ConcurrentHandles
                   -> root
                   -> m (ConcurrentDb root)

openConcurrentDb :: (Root root, ConcurrentMetaStoreM m)
                 => ConcurrentHandles
                 -> m (Maybe (ConcurrentDb root))

transact :: (ConcurrentMetaStoreM m, Root root)
         => (forall n. AllocM n => root -> n (Transaction root a))
         -> ConcurrentDb root
         -> m a

transactReadOnly :: (ConcurrentMetaStoreM m, Root root)
                 => (forall n. AllocReaderM n => root -> n a)
                 -> ConcurrentDb root
                 -> m a
```

The [`openConcurrentDb`][openConcurrentDb] and [`createConcurrentDb`][createConcurrentDb] functions are used to create new and open existing databases. While the [`transact`][transact] and the [`transactReadOnly`][transactReadOnly] functions are used to start read-write and read-only transactions.

We can see that the [`transact`][transact] and [`transactReadOnly`][transactReadOnly] functions take a function that takes some database [`Root`][Root] and can do [`AllocM`][AllocM] and [`AllocReaderM`][AllocReaderM] actions. Exactly what we need to run tree modifications and queries from [Data.BTree.Impure]! Excellent!

But there is one more catch! We need to run these functions in a [`ConcurrentMetaStoreM`][ConcurrentMetaStoreM]! Where can we find such a monad?


## The file-based storage back-end

Let’s take a closer look to the [`ConcurrentMetaStoreM`][ConcurrentMetaStoreM] type class. The functions in the type class aren’t all that interesting, but the superclass is! It’s the [`StoreM`][StoreM] class:

```haskell
class StoreM FilePath m => ConcurrentMetaStoreM m where
  -- Omitted type class body...

-- Simplified type signatures, not all functions included...
class Monad m => StoreM hnd m | m -> hnd where
  putNodePage :: (Key key, Value val)
              => hnd
              -> Height height
              -> NodeId height key val
              -> Node height key val
              -> m ()

  getNodePage :: (Key key, Value val)
              => hnd
              -> Height height
              -> NodeId height key val
              -> m (Node height key val)

  putOverflow :: Value val => hnd -> val m ()
  getOverflow :: Value val => hnd -> m val
```

We can see that the [`StoreM`][StoreM] type class simply abstracts over a storage back-end that can read and write pages and overflow values. There are two built-in storage back-ends. One is the [InMemory][Database.Haskey.Store.InMemory] storage back-end, which we’ll let you discover by yourself. The other one is the on-disk store in [Database.Haskey.Store.File]. Let’s look at the interesting functions.

```haskell
runFileStoreT :: Monad m	 
              => FileStoreT FilePath m a
              -> FileStoreConfig
              -> m a

defFileStoreConfig :: FileStoreConfig
```

The [`runFileStoreT`][runFileStoreT] function takes a [`FileStoreT`][FileStoreT] monad and a configuration parameter. The [`FileStoreT`][FileStoreT] monad conveniently is an instance of the [`ConcurrentMetaStoreM`][ConcurrentMetaStoreM] and [`defFileStoreConfig`][defFileStoreConfig] conveniently returns a database configuration.

As such we can run our beloved transactions! A full example putting it all the above together is on [GithHub](https://github.com/haskell-haskey/haskey/blob/master/example/Main.hs), but we advise you to keep around a little longer for the [`HaskeyT`][HaskeyT] explanation, as it will make your life a lot easier!


## User-defined schemas and multi-table support

Haskey stores all data in a tree-like structure called a B<sup>+</sup>-tree, where the nodes are identified by page numbers pointing to pages in the database. Two specially designated fixed pages contain the metadata of the database. The [`ConcurrentMeta`][ConcurrentMeta] data type holds this metadata. The [`ConcurrentMeta`][ConcurrentMeta] data type is parameterized over a user-defined [`Root`][Root] type, as can be seen below.

```haskell
-- Simplified data type definition...
data ConcurrentMeta root = ConcurrentMeta {
    concurrentMetaRevision :: TxId
    -- Some record fields omitted...
  , concurrentMetaRoot :: root
    -- Some record fields omitted...
  }
```

This user-defined data type will be stored in the metadata and is passed along to and returned by transactions, as can be seen from the type signature of the [`transact`][transact] and [`transactReadOnly`][transactReadOnly] functions. As long as our root type is an instance of the [`Root`][Root] type class, this essentially allows the user to store and manipulate multiple database trees.

```haskell
transact :: (ConcurrentMetaStoreM m, Root root)
         => (forall n. (AllocM n, MonadMask n) => root -> n (Transaction root a))
         -> ConcurrentDb root
         -> m a
transactReadOnly :: (ConcurrentMetaStoreM m, Root root)
                 => (forall n. (AllocReaderM n, MonadMask n) => root -> n a)
                 -> ConcurrentDb root
                 -> m a
```

The usage of lenses simplifies querying and manipulating a database with a user-defined schema. As can be seen in the full code example in the section below.

## The HaskeyT monad transformer

The [haskey-mtl] library contains the [`HaskeyT`][HaskeyT] monad transformer, which is an instance of the [`MonadHaskey`][MonadHaskey] type class, which has the following definition:

```haskell
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
```

The [`HaskeyT`][HaskeyT] monad transformer essentially allows you to incorporate access to a Haskey database in your application’s monad transformer stack, by using the [`runFileStoreT`][runFileStoreT] function. Let’s move on to a full code example.

## Full code example
In this section we will dissect the example application included with the [haskey-mtl] library, of which the code can be [found on GitHub](https://github.com/haskell-haskey/haskey-mtl/tree/master/example). We will skip the imports and immediately jump to the definition of our application’s monad transformer stack. Our `App` monad is built using a `ReaderT` and a `HaskeyT` and can simply be run using the `runApp` function.

```haskell
newtype App a = AppT (ReaderT String (HaskeyT Schema IO) a)
              deriving (Functor, Applicative, Monad, MonadIO,
                        MonadHaskey Schema, MonadReader String)

runApp :: App a
       -> String
       -> ConcurrentDb Schema
       -> FileStoreConfig
       -> IO a
runApp (AppT m) r = runHaskeyT (runReaderT m r)
```

Let’s now define the `Schema` of our database. We will store a collection of tweets, identified by their unique identifier, and a collection of users who have sent out those tweets, in two separate trees. The `Schema` data type instantiates the `Root` type class, which allows us to use it as the type parameter to a `ConcurrentDb`. We also define two lenses to access the fields in this data type, called `schemaTweets` and `schemaUsers`.

```haskell
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
emptySchema = Schema B.empty B.empty

schemaTweets :: Lens' Schema (Tree Int64 Tweet)
schemaTweets = lens _schemaTweets $ \s x -> s { _schemaTweets = x }

schemaUsers :: Lens' Schema (Tree Text User)
schemaUsers = lens _schemaUsers $ \s x -> s { _schemaUsers = x }
```

Lenses allow us to very easily query and manipulate trees in our custom schema. We define some functions using lenses to query and manipulate the database. We use functions like `insert` and `lookup` from the [haskey-btree] package to query and modify the underlying B<sup>+</sup>-trees. These actions run inside an `AllocM` or `AllocReaderM` monad which provide read-write and read-only manipulations of the B<sup>+</sup>-trees. We can use the `transact` and `transactReadOnly` functions from the `MonadHaskey` type class to execute these functions.

```haskell
-- | Insert or update a tweet.
insertTweet :: AllocM n => Int64 -> Tweet -> Schema -> n Schema
insertTweet k v = schemaTweets %%~ B.insert k v

-- | Query all tweets.
queryAllTweets :: AllocReaderM n => Schema -> n [(Int64, Tweet)]
queryAllTweets root = B.toList (root ^. schemaTweets)

-- | Query a tweet.
queryTweet :: AllocReaderM n => Int64 -> Schema -> n (Maybe Tweet)
queryTweet k root = B.lookup k (root ^. schemaTweets)

-- | Insert a new user.
insertUser :: AllocM n => Text -> User -> Schema -> n Schema
insertUser k v = schemaUsers %%~ B.insert k v

-- | Quer a user.
queryUser :: AllocReaderM n => Text -> Schema -> n (Maybe User)
queryUser userId root = B.lookup userId (root ^. schemaUsers)
```

We now have all ingredients to write our application using our custom `App` monad. Our application simply stores some tweets and users in the database, and then prints them all out to the console.

```haskell
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
```

We now have a fully working application that uses a Haskey database to store and query information. When we run the application, we get the following output:

```
Using /tmp/mtl-example.haskey
Foo: Hey, I'm Foo!
Bar: Hey, I'm Bar!
Foo: I like you, Bar!
```

  [Data.BTree.Pure]: https://hackage.haskell.org/package/haskey-btree/docs/Data-BTree-Pure.html
  [Data.BTree.Impure]: https://hackage.haskell.org/package/haskey-btree/docs/Data-BTree-Impure.html

  [Database.Haskey.Alloc.Concurrent]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent.html
  [Database.Haskey.Alloc.Concurrent.Internal.Monad]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent-Internal-Monad.html
  [Database.Haskey.Store.InMemory]: https://hackage.haskell.org/package/haskey/docs/Database-Haskey-Store-InMemory.html
  [Database.Haskey.Store.File]: https://hackage.haskell.org/package/haskey/docs/Database-Haskey-Store-File.html

  [ConcurrentMeta]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent.html#t:ConcurrentMeta
  [ConcurrentMetaStoreM]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent.html#t:ConcurrentMetaStoreM
  [AllocM]: https://hackage.haskell.org/package/haskey-btree/docs/Data-BTree-Alloc-Class.html#t:AllocM
  [AllocReaderM]: https://hackage.haskell.org/package/haskey-btree/docs/Data-BTree-Alloc-Class.html#t:AllocReaderM
  [FileStoreT]: https://hackage.haskell.org/package/haskey/docs/Database-Haskey-Store-File.html#t:FileStoreT
  [HaskeyT]: https://hackage.haskell.org/package/haskey-mtl/docs/Control-Monad-Haskey.html#t:HaskeyT
  [MonadHaskey]: https://hackage.haskell.org/package/haskey-mtl/docs/Control-Monad-Haskey.html#t:MonadHaskey
  [Root]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent.html#t:Root
  [StoreM]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Store-Class.html#t:StoreM

  [createConcurrentDb]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent.html#v:createConcurrentDb
  [defFileStoreConfig]: https://hackage.haskell.org/package/haskey/docs/Database-Haskey-Store-File.html#v:defFileStoreConfig
  [openConcurrentDb]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent.html#v:openConcurrentDb
  [runFileStoreT]: https://hackage.haskell.org/package/haskey/docs/Database-Haskey-Store-File.html#v:runFileStoreT
  [transact]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent.html#v:transact
  [transactReadOnly]: http://hackage.haskell.org/package/haskey/docs/Database-Haskey-Alloc-Concurrent.html#v:transactReadOnly
