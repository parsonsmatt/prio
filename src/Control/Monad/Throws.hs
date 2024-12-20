{-# language FunctionalDependencies #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

-- | We want a general function for throwing checked exceptions, such that
-- they can be plucked out by the plucky exception mechanism.
--
-- An ideal interface is something like this:
--
-- @
-- foo :: (MonadThrows Foo m) => m ()
-- foo = throwChecked Foo
-- @
--
-- However, we want the ability to have multiple exception types:
--
-- @
-- foo :: (MonadThrows Foo m, MonadThrows Bar m) => m ()
-- foo = if wat
--   then throwChecked Foo
--   else throwChecked Bar
-- @
--
-- So a plain functional dependency would have to look more like the
-- 'MonadError' approach:
--
-- @
-- foo :: (MonadThrows e m, ExceptionSubtype e Foo, ExceptionSubtype e Bar) => m ()
-- foo = throwChecked $ project Foo
-- @
--
-- Now, tracking throwing is all well and good, but what we really want is
-- the ability to express catching in a general way.
--
-- @
-- try
--     :: forall e err m a
--      . (Exception e, Exception err, MonadCatch m)
--     => CheckedT (e || err) m           a
--     -> CheckedT       err  m (Either e a)
-- @
--
-- This is the type we want to generalize. This function is doing some
-- "plucking" of it's own. You'd have a generalized "producing" function:
--
-- @
-- blah
--     :: (ExceptionSubtype err Foo, ExceptionSubtype err Bar, MonadThrow m)
--     => CheckedT err m a
-- blah = do
--     throw Foo
--     throw Bar
-- @
--
-- And when you pass it to @try@ with a type application, the instance
-- resolution ends up plucking things out by picking @||@ as the outermost
-- type and then finding the relevant instance to delegate the remainder to
-- the other.
--
-- @
-- try @Foo blah
--     :: (ExceptionSubtype err Bar, MonadCatch m)
--     => CheckedT err m (Either Foo a)
-- @
--
-- What happens here is that @blah@ gets partially concretized: from
--
-- @
--  blah
--      :: ( ExceptionSubtype err Foo
--         , ExceptionSubtype err Bar
--         )
--      => CheckedT err m a
--
--  -- to,
--  blah
--      :: ( ExceptionSubtype (Foo || err) Foo
--         , ExceptionSubtype (Foo || err) Bar
--         )
--      => CheckedT (Foo || err) m a
-- @
--
-- GHC is able to find the relevant instances, so it works out. The @err@
-- type shrinks.
--
-- Now, we want to generalize @try@ so that we can pluck a single exception
-- type out of a polymorphic monad. The most straightforward approach would be:
--
-- @
--  class MonadTry m where
--      try :: m (e || err) a -> m err (Either e a)
-- @
--
-- But this is bad! We now have a class of kind @Type -> Type -> Type@.
-- This is incompatible with any monad transformer, and requires that the
-- underlying type track the error monad. So we really want a type that can
-- conform to the @Type -> Type@ form of most monads.
--
-- @
--  class MonadTry m where
--      try :: m a -> m (Either e a)
-- @
--
-- Well, that may just put us dead in the water. @m@ here is the exact same
-- on left and right hand side. We'd need @m@ and @n@ to allow them to
-- vary. perhaps that's alright.
--
-- @
--  class MonadTry m n | m -> n, n -> m where
--      try :: n a -> m (Either e a)
module Control.Monad.Throws where

import Control.Monad.Catch (MonadThrow, MonadCatch, throwM, Exception)
import Lib
import GHC.Stack
import GHC.TypeLits
import qualified Control.Exception.Safe as Exception

class (Monad m, Exception err) => MonadThrows err m | m -> err where
    throwAllChecked :: (HasCallStack) => err -> m a

throwChecked :: (MonadThrows err m, ExceptionSubtype err e) => e -> m a
throwChecked = throwAllChecked . inject

instance
    ( Exception err, MonadThrow m
    ) => MonadThrows err (CheckedT err m) where
    throwAllChecked = throwUnchecked

-- instance
--     (TypeError ('Text "no"), Exception e, err ~ NoExceptions) =>
--         MonadThrows e err IO where
--     throwChecked = throwM

data FooExn = FooExn
    deriving (Show, Exception)

data BarExn = BarExn
    deriving (Show, Exception)

blah :: (Throws err (FooExn, BarExn), MonadThrows err m) => Int -> m a
    -- _ => Int -> _
blah i
    | i < 0 =
        throwChecked FooExn
    | otherwise =
        throwChecked BarExn

concretized :: (MonadThrow m, Throws e (FooExn, BarExn)) => CheckedT e m a
concretized = blah 2

concretized' :: (MonadThrow m) => CheckedT (FooExn || BarExn) m a
concretized' = blah 2

concretized'' :: (MonadCatch m) => CheckedT BarExn m (Either FooExn a)
concretized'' = Lib.try @FooExn $ blah 2

-- concretized2 :: (MonadIO m) => m a
-- concretized2 = liftIO $ blah 2

tryAllChecked ::
    forall err m a.
    -- ( Exception e, Exception err,
    --   MonadCatch m, MonadThrows err m, ExceptionSubtype x err) =>
    -- (Exception e, MonadCatch m, Exception exn, ExceptionSubtype err exn, MonadThrows err m) =>
    ( Exception err
    , MonadCatch m
    ) =>
    CheckedT err m a ->
    m (Either err a)
tryAllChecked (CheckedT action) = do
    Exception.try action

tryOneChecked
    ::  forall e rest m a.
    ( MonadCatch m
    , MonadThrows rest m
    , Exception (e || rest)
    ) =>
    CheckedT (e || rest) m a ->
    m (Either e a)
tryOneChecked action = do
    eres <- unsafeRunCheckedT $ tryAll action
    case eres of
        Right a ->
            pure (Right a)
        Left eerr ->
            case eerr of
                This e ->
                    pure (Left e)
                That err ->
                    throwAllChecked err

wat
    :: (MonadCatch m, MonadThrows rest m)
    => m (Either FooExn Int)
wat = tryOneChecked (throwChecked FooExn)

wat2
    :: forall rest exn m a. (MonadThrows exn m, MonadCatch m)
    => m (Either FooExn a)
wat2 = tryOneChecked @FooExn (throwChecked FooExn)

concretized3
    :: forall e err m a.
    ( MonadCatch m
    , MonadThrows err m
    , ExceptionSubtype err BarExn
    )
    => m (Either FooExn a)
concretized3 = tryOneChecked @FooExn $ blah 2
