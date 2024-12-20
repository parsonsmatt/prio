{-# language FunctionalDependencies #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

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
