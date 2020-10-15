module Lib
    ( module Lib
    , MonadIO(..)
    ) where

import Control.Monad.State
import Data.Kind
import Control.Applicative
import Control.Exception.Safe (SomeException(..), MonadThrow(..), throwM, Exception(..), throwIO, MonadCatch(..))
import qualified Control.Exception.Safe as Exception
import Control.Monad.Reader
import Data.Void
import Data.Typeable
import GHC.TypeLits
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.Coerce

newtype CheckedT (e :: Type) (m :: Type -> Type) (a :: Type)
  = CheckedT { unsafeRunCheckedT :: m a }
  deriving
    newtype (Functor, Applicative, Monad, MonadReader r, MonadError e, MonadState s)

instance (MonadIO m, SomeException :< err) => MonadIO (CheckedT err m) where
  liftIO = CheckedT . liftIO

deriving newtype instance (forall a b. Coercible a b => Coercible (m a) (m b), SomeException :< e, MonadUnliftIO m) => MonadUnliftIO (CheckedT e m)

type Checked e = CheckedT e IO

type Throwing es m a = forall err. (Exception err, Throws err es) => CheckedT err m a

runChecked :: Exception e => Checked e a -> IO (Either e a)
runChecked = runCheckedT

runCheckedT :: (Exception e, MonadCatch m) => CheckedT e m a -> m (Either e a)
runCheckedT action = Exception.try (unsafeRunCheckedT action)

runCheckedSafe :: Checked NoExceptions a -> IO a
runCheckedSafe = runCheckedTSafe

runCheckedTSafe :: CheckedT NoExceptions m a -> m a
runCheckedTSafe = unsafeRunCheckedT

toExceptT :: (Exception e, MonadCatch m) => CheckedT e m a -> ExceptT e m a
toExceptT (CheckedT ma) = ExceptT $ Exception.try ma

data Or a b = This a | That b
  deriving stock (Eq, Show)

type (||) = Or
infixr 6 ||

-- | it's like Void but, not really
data NoExceptions

instance Show NoExceptions where show = undefined

instance Exception NoExceptions where
  toException = undefined
  fromException _ = Nothing

type Only a = Or a NoExceptions

instance (Exception a, Exception b) => Exception (Or a b) where
  toException = \case
    This a -> toException a
    That a -> toException a

  fromException e =
    This <$> fromException e <|> That <$> fromException e

  displayException = \case
    This a -> displayException a
    That a -> displayException a

type family Throws (e :: Type) (es :: k) :: Constraint where
  Throws e '[] = e ~ NoExceptions
  Throws e () = e ~ NoExceptions
  Throws e '[x] = (x :< e)
  Throws e (x ': xs) = (x :< e, Throws e xs)
  Throws e (a, b) = (a :< e, b :< e)
  Throws e (a, b, c) = (a :< e, b :< e, c :< e)
  Throws e (a, b, c, d) = Throws e '[a, b, c, d]
  Throws e (a, b, c, d, f) = Throws e '[a, b, c, d, f]
  Throws e (a, b, c, d, g, h) = Throws e '[a, b, c, d, g, h]
  Throws e (a, b, c, d, g, h, k) = Throws e '[a, b, c, d, g, h, k]
  Throws e (a, b, c, d, g, h, k, j) = Throws e '[a, b, c, d, g, h, k, j]
  Throws e (a, b, c, d, g, h, k, j, i) = Throws e '[a, b, c, d, g, h, k, j, i]
  Throws e (a, b, c, d, g, h, k, j, i, l) = Throws e '[a, b, c, d, g, h, k, j, i, l]
  Throws e (a :: Type) = a :< e

class (Subtype big lil, Exception big) => lil :< big
instance (Subtype big lil, Exception big) => lil :< big

class Subtype large single where
  project :: single -> large

instance {-# Overlappable #-} Subtype a a where
  project = id

instance {-# Overlapping #-} Subtype (a || b) a where
  project = This

instance {-# Overlappable #-} (Subtype b c) => Subtype (a || b) c where
  project = That . project

instance {-# overlappable #-} (TypeError (SubtypeErrorMsg a b)) => Subtype a b where
  project = undefined

type SubtypeErrorMsg a b =
  'Text "The type " ':<>: 'ShowType b ':<>: 'Text " is not a subtype of " ':<>: 'ShowType a

throw :: forall e err m a. (Subtype err e, Exception e, Exception err, MonadThrow m) => e -> CheckedT err m a
throw e = throwUnchecked (project e :: err)

throwUnchecked :: (MonadThrow m, Exception e) => e -> CheckedT err m a
throwUnchecked = CheckedT . throwM

tryAll :: (Exception err, MonadCatch m) => CheckedT err m a -> CheckedT NoExceptions m (Either err a)
tryAll (CheckedT action) = CheckedT (Exception.try action)

try :: forall e err m a. (Exception e, Exception err, MonadCatch m) => CheckedT (e || err) m a -> CheckedT err m (Either e a)
try (CheckedT action) = do
  eresult <- CheckedT $ Exception.try action
  case eresult :: Either (e || err) a of
    Left (This e) ->
      pure (Left e)
    Left (That err) ->
      throwUnchecked err
    Right a ->
      pure (Right a)

catch :: forall e err m a. (Exception e, Exception err, MonadCatch m) => CheckedT (e || err) m a -> (e -> CheckedT err m a) -> CheckedT err m a
catch action withException = do
  ea <- try action
  case ea of
    Left e -> withException e
    Right a -> pure a

infixl 6 `catch`

liftIOWith :: forall es err m a. (MonadIO m, SomeException :< err, Throws err es) => IO a -> CheckedT err m a
liftIOWith = liftIO

embedMonadThrow :: (SomeException :< err, MonadThrow m) => m a -> CheckedT err m a
embedMonadThrow = CheckedT

generalize :: e :< err => CheckedT e m a -> CheckedT err m a
generalize (CheckedT a) = CheckedT a

-- |
specialize
  :: forall e err m a.
  ( SomeException :< err
  , Exception e
  , e :< err
  , MonadCatch m
  )
  => CheckedT (SomeException || err) m a
  -> CheckedT err m a
specialize someException = do
  ea <- try someException
  case ea of
    Left (exn :: SomeException) ->
      case fromException exn of
        Nothing ->
          throw exn
        Just (e :: e) ->
          throw e
    Right a ->
      pure a

-- | Assert that the 'CheckedT' action doesn't throw 'SomeException'
-- anymore. This function is unsafe because there's absolutely no guarantee
-- that it's true!
dismissUnsafe
  :: CheckedT (SomeException || err) m a
  -> CheckedT err m a
dismissUnsafe (CheckedT ma) = CheckedT ma
