module Lib where

import Data.Kind
import Control.Applicative
import Control.Exception.Safe (SomeException(..), MonadThrow(..), throwM, Exception(..), throwIO, MonadCatch(..))
import qualified Control.Exception.Safe as Exception
import Control.Monad.Reader
import Data.Void
import Data.Typeable
import GHC.TypeLits

newtype CheckedT e m a = CheckedT { unsafeRunCheckedT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadReader r)

type Checked e = CheckedT e IO

type Throwing es m a = forall err. Throws err es => CheckedT err m a

runChecked :: Exception e => Checked e a -> IO (Either e a)
runChecked = runCheckedT

runCheckedT :: (Exception e, MonadCatch m) => CheckedT e m a -> m (Either e a)
runCheckedT action = Exception.try (unsafeRunCheckedT action)

runCheckedSafe :: Checked NoExceptions a -> IO a
runCheckedSafe = runCheckedTSafe

runCheckedTSafe :: CheckedT NoExceptions m a -> m a
runCheckedTSafe = unsafeRunCheckedT

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

type family Throws e es :: Constraint where
  Throws e '[] = e ~ NoExceptions
  Throws e '[x] = (x :< e)
  Throws e (x ': xs) = (x :< e, Throws e xs)

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
throw e = CheckedT . throwM $ (project e :: err)

try :: forall e err m a. (Exception e, Exception err, MonadCatch m) => CheckedT (e || err) m a -> CheckedT err m (Either e a)
try (CheckedT action) = do
  eresult <- CheckedT $ Exception.try action
  case eresult :: Either (e || err) a of
    Left (This e) ->
      pure (Left e)
    Left (That err) ->
      CheckedT . throwM $ (err :: err)
    Right a ->
      pure (Right a)

catch :: forall e err m a. (Exception e, Exception err, MonadCatch m) => CheckedT (e || err) m a -> (e -> CheckedT err m a) -> CheckedT err m a
catch action withException = do
  ea <- try action
  case ea of
    Left e -> withException e
    Right a -> pure a

infixl 6 `catch`

runIO :: MonadIO m => IO a -> CheckedT SomeException m a
runIO = runIOUnsafe

runIOUnsafe :: MonadIO m => IO a -> CheckedT e m a
runIOUnsafe = CheckedT . liftIO


