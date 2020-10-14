module Lib where

import Data.Kind
import Control.Applicative
import Control.Exception (SomeException(..), Exception(..), throwIO)
import qualified Control.Exception as Exception
import Control.Monad.Reader
import Data.Void
import Data.Typeable
import GHC.TypeLits

newtype PRIO e r a = PRIO (ReaderT r IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader r)

type Checked (e :: [Type]) r a = forall err. Throws err e => PRIO err r a

runPRIO :: Exception e => r -> PRIO e r a -> IO (Either e a)
runPRIO r (PRIO k) = Exception.try (runReaderT k r)

runPRIOSafe :: r -> PRIO NoExceptions r a -> IO a
runPRIOSafe r (PRIO k) = runReaderT k r

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

throw :: forall e err r a. (Subtype err e, Exception e, Exception err) => e -> PRIO err r a
throw e = PRIO . liftIO . throwIO $ (project e :: err)

try :: forall e err r a. (Exception e, Exception err) => PRIO (e || err) r a -> PRIO err r (Either e a)
try (PRIO action) = do
  eresult <- PRIO $ ReaderT $ \r -> Exception.try (runReaderT action r)
  case eresult :: Either (e || err) a of
    Left (This e) ->
      pure (Left e)
    Left (That err) ->
      PRIO . liftIO . throwIO $ (err :: err)
    Right a ->
      pure (Right a)

catch :: forall e err r a. (Exception e, Exception err) => PRIO (e || err) r a -> (e -> PRIO err r a) -> PRIO err r a
catch action withException = do
  ea <- try action
  case ea of
    Left e -> withException e
    Right a -> pure a

infixl 6 `catch`

runIO :: IO a -> PRIO SomeException r a
runIO = PRIO . liftIO

runIOUnsafe :: IO a -> PRIO e r a
runIOUnsafe = PRIO . liftIO

