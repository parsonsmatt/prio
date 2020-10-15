import Lib
import Test.Hspec
import Control.Exception (SomeException(..), Exception(..))

instance Eq SomeException where
  SomeException e0 == SomeException e1 =
    show e0 == show e1

data Foo = Foo
  deriving (Eq, Show, Exception)

data Bar = Bar
  deriving (Eq, Show, Exception)

data Quux = Quux
  deriving (Eq, Show, Exception)

main :: IO ()
main = hspec do
  describe "instance Exception Or" do
    it "Catching `Or a b` catches an `a`" do
      let
        exn =
          toException Foo
        orExn :: Maybe (Foo || Bar)
        orExn =
          fromException  exn
      orExn `shouldBe` Just (This Foo)

    it "Catching `Or a b` catches a `b`" do
      let
        exn =
          toException Bar
        orExn :: Maybe (Foo || Bar)
        orExn =
          fromException  exn
      orExn `shouldBe` Just (That Bar)

    it "Catching an `Or a (Or b c)` catches an `a`" do
      fromException (toException Foo)
        `shouldBe`
          Just (This Foo :: Foo || Bar || Quux)

    it "Catching an `Or a (Or b c)` catches a `b`" do
      fromException (toException Bar)
        `shouldBe`
          Just (That (This Bar) :: Foo || Bar || Quux)

    it "Catching an `Or a (Or b c)` catches a `c`" do
      fromException (toException Quux)
        `shouldBe`
          Just (That (That Quux) :: Foo || Bar || Quux)

    it "Prefers earlier rather than later exceptions" do
      fromException (toException Foo)
        `shouldBe`
          Just (This Foo :: Foo || Bar || Foo)

  describe "CheckedT" do
    describe "error accumulation" do
      describe "no errors thrown" do
        it "allows a safe run" do
          runCheckedSafe (pure ())
            `shouldReturn`
              ()

      describe "single error" do
        let
          prog :: (Exception e, Foo :< e) => Checked e ()
          prog = do
            throw Foo
        it "can return just a single error in the type, no Or needed" do
          runChecked prog
            `shouldReturn`
              Left Foo
        it "doesn't allow a safe run" do
          -- Uncommenting this line causes a type error.
          -- runCheckedSafe prog
          True `shouldBe` True

      describe "multiple errors" do
        let
          prog :: (Foo :< err, Bar :< err) => Checked err ()
          prog = do
            throw Foo
            throw Bar
            pure ()
        it "accumulates transparently" do
          let
            fooFirst = prog :: Checked (Foo || Bar) ()
            barFirst = prog :: Checked (Bar || Foo) ()
          runChecked fooFirst
            `shouldReturn`
              Left (This Foo)
          runChecked barFirst
            `shouldReturn`
              Left (That Foo)

      describe "type inference is p good" do
        let
          prog :: (Throws e '[Foo, Bar]) => Checked e Int
          prog = do
            throw Foo
            throw Bar
            pure 3
        it "compiles" compiles

    describe "catch" do
      let
        throwFooAndBar :: Throwing (Foo, Bar) IO ()
        throwFooAndBar = do
          throw Foo
          throw Bar
          pure ()
        catchesFoo :: Throwing Bar IO Int
        catchesFoo = do
          res <- try throwFooAndBar
          case res of
            Left Foo -> do
              pure 1
            Right () -> do
              pure 5

      it "works" do
        runChecked catchesFoo
          `shouldReturn`
            (Right 1 :: Either Bar Int)

      it "can discharge all errors" do
        runCheckedSafe
          (throwFooAndBar `catch` (\Foo -> pure ()) `catch` \Bar -> pure ())
          `shouldReturn`
            ()

    describe "generalize" do
      it "can stil catch" do
        let
          prg :: Checked Foo ()
          prg = throw Foo

          prg' :: Foo :< err => Checked err ()
          prg' = generalize prg
        runChecked prg'
          `shouldReturn`
            Left Foo

    describe "liftIO" do
      let io :: IO ()
          io = pure ()
      it "adds SomeException to the error constraint" do
        runChecked (liftIO io)
          `shouldReturn`
            Right @SomeException ()

    describe "specialize" do
      let io :: IO ()
          io = pure ()
      it "adds an additional type to the constraint" do
        let
          embedded :: Throwing SomeException IO ()
          embedded = liftIO io

          withHttp :: Throwing (SomeException, HttpException) IO ()
          withHttp = embedded

        runChecked withHttp
          `shouldReturn`
            Right @(SomeException || HttpException) ()


compiles = True `shouldBe` True

someHttpCall :: IO ()
someHttpCall = pure ()

data HttpException = HttpException
  deriving (Eq, Show, Exception)
data IOException = IOException
  deriving (Eq, Show, Exception)

someHttpCall' :: Throwing SomeException IO ()
someHttpCall' = liftIO someHttpCall

someHttpCall'' :: Throwing (HttpException, IOException, SomeException) IO ()
someHttpCall'' = liftIOWith @(HttpException, IOException) someHttpCall

someHttpCall'0 :: Throwing (HttpException, IOException, SomeException) IO ()
someHttpCall'0 = someHttpCall'

someHttpCall''' :: Throwing (HttpException, IOException) IO ()
someHttpCall''' = dismissUnsafe someHttpCall'

