import Lib
import Test.Hspec
import Control.Exception (Exception(..))

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

  describe "Prio" do
    describe "error accumulation" do
      describe "no errors thrown" do
        it "allows a safe run" do
          runPRIOSafe () (pure ())
            `shouldReturn`
              ()

      describe "single error" do
        let
          prog :: (Exception e, Foo :< e) => PRIO e r ()
          prog = do
            throw Foo
        it "can return just a single error in the type, no Or needed" do
          runPRIO () prog
            `shouldReturn`
              Left Foo
        it "doesn't allow a safe run" do
          -- Uncommenting this line causes a type error.
          -- runPRIOSafe () prog
          True `shouldBe` True

      describe "multiple errors" do
        let
          prog :: (Foo :< err, Bar :< err) => PRIO err r ()
          prog = do
            throw Foo
            throw Bar
            pure ()
        it "accumulates transparently" do
          let
            fooFirst = prog :: PRIO (Foo || Bar) () ()
            barFirst = prog :: PRIO (Bar || Foo) () ()
          runPRIO () fooFirst
            `shouldReturn`
              Left (This Foo)
          runPRIO () barFirst
            `shouldReturn`
              Left (That Foo)

      describe "type inference is p good" do
        let
          prog :: Checked '[Foo, Bar] r Int
          prog = do
            throw Foo
            throw Bar
            pure 3
        it "compiles" compiles

    describe "catch" do
      let
        throwFooAndBar :: _ => PRIO err r ()
        throwFooAndBar = do
          throw Foo
          throw Bar
          pure ()
        catchesFoo :: Checked '[Bar] r Int
        catchesFoo = do
          res <- try throwFooAndBar
          case res of
            Left Foo -> do
              pure 1
            Right () -> do
              pure 5

      it "works" do
        runPRIO () catchesFoo
          `shouldReturn`
            (Right 1 :: Either Bar Int)

      it "can discharge all errors" do
        runPRIOSafe
          ()
          (throwFooAndBar `catch` (\Foo -> pure ()) `catch` \Bar -> pure ())
          `shouldReturn`
            ()


compiles = True `shouldBe` True
