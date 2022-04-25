module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event (keepLatest, makeEvent, subscribe)
import FRP.Event.Variant (V, vbus)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))

type Test =
  V
    ( a :: Int
    , b :: Unit
    , c :: V (a :: Int, b :: String, q :: V (r :: Boolean))
    , d :: Array Int
    )

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "VBus" do
          it "works with simple pushing" $ liftEffect do
            r <- Ref.new []
            u <- subscribe
              ( keepLatest $ vbus (Proxy :: _ Test)
                  ( \p e -> e.d <|> makeEvent \k -> do
                      k [ 1, 2 ]
                      p.d [ 34 ]
                      pure (pure unit)
                  )
              )
              \i -> Ref.modify_ (append i) r
            u
            Ref.read r >>= shouldEqual [ 34, 1, 2 ]
          it "works with more complex pushing 1" $ liftEffect do
            r <- Ref.new ""
            u <- subscribe
              ( keepLatest $ vbus (Proxy :: _ Test)
                  ( \p e -> map show e.d <|> map show e.c.a <|> map show e.c.q.r <|> makeEvent \_ -> do
                      p.d [1]
                      p.c.a 55
                      p.c.q.r false
                      p.b unit
                      pure (pure unit)
                  )
              )
              \i -> Ref.modify_ (append i) r
            u
            Ref.read r >>= shouldEqual "false55[1]"
          it "works with more complex pushing 2" $ liftEffect do
            r <- Ref.new ""
            u <- subscribe
              ( keepLatest $ vbus (Proxy :: _ Test)
                  ( \p e -> map show e.d <|> map show e.c.a <|> map show e.b <|> makeEvent \_ -> do
                      p.d [1]
                      p.c.a 55
                      p.c.q.r false
                      p.b unit
                      pure (pure unit)
                  )
              )
              \i -> Ref.modify_ (append i) r
            u
            Ref.read r >>= shouldEqual "unit55[1]"
