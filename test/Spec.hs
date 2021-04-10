module Main where

import Test.Hspec
import Data.Function ((&))

main :: IO ()
main = hspec $
  describe "All tests" $ do
    it "Obviously" $
      True `shouldBe` True



