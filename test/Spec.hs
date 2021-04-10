module Main where

import Test.Hspec
-- import Data.Function ((&))
import           Prelude             hiding (String)

main :: IO ()
main = hspec $
  describe "All tests" $ do
    it "Obviously" $
      True `shouldBe` True



