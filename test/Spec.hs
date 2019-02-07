{-# OPTIONS -fplugin=With.Assertions #-}

import Control.Exception hiding (assert)
import Test.Assert
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Replacing assert with assertError" $ do
    it "accepts passed assertions" $ do
      assert True 17 `shouldBe` 17
    it "throws an error on a failed assertion" $ do
      evaluate (assert False ()) `shouldThrow` anyException
