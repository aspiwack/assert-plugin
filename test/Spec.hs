{-# OPTIONS -fplugin=With.Assertions #-}

import qualified Data.Validity as Validity
import Control.Exception hiding (assert)
import Test.Assert
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Replacing assert with assertError" $ do
    it "accepts passed boolean assertions" $ do
      assert True 17 `shouldBe` 17
    it "throws an error on a failed boolean assertion" $ do
      evaluate (assert False ()) `shouldThrow` anyException
    it "accepts passed validity assertions" $ do
      assert Validity.valid 17 `shouldBe` 17
    it "throws an error on a failed validity assertion" $ do
      evaluate (assert (Validity.invalid "") ()) `shouldThrow` anyException
