{-# OPTIONS -fplugin=With.Assertions #-}

import Control.Exception hiding (assert)
import Test.Assert
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Replacing assert with assertError" $ do
    it "is performed" $ do
      evaluate (assert ()) `shouldThrow` anyException
