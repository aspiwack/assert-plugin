module Test.Assert where

import GHC.Stack

assert :: (HasCallStack, IsAssertion v) => v -> a -> a
assert _ x = x
{-# INLINE assert #-}

assertError :: (HasCallStack, IsAssertion v) => v -> a -> a
assertError v x =
  case checkAssertion v of
    Passed -> x
    Failed msg -> error ("Assertion failure: " ++ msg)

data Result
  = Passed
  | Failed String

class IsAssertion v where
  checkAssertion :: v -> Result

instance IsAssertion Bool where
  checkAssertion True = Passed
  checkAssertion False = Failed "boolean test"
