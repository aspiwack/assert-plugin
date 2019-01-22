module Test.Assert where

assert :: a -> a
assert x = x
{-# INLINE assert #-}

assertError :: a -> a
assertError _ = error "It works"
