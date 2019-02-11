# Assert plugin

## What

Drop in replacement for GHC's `assert` feature.

How to use:

- Use `assert` in your code.
- Turn on assertion checking by calling GHC with
  `-fplugin=With.Assertions` (otherwise `assert` doesn't have any
  effect the generated code)

## Why

GHC's native `assert` only supports boolean assertions. So assert failures are not very informative. I wanted richer assertions, which could build up an assertion failure message (such as `Validation` in the `validity` package, which are supported out of the box by the assert plugin).
