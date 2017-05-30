{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Analysis.Dummy where

foo = putStrLn "foo"

foreign export ccall "hat_foo"
  foo :: IO ()
