{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( Tokenoid (..),
  )
where

import Tokenoid.Prelude

data Tokenoid = Tokenoid
  { i :: Sum Int,
    o :: Sum Int
  }
  deriving (Semigroup, Monoid)

newtype TokenoidT m a = TokenoidT {runTokenoidT :: StateT Tokenoid m a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadTrans
    )

runTokenoidIO :: TokenoidT IO a -> IO (Tokenoid, a)
runTokenoidIO = flip runStateT (Tokenoid 0 0) . runTokenoidT

runTokenI :: TokenoidT Identity a -> Identity a
runTokenI = flip evalState (Tokenoid 0 0) . runTokenoidT
