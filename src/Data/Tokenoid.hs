{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Tokenoid
  ( InputTokens (..),
    OutputTokens (..),
    Tokenoid (..),
    consume,
    consumed,
    from,
    fromInputTokens,
    fromOutputTokens,
    inputTokens,
    outputTokens,
    produce,
    produced,
    roughEstimateInput,
    roughEstimateOutput,
    runTokenoidT,
    runTokenoidIO,
    spend,
  )
where

import qualified Data.Text as T
import Data.Tokenoid.Prelude hiding (from)

newtype InputTokens n = InputTokens (Sum n)
  deriving newtype
    ( Applicative,
      Bounded,
      Eq,
      Foldable,
      Functor,
      Semigroup,
      Monad,
      Monoid,
      Num,
      Ord,
      Read,
      Show
    )

newtype OutputTokens n = OutputTokens (Sum n)
  deriving newtype
    ( Applicative,
      Bounded,
      Eq,
      Foldable,
      Functor,
      Semigroup,
      Monad,
      Monoid,
      Num,
      Ord,
      Read,
      Show
    )

newtype Tokenoid n
  = Tokenoid
      ( InputTokens n,
        OutputTokens n
      )
  deriving newtype (Eq, Semigroup, Monoid, Ord, Read, Show)

inputTokens :: forall n. Lens' (Tokenoid n) (InputTokens n)
inputTokens = lens (\(Tokenoid (i, _)) -> i) (\(Tokenoid (_, o)) i -> Tokenoid (i, o))

outputTokens :: forall n. Lens' (Tokenoid n) (OutputTokens n)
outputTokens = lens (\(Tokenoid (_, o)) -> o) (\(Tokenoid (i, _)) o -> Tokenoid (i, o))

newtype TokenoidT m n a = TokenoidT {runTokenoidT :: StateT (Tokenoid n) m a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO
    )

produce :: forall m n. (Monad m, Num n) => InputTokens n -> TokenoidT m n ()
produce n = TokenoidT $ inputTokens += n

consume :: forall m n. (Monad m, Num n) => OutputTokens n -> TokenoidT m n ()
consume n = TokenoidT $ outputTokens += n

produced :: forall m n. (Monad m) => TokenoidT m n (InputTokens n)
produced = TokenoidT $ use inputTokens

consumed :: forall m n. (Monad m) => TokenoidT m n (OutputTokens n)
consumed = TokenoidT $ use outputTokens

spend :: forall m n. (Monad m, Num n) => (InputTokens n, OutputTokens n) -> TokenoidT m n ()
spend (i, o) = TokenoidT $ do
  inputTokens += i
  outputTokens += o

runTokenoidIO :: forall n a. (Num n) => TokenoidT IO n a -> IO (a, Tokenoid n)
runTokenoidIO = flip runStateT mempty . runTokenoidT

fromInputTokens :: forall n. (Num n) => InputTokens n -> Tokenoid n
fromInputTokens i = mempty & inputTokens .~ i

fromOutputTokens :: forall n. (Num n) => OutputTokens n -> Tokenoid n
fromOutputTokens o = mempty & outputTokens .~ o

from :: forall n. (Num n) => (InputTokens n, OutputTokens n) -> Tokenoid n
from (i, o) = mempty & inputTokens .~ i & outputTokens .~ o

roughEstimateInput :: Text -> InputTokens Int
roughEstimateInput = InputTokens <$> roughEstimate

roughEstimate :: Text -> Sum Int
roughEstimate = Sum . adjust . length . T.words
  where
    adjust :: Int -> Int
    adjust w = ceiling $ fromIntegral w * (1.33 :: Double)

roughEstimateOutput :: Text -> OutputTokens Int
roughEstimateOutput = OutputTokens <$> roughEstimate
