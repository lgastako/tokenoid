{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Tokenoid
  ( InputTokens (..),
    OutputTokens (..),
    Rates (..),
    Tokenoid (..),
    consume,
    consumed,
    cost,
    from,
    fromInputTokens,
    fromOutputTokens,
    inputTokens,
    outputTokens,
    produce,
    produced,
    roughEstimateInput,
    roughEstimateOutput,
    runTokenoid,
    runTokenoidT,
    runTokenoidIO,
    spend,
    spent,
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
      Monad,
      Monoid,
      Num,
      Ord,
      Read,
      Semigroup,
      Show
    )

instance (Enum n) => Enum (InputTokens n) where
  fromEnum (InputTokens (Sum n)) = fromEnum n
  toEnum = InputTokens . Sum . toEnum

instance (Real n) => Real (InputTokens n) where
  toRational (InputTokens (Sum n)) = toRational n

instance (Integral n) => Integral (InputTokens n) where
  toInteger (InputTokens (Sum n)) = toInteger n
  quotRem (InputTokens (Sum n)) (InputTokens (Sum m)) = (InputTokens (Sum q), InputTokens (Sum r))
    where
      (q, r) = quotRem n m

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

instance (Enum n) => Enum (OutputTokens n) where
  fromEnum (OutputTokens (Sum n)) = fromEnum n
  toEnum = OutputTokens . Sum . toEnum

instance (Real n) => Real (OutputTokens n) where
  toRational (OutputTokens (Sum n)) = toRational n

instance (Integral n) => Integral (OutputTokens n) where
  toInteger (OutputTokens (Sum n)) = toInteger n
  quotRem (OutputTokens (Sum n)) (OutputTokens (Sum m)) = (OutputTokens (Sum q), OutputTokens (Sum r))
    where
      (q, r) = quotRem n m

newtype Tokenoid n
  = Tokenoid
      ( InputTokens n,
        OutputTokens n
      )
  deriving newtype
    ( Eq,
      Semigroup,
      Monoid,
      Ord,
      Read,
      Show
    )

newtype TokenoidT m n a = TokenoidT {runTokenoidT :: StateT (Tokenoid n) m a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadState (Tokenoid n),
      MonadIO
    )

type TokenoidM = TokenoidT Identity

data Rates r = Rates
  { inputCost :: r,
    outputCost :: r
  }

inputTokens :: forall n. Lens' (Tokenoid n) (InputTokens n)
inputTokens = lens (\(Tokenoid (i, _)) -> i) (\(Tokenoid (_, o)) i -> Tokenoid (i, o))

outputTokens :: forall n. Lens' (Tokenoid n) (OutputTokens n)
outputTokens = lens (\(Tokenoid (_, o)) -> o) (\(Tokenoid (i, _)) o -> Tokenoid (i, o))

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

spent :: forall m n. (Monad m) => TokenoidT m n (InputTokens n, OutputTokens n)
spent = TokenoidT $ (,) <$> use inputTokens <*> use outputTokens

runTokenoidIO :: forall n a. (Num n) => TokenoidT IO n a -> IO (a, Tokenoid n)
runTokenoidIO = flip runStateT mempty . runTokenoidT

runTokenoid :: forall n a. (Num n) => TokenoidM n a -> (a, Tokenoid n)
runTokenoid = flip runState mempty . runTokenoidT

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

cost ::
  forall n r.
  ( Num r,
    Integral (InputTokens n),
    Integral (OutputTokens n)
  ) =>
  Rates r ->
  Tokenoid n ->
  r
cost Rates {..} t =
  inputCost
    * fromIntegral (t ^. inputTokens)
    + outputCost
    * fromIntegral (t ^. outputTokens)
