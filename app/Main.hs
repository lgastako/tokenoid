{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.Tokenoid (Rates (Rates), consume, cost, produce, runTokenoidIO, spent)
import qualified Data.Tokenoid as Tokenoid
import Data.Tokenoid.Prelude
import Options.Applicative

data Options = Options
  { inputTokenFile :: Maybe FilePath,
    outputTokenFile :: Maybe FilePath
  }
  deriving (Eq, Ord, Show)

main :: IO ()
main = execParser optInfo >>= run

run :: Options -> IO ()
run Options {..} = case (inputTokenFile, outputTokenFile) of
  ( Nothing,
    Nothing
    ) -> putText "You must specify at least one of --input-tokens or --output-tokens"
  ( Just inputTokenPath,
    Nothing
    ) -> do
      putText $ "Reading input tokens from " <> cs inputTokenPath
      inputText <- readFile inputTokenPath
      let inputTokens = countInputTokens inputText
      print $ Tokenoid.fromInputTokens inputTokens
  ( Nothing,
    Just outputTokenPath
    ) -> do
      putText $ "Writing output tokens to " <> cs outputTokenPath
      outputText <- readFile outputTokenPath
      let outputTokens = countOutputTokens outputText
      print $ Tokenoid.fromOutputTokens outputTokens
  ( Just inputTokenPath,
    Just outputTokenPath
    ) -> do
      putText $ "Reading input tokens from " <> cs inputTokenPath <> " and output tokens from " <> cs outputTokenPath
      inputText <- readFile inputTokenPath
      outputText <- readFile outputTokenPath
      let inputTokens = countInputTokens inputText
          outputTokens = countOutputTokens outputText
      print $ Tokenoid.from (inputTokens, outputTokens)
  where
    countInputTokens = Tokenoid.roughEstimateInput
    countOutputTokens = Tokenoid.roughEstimateOutput

optInfo :: ParserInfo Options
optInfo = info optP fullDesc

optP :: Parser Options
optP = Options <$> optional inputTokenFileP <*> optional outputTokenFileP

inputTokenFileP :: Parser FilePath
inputTokenFileP = tokenFileP 'i' "input-tokens" "File with input tokens"

outputTokenFileP :: Parser FilePath
outputTokenFileP = tokenFileP 'o' "output-tokens" "File with output tokens"

tokenFileP :: Char -> Text -> Text -> Parser FilePath
tokenFileP c l h = strOption (short c <> long (cs l) <> metavar "FILE" <> help (cs h))

-- TODO expose as subcommand or something
_example :: IO ()
_example = void . runTokenoidIO @Int $ do
  produce 5
  consume 10
  print =<< spent
  void . replicateM 3 $ do
    produce 8
    consume 24
  print =<< spent
  print . cost (Rates inputCost outputCost) =<< get
  where
    inputCost, outputCost :: Double
    inputCost = 1.25
    outputCost = 2.75
