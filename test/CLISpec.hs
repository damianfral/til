{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLISpec (spec) where

import CLI
import Data.GenValidity
import Options.Generic
import Relude hiding (sort)
import Test.Syd
import Test.Syd.Validity

toArg :: Text -> Maybe Text -> [Text]
toArg _ Nothing = []
toArg name (Just v) = ["--" <> name, v]

spec :: Spec
spec =
  it "decodes valid options" $ do
    forAllValid $ \options@(Options {..}) -> do
      let record =
            mconcat
              [ toArg "name" $ Just name,
                toArg "enthusiasm" $ Just $ show $ fromEnum enthusiasm
              ]
      unwrapRecordPure record `shouldBe` Just options
