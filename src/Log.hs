{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Log where

import qualified Data.List.NonEmpty as NE
import Data.Text hiding (reverse)
import Data.Time
import Relude

--------------------------------------------------------------------------------

data Zipper a = Zipper
  { current :: a,
    previous :: [a],
    next :: [a]
  }
  deriving stock (Show, Eq, Ord, Generic, Functor)

zipperToNEList :: Zipper a -> NE.NonEmpty a
zipperToNEList Zipper {..} = case reverse previous of
  [] -> current :| next
  p : ps -> p :| (ps <> [current] <> next)

zipperToList :: Zipper a -> [a]
zipperToList = NE.toList . zipperToNEList

instance Semigroup (Zipper a) where
  z1 <> z2 = z2 {previous = zipperToList z1 <> previous z2}

moveNext :: Zipper a -> Zipper a
moveNext z@(Zipper _ _ []) = z
moveNext (Zipper current previous next) = Zipper current' previous' next'
  where
    (current' : next') = next
    previous' = current : previous

movePrev :: Zipper a -> Zipper a
movePrev z@(Zipper _ [] _) = z
movePrev (Zipper current previous next) = Zipper current' previous' next'
  where
    (current' : previous') = previous
    next' = current : next

--------------------------------------------------------------------------------

data Entry = Entry
  { entryDay :: Day,
    entryFile :: FilePath,
    entryContent :: Text
  }
  deriving (Show, Eq, Ord)
