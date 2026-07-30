{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.AppState where

import Control.Exception (try)
import Control.Lens
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import Data.Time (Day, fromGregorianValid)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import Data.Zipper
import Relude hiding ((<|>))
import System.Directory (listDirectory)
import qualified System.FilePath as FP
import Text.Parsec (digit, (<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec (unexpected)
import UI.AppConfig

data AppState = AppState {entries :: Zipper Day, markdown :: Text, help :: Bool}
  deriving (Eq, Show, Ord, Generic)

data Resources = SideBar | Content Day
  deriving (Eq, Show, Ord)

makeAppState :: AppConfig -> IO AppState
makeAppState AppConfig {..} = do
  today <- getCurrentDay
  paths <- Relude.filter isMarkdownFile <$> listDirectory appConfigLogPath
  let daysFromFiles = rights $ parseDay appConfigLogPath <$> paths
  let previousDays = case sortBy (comparing Down) daysFromFiles of
        [] -> []
        mostRecentDay : rest ->
          if mostRecentDay == today then rest else mostRecentDay : rest
  let entries = Zipper today previousDays []
  entryContent <- readLogFile $ dayToFilePath appConfigLogPath $ entries ^. #current
  pure $ AppState {entries = entries, markdown = entryContent, help = False}
  where
    isMarkdownFile file = FP.takeExtension file == ".md"

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime

dayToFilePath :: FilePath -> Day -> FilePath
dayToFilePath parent d =
  parent FP.</> formatShow iso8601Format d FP.<.> "md"

readLogFile :: (MonadIO m) => FilePath -> m Text
readLogFile file = do
  eContent <- liftIO $ try $ BS.readFile file
  case eContent of
    Left (SomeException _) -> pure ""
    Right c -> pure $ decodeUtf8With lenientDecode c

parseDay :: Parsec.SourceName -> FilePath -> Either Parsec.ParseError Day
parseDay = Parsec.runParser day ()

day :: Parsec.ParsecT String u Identity Day
day = do
  sign <- (P.char '-' $> negate) <|> (P.char '+' $> identity) <|> pure identity
  y <- year
  _ <- P.char '-'
  m <- twoDigits
  _ <- P.char '-'
  d <- twoDigits
  maybe (unexpected "invalid date") pure (fromGregorianValid (sign y) m d)

year :: P.ParsecT String u Identity Integer
year = do
  ds <- some digit
  if length ds < 4
    then unexpected "expected year with at least 4 digits"
    else pure (foldl' step 0 ds)
  where
    step a w = a * 10 + fromIntegral (ord w - 48)

twoDigits :: Parsec.ParsecT String u Identity Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  pure $ c2d a * 10 + c2d b
