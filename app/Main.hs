{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import MHLib

import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Control.Monad (mzero)
import Data.Csv
import Data.Time
import Data.Text (Text, foldr)
import GHC.Generics (Generic)
import System.Environment

newtype Price = Price Double deriving (Eq, Show)
newtype QDay = QDay (Maybe Day) deriving (Eq, Show)

data Quotation = Quotation QDay Price deriving (Show)


instance FromRecord Quotation where
  parseRecord v
    | length v == 2 = Quotation <$>  (v .! 0)  <*> v .! 1
    | otherwise = mzero

-- String laziness is inneficient, but many APIs uses String type
-- Where is the brigde?
getDay :: Text -> QDay
getDay v = QDay day
  where textToString = Data.Text.foldr (\a acc -> a : acc) []
        day = parseTimeM True defaultTimeLocale "%d/%m/%Y" (textToString v) :: Maybe Day

instance FromField (QDay) where
  parseField s = case runParser (parseField s) of
    (Left _) -> pure $ QDay Nothing
    (Right e) -> pure $ getDay e


instance FromField (Price) where
  parseField s = case runParser (parseField s :: Parser Double) of
    (Left _) -> pure $ Price 0
    (Right n) -> pure $ Price n



main :: IO ()
main = do
  args <- getArgs
  file <- BL.readFile (head args)
  putStrLn $ show file
