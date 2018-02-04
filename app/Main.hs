{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import MHLib
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Csv
import Data.Time

newtype DefaultToZero = DefaultToZero Double


instance FromField DefaultToZero where
  parseField s = case runParser (parseField s) of
    (Left _) -> pure $ DefaultToZero 0
    (Right n) -> pure $ DefaultToZero n


-- data Quotation = { date }




main :: IO ()
main = putStrLn "Here begins"
