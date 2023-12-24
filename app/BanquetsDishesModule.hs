{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BanquetsDishesModule where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.List (find)

data BanquetsDishesItem = BanquetsDishesItem
  {
    dish_id :: Int
  , banquet :: Int
  , count :: Int
  } deriving (Show, Generic)

instance FromJSON BanquetsDishesItem
instance ToJSON BanquetsDishesItem


getBanquetsDishes :: IO [BanquetsDishesItem]
getBanquetsDishes = do
  jsonContent <- B.readFile "app/db/banquets-dishes.json"

  case decode (BSLU.fromString (BSLU.toString jsonContent)) of
    Nothing -> do
      putStrLn "Error parsing JSON"
      return []
    Just ingredientsItems -> do
      return ingredientsItems

addBanquetsDishesItem :: Int -> Int -> Int -> IO ()
addBanquetsDishesItem dishId banquetId itemCount = do
  existingBanquetsDishes <- getBanquetsDishes

  -- Создаем новый элемент
  let newItem = BanquetsDishesItem { dish_id = dishId, banquet = banquetId, count = itemCount }

  -- Добавляем новый элемент к существующим
  let updatedBanquetsDishes = existingBanquetsDishes ++ [newItem]

  -- Записываем обновленный список в файл
  writeBanquetsDishes updatedBanquetsDishes

writeBanquetsDishes :: [BanquetsDishesItem] -> IO ()
writeBanquetsDishes items = B.writeFile "app/db/banquets-dishes.json" (encode items)