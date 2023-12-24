{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CategoryModule where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.List (find)

data CategoryItem = CategoryItem
  { id :: Int
  , name :: String
  } deriving (Show, Generic)

instance FromJSON CategoryItem

printCategoryNames :: [CategoryItem] -> IO ()
printCategoryNames categoryItems =
  mapM_ (\(category) -> putStrLn $ "id - " ++ show (CategoryModule.id category) ++ ", Название - " ++ CategoryModule.name category) categoryItems

getCategories :: IO [CategoryItem]
getCategories = do
  jsonContent <- B.readFile "app/db/categories.json"

  case decode (BSLU.fromString (BSLU.toString jsonContent)) of
    Nothing -> do
      putStrLn "Error parsing JSON"
      return []  -- В случае ошибки возвращаем пустой список
    Just menuItems -> do
      return menuItems  -- Возвращаем разобранный список блюд

getCategoryById :: Int -> [CategoryItem] -> Maybe CategoryItem
getCategoryById targetId categories =
  find (\category -> CategoryModule.id category == targetId) categories