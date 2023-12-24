{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DishModule where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.Char (toLower)
import Data.List (isInfixOf, sortOn, find, intercalate)

--import CategoryModule
--import DishesIngredientsModule
--import IngredientsModule

data MenuItem = MenuItem
  { id :: Int
  , name :: String
  , price :: Double
  , grams :: Int
  , calories :: Int
  , category_id :: Int
  } deriving (Show, Generic)

instance FromJSON MenuItem

-- ++ show (getCategories DishModule.category_id dish categories) ++ "\n"

--ingredientNamesByIds :: [Int] -> [IngredientItem] -> String
--ingredientNamesByIds ids ingredientList =
--  let matchingIngredients = filter (\ingredient -> IngredientsModule.id ingredient `elem` ids) ingredientList
--  in intercalate ", " (map IngredientsModule.name matchingIngredients)
--
--printDishNames :: [MenuItem] -> IO ()
--printDishNames menuItems = do
--  categories <- getCategories
--  dishesIngredients <- getDishesIngredients
--  ingredients <- getIngredients
--  mapM_ (\(dish) -> putStrLn $ "id - " ++ show (DishModule.id dish) ++
--    "\nНазвание - " ++ DishModule.name dish ++
--    "\nЦена - " ++ show (DishModule.price dish) ++
--    "\nКол-во грамм - " ++ show(DishModule.grams dish) ++
--    "\nКол-во калорий - " ++ show (DishModule.calories dish) ++
--    "\nКатегория - " ++ maybe "Неизвестная категория" CategoryModule.name (getCategoryById (DishModule.category_id dish) categories) ++
--    "\n - " ++ ingredientNamesByIds (getIngredientIds dishesIngredients dish) ingredients) menuItems

getDishes :: IO [MenuItem]
getDishes = do
  jsonContent <- B.readFile "app/db/dishes.json"

  case decode (BSLU.fromString (BSLU.toString jsonContent)) of
    Nothing -> do
      return []  -- В случае ошибки возвращаем пустой список
    Just menuItems -> do
      return menuItems  -- Возвращаем разобранный список блюд.


getDishesByCategoryId :: Int -> [MenuItem] -> [MenuItem]
getDishesByCategoryId categoryId menuItems = filter (\dish -> DishModule.category_id dish == categoryId) menuItems

searchDishesByName :: String -> [MenuItem] -> [MenuItem]
searchDishesByName query menuItems =
  filter (\dish -> isInfixOf (map toLower query) (map toLower (DishModule.name dish))) menuItems

sortDishesBy :: Ord b => (MenuItem -> b) -> [MenuItem] -> [MenuItem]
sortDishesBy extractor = sortOn extractor

getDish :: Int -> [MenuItem] -> Maybe MenuItem
getDish targetId menuItems = find (\dish -> DishModule.id dish == targetId) menuItems


--searchDishesByName :: String -> IO [MenuItem]
--searchDishesByName categoryId = do
--  menuItems <- getDishes
--  let filteredMenuItems = filter (\dish -> DishModule.category_id dish == categoryId) menuItems
--  return filteredMenuItems