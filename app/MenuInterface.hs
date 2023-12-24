{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MenuInterface where

import DishModule
import CategoryModule
import IngredientsModule
import DishesIngredientsModule

import Text.Read
import Control.Monad (when)

optionShowDishesByCategoryId :: IO ()
optionShowDishesByCategoryId = do
  categoryItems <- getCategories
  putStrLn "\n"
  printCategoryNames categoryItems
  putStrLn "\nВыберите блюда какой категории необходимо вывести (впишите id категории)"
  categoryChoice <- getLine
  case readMaybe categoryChoice of
    Just number -> do
      menuItems <- getDishes
      let filteredMenuItems = getDishesByCategoryId number menuItems
      printDishNames filteredMenuItems
      when (length filteredMenuItems == 0) $ do
         putStrLn "\nНет блюд у указанной категории"

    Nothing -> do
      putStrLn "Некорректный ввод. Введите число."
      optionShowDishesByCategoryId

searchDishes :: IO ()
searchDishes = do
  putStrLn "\nВведите текст:"
  searchValue <- getLine
  menuItems <- getDishes
  let items = searchDishesByName searchValue menuItems
  printDishNames items
  when (length items == 0) $ do
     putStrLn "\nНет блюд с указанным названием"


sortingDishesByParams :: IO ()
sortingDishesByParams = do
  putStrLn "Впишите id параметра по которому необходимо сортировать:"
  putStrLn "id - 1"
  putStrLn "Название - 2"
  putStrLn "Цена - 3"
  putStrLn "Кол-во грамм - 4"
  putStrLn "Кол-во калорий - 5"

  params <- getLine

  menuItems <- getDishes

  case params of
    "1" -> do
      let items = sortDishesBy (DishModule.id) menuItems
      printDishNames items

    "2" -> do
      let items = sortDishesBy (DishModule.name) menuItems
      printDishNames items

    "3" -> do
      let items = sortDishesBy (DishModule.price) menuItems
      printDishNames items

    "4" -> do
      let items = sortDishesBy (DishModule.grams) menuItems
      printDishNames items

    "5" -> do
      let items = sortDishesBy (DishModule.calories) menuItems
      printDishNames items
    _ -> do
      putStrLn "Некорректный выбор"
      sortingDishesByParams

selectConjunctionOrDisjunction :: [Int] -> IO ()
selectConjunctionOrDisjunction ingrIds = do
  putStrLn "Выберите конъюнкция или дизъюнкция:\n1 - конъюнкция\n2 - дизъюнкция"
  params <- getLine
  menuItems <- getDishes
  putStrLn "\n"
  case params of
    "1" -> do
       dishesIngredients <- getDishesIngredients
       let dishes = getDishesConjunction dishesIngredients ingrIds menuItems
       printDishNames dishes
    "2" -> do
      dishesIngredients <- getDishesIngredients
      let dishes = getDishesDisjunction dishesIngredients ingrIds menuItems
      printDishNames dishes
    _ -> do
      selectConjunctionOrDisjunction ingrIds

getDishesByComponents :: IO ()
getDishesByComponents = do
  ingredientItems <- getIngredients
  putStrLn "\n"
  printIngredients ingredientItems
  putStrLn "\nВыберите компоненты (впишите id компонентов через запятую 1,2,3...)"
  input <- getLine
  case parseInput input of
    Just numbers -> do
      putStrLn $ "Введённые числа: " ++ show numbers
      selectConjunctionOrDisjunction numbers
    Nothing      -> do
      putStrLn "Ошибка ввода. Пожалуйста, введите числа, разделённые запятой."
      getDishesByComponents

--  categoryChoice <- getLine
--  case readMaybe categoryChoice of
--    Just number -> do
--      menuItems <- getDishes
--      let filteredMenuItems = getDishesByCategoryId number menuItems
--      printDishNames filteredMenuItems
--      when (length filteredMenuItems == 0) $ do
--         putStrLn "\nНет блюд у указанной категории"
--      optionsViewingDishes back
--    Nothing -> do
--      putStrLn "Некорректный ввод. Введите число."
--      optionShowDishesByCategoryId back


optionsViewingDishes :: Bool -> IO ()
optionsViewingDishes shouldContinue = do

    if shouldContinue
        then do

          putStrLn "\nВыберите вариант просмотра меню:"
          putStrLn "1 - Смотреть все блюда"
          putStrLn "2 - Смотреть блюда по категориям"
          putStrLn "3 - Искать блюда по названию"
          putStrLn "4 - Сортировка блюд по параметрам"
          putStrLn "5 - Смотреть блюда по компонентам"
          putStrLn "6 - Назад"

          choice <- getLine

          case choice of
            "1" -> do
              menuItems <- getDishes
              printDishNames menuItems
              optionsViewingDishes True
            "2" -> do
              optionShowDishesByCategoryId
              optionsViewingDishes True
            "3" -> do
              searchDishes
              optionsViewingDishes True
            "4" -> do
              sortingDishesByParams
              optionsViewingDishes True
            "5" -> do
              getDishesByComponents
              optionsViewingDishes True
            "6" -> do
              putStrLn "Назад"
            _ -> do
              putStrLn "Некорректный выбор"
              optionsViewingDishes True
        else putStrLn ""

