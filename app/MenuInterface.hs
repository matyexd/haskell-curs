{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MenuInterface where

import DishModule
import CategoryModule
import Text.Read
import Control.Monad (when)

optionShowDishesByCategoryId :: IO () -> IO ()
optionShowDishesByCategoryId back = do
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
      optionsViewingDishes back
    Nothing -> do
      putStrLn "Некорректный ввод. Введите число."
      optionShowDishesByCategoryId back

searchDishes :: IO () -> IO ()
searchDishes back = do
  putStrLn "\nВведите текст:"
  searchValue <- getLine
  menuItems <- getDishes
  let items = searchDishesByName searchValue menuItems
  printDishNames items
  when (length items == 0) $ do
     putStrLn "\nНет блюд с указанным названием"
  optionsViewingDishes back

sortingDishesByParams :: IO () -> IO ()
sortingDishesByParams back = do
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
      optionsViewingDishes back
    "2" -> do
      let items = sortDishesBy (DishModule.name) menuItems
      printDishNames items
      optionsViewingDishes back
    "3" -> do
      let items = sortDishesBy (DishModule.price) menuItems
      printDishNames items
      optionsViewingDishes back
    "4" -> do
      let items = sortDishesBy (DishModule.grams) menuItems
      printDishNames items
      optionsViewingDishes back
    "5" -> do
      let items = sortDishesBy (DishModule.calories) menuItems
      printDishNames items
      optionsViewingDishes back
    _ -> do
      putStrLn "Некорректный выбор"
      sortingDishesByParams back




optionsViewingDishes :: IO () -> IO ()
optionsViewingDishes back = do
    putStrLn "\nВыберите вариант просмотра меню:"
    putStrLn "1 - Смотреть все блюда"
    putStrLn "2 - Смотреть блюда по категориям"  -- Замените это на ваше второе действие
    putStrLn "3 - Искать блюда по названию"  -- Замените это на ваше второе действие
    putStrLn "4 - Сортировка блюд по параметрам"  -- Замените это на ваше второе действие
    putStrLn "6 - Назад"

    choice <- getLine

    case choice of
      "1" -> do
        menuItems <- getDishes
        printDishNames menuItems
        optionsViewingDishes back
      "2" -> do
        optionShowDishesByCategoryId back
      "3" -> do
        searchDishes back
      "4" -> do
        sortingDishesByParams back
      "6" -> do
        back
      _ -> do
        putStrLn "Некорректный выбор"
        optionsViewingDishes back
