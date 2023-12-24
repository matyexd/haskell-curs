{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics

import MenuInterface

main :: IO ()
main = do
  putStrLn "Выберите действие:"
  putStrLn "1 - Смотреть меню"
  putStrLn "2 - Смотреть что-то еще"  -- Замените это на ваше второе действие
  putStrLn "3 - Выход из программы"

  choice <- getLine  -- Получаем ввод пользователя

  -- Загрузить содержимое файла
--  menuItems <- getDishes
--  printDishNames menuItems
--
--  categoryItems <- getCategories
--  printCategoryNames categoryItems
  case choice of
    "1" -> do
      optionsViewingDishes(main)
    "2" -> do
      putStrLn("Реализация второго действия")
    "3" -> do
      putStrLn "Выход из программы"
    _ -> do
      putStrLn "Некорректный выбор"
      main