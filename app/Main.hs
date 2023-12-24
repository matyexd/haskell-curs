{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics
import System.Exit (exitSuccess)

import MenuInterface
import BanquetsInterface

main :: IO ()
main = do
  putStrLn "Выберите действие:"
  putStrLn "1 - Смотреть меню"
  putStrLn "2 - Бронирование банкета"
  putStrLn "3 - Выход из программы"

  choice <- getLine

  case choice of
    "1" -> do
      optionsViewingDishes True
    "2" -> do
      optionsBookBanquet
    "3" -> do
      putStrLn "Выход из программы"
      exitSuccess
    _ -> do
      putStrLn "Некорректный выбор"
  main