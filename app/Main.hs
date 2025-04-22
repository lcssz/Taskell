module Main (main) where

import CLI.Interface
import Persistence (readTextFile)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Types

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Deseja iniciar com arquivo? (Deixe vazio caso contrário)"
  putStr "Nome do arquivo: "
  filename <- getLine
  tasks <- loadInitialTasks filename
  mainLoop tasks

mainLoop :: [Task] -> IO ()
mainLoop tasks = do
  newTasks <- mainMenu tasks
  mainLoop newTasks

loadInitialTasks :: String -> IO [Task]
loadInitialTasks fileName = do
  if null fileName
    then do
      putStrLn "Iniciando com lista vazia."
      return []
    else do
      result <- readTextFile fileName
      case result of
        Right tasks -> return tasks
        Left err -> do
          putStrLn $ "Erro: " ++ err
          putStrLn "Iniciando com lista vazia."
          return []
