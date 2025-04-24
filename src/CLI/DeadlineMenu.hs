module CLI.DeadlineMenu (deadlineMenu) where

import Data.Time (getCurrentTime, utctDay)
import Deadline (checkOverdueTasks, remainingDays)
import Types

{-
Menu de gestão de prazos
- Recebe: Lista atual de tarefas
- Retorna: IO [Task] (lista original)
-}
deadlineMenu :: [Task] -> IO [Task]
deadlineMenu tasks = do
  putStrLn "\ESC[33mGestão de Prazos."
  putStr "\ESC[0m"
  putStrLn "1 - Ver Tarefas Expiradas"
  putStrLn "2 - Ver Dias Restantes para Tarefas"
  putStrLn "3 - Voltar"
  opt <- getLine
  putStrLn ""
  case opt of
    "1" -> showOverdueTasks tasks
    "2" -> showRemainingDays tasks
    "3" -> return tasks
    _ -> putStrLn "Operação inválida" >> deadlineMenu tasks

{-
Exibe tarefas com prazos expirados
- Recebe: Lista de tarefas
- Retorna: IO [Task] (lista original)
-}
showOverdueTasks :: [Task] -> IO [Task]
showOverdueTasks tasks = do
  currentDay <- utctDay <$> getCurrentTime
  let overdue = checkOverdueTasks tasks currentDay
  if null overdue
    then putStrLn "Nenhuma tarefa expirada!"
    else mapM_ print overdue
  deadlineMenu tasks

{-
Exibe dias restantes para cada tarefa
- Recebe: Lista de tarefas
- Retorna: IO [Task] (lista original)
-}
showRemainingDays :: [Task] -> IO [Task]
showRemainingDays tasks = do
  currentDay <- utctDay <$> getCurrentTime
  let daysList = [(description t, remainingDays t currentDay) | t <- tasks]
  if null daysList
    then putStrLn "Nenhuma tarefa com prazo!"
    else mapM_ (\(desc, days) -> putStrLn $ desc ++ ": " ++ show days) daysList
  deadlineMenu tasks
