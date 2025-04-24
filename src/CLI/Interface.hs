module CLI.Interface (mainMenu) where

import CLI.DeadlineMenu (deadlineMenu)
import CLI.FiltersMenu (filtersMenu)
import CLI.PersistenceMenu (persistenceMenu)
import CLI.TaskManagerMenu (taskManagerMenu)
import Filters (categoryFilter, statusFilter)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import Types

{-
Menu principal da aplicação
- Recebe: Lista atual de tarefas
- Retorna: IO [Task] (lista atualizada por submenus)
Gerencia toda lógica de redirecionamento para submenus e saída do programa
-}
mainMenu :: [Task] -> IO [Task]
mainMenu tasks = do
  hSetBuffering stdin NoBuffering
  putStrLn "\ESC[33mInforme a opção desejada:"
  putStr "\ESC[0m"
  putStrLn "1 - Gerenciar suas Tarefas."
  putStrLn "2 - Gestão de Prazos."
  putStrLn "3 - Filtros Avançados."
  putStrLn "4 - Salvar/Carregar Arquivo."
  putStrLn "5 - Exibir Relatório."
  putStrLn "Q - Para sair sem salvar"
  opt <- getLine
  putStrLn ""
  case opt of
    "q" -> exitSuccess >> return tasks
    "Q" -> exitSuccess >> return tasks
    "1" -> taskManagerMenu tasks
    "2" -> deadlineMenu tasks
    "3" -> filtersMenu tasks
    "4" -> persistenceMenu tasks
    "5" -> showTaskReport tasks
    _ -> do
      putStrLn "Operação inválida"
      return tasks

{-
Gera relatório estatístico
- Recebe: Lista de tarefas
- Retorna: IO [Task] (mesma lista)
- Propósito: Exibir métricas sobre:
  - Total de tarefas
  - Distribuição por status
  - Distribuição por categoria (com porcentagens)
Exibição formatada exigida no console
-}
showTaskReport :: [Task] -> IO [Task]
showTaskReport tasks = do
  putStrLn "Relatório Resumido:"
  let totalTasks = length tasks
  putStrLn $ " - Total de tarefas: " ++ show totalTasks
  let pendingTasks = length (statusFilter Pending tasks)
  let completedTasks = length (statusFilter Completed tasks)
  putStrLn $ " - Pendentes: " ++ show pendingTasks ++ " | Concluídas: " ++ show completedTasks
  if totalTasks > 0
    then do
      putStrLn $ "Distribuição por Categoria:"
      let workTasks = length (categoryFilter Work tasks)
      let studyTasks = length (categoryFilter Study tasks)
      let personalTasks = length (categoryFilter Personal tasks)
      let otherTasks = length (categoryFilter Other tasks)
      putStrLn $ " * Trabalho: " ++ show workTasks ++ " (" ++ show ((fromIntegral workTasks / fromIntegral totalTasks * 100) :: Double) ++ "%)"
      putStrLn $ " * Estudo: " ++ show studyTasks ++ " (" ++ show ((fromIntegral studyTasks / fromIntegral totalTasks * 100) :: Double) ++ "%)"
      putStrLn $ " * Pessoal: " ++ show personalTasks ++ " (" ++ show ((fromIntegral personalTasks / fromIntegral totalTasks * 100) :: Double) ++ "%)"
      putStrLn $ " * Outros: " ++ show otherTasks ++ " (" ++ show ((fromIntegral otherTasks / fromIntegral totalTasks * 100) :: Double) ++ "%)"
    else putStrLn $ "Nenhuma tarefa cadastrada."
  return tasks
