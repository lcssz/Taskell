module CLI.FiltersMenu (filtersMenu) where

import Data.Char (toLower)
import Filters
  ( categoryFilter,
    filterByStatus,
    keywordSearch,
    priorityFilter,
    prioritySorter,
    tagFilter,
  )
import Text.Read (readMaybe)
import Types

filtersMenu :: [Task] -> IO [Task]
filtersMenu tasks = do
  putStrLn "\ESC[33mFiltros Avançados."
  putStr "\ESC[0m"
  putStrLn "1 - Filtrar por Categoria"
  putStrLn "2 - Filtrar por Prioridade"
  putStrLn "3 - Filtrar por Status"
  putStrLn "4 - Buscar por Palavra-Chave"
  putStrLn "5 - Filtrar por Tag"
  putStrLn "6 - Ordenar por Prioridade"
  putStrLn "7 - Voltar"
  opt <- getLine
  putStrLn ""
  case opt of
    "1" -> categoryFilterFlow tasks
    "2" -> priorityFilterFlow tasks
    "3" -> statusFilterFlow tasks
    "4" -> keywordFilterFlow tasks
    "5" -> tagFilterFlow tasks
    "6" -> prioritySortFlow tasks
    "7" -> return tasks
    _ -> putStrLn "Operação inválida" >> filtersMenu tasks

-- Fluxo para filtro de categoria
categoryFilterFlow :: [Task] -> IO [Task]
categoryFilterFlow tasks = do
  putStrLn "Categorias disponíveis:"
  putStrLn "1 - Trabalho | 2 - Estudo | 3 - Pessoal | 4 - Outro"
  putStr "Escolha: "
  input <- readMaybe <$> getLine
  case input of
    Just 1 -> showFiltered (categoryFilter Work tasks)
    Just 2 -> showFiltered (categoryFilter Study tasks)
    Just 3 -> showFiltered (categoryFilter Personal tasks)
    Just 4 -> showFiltered (categoryFilter Other tasks)
    _ -> putStrLn "Categoria inválida!" >> filtersMenu tasks
  where
    showFiltered filtered = do
      putStrLn "\nTarefas filtradas:"
      mapM_ print filtered
      filtersMenu tasks -- Mantém a lista original

-- Fluxo para filtro de prioridade
priorityFilterFlow :: [Task] -> IO [Task]
priorityFilterFlow tasks = do
  putStrLn "Prioridades disponíveis:"
  putStrLn "1 - Baixa | 2 - Média | 3 - Alta"
  putStr "Escolha: "
  input <- readMaybe <$> getLine
  case input of
    Just 1 -> showFiltered (priorityFilter Low tasks)
    Just 2 -> showFiltered (priorityFilter Medium tasks)
    Just 3 -> showFiltered (priorityFilter High tasks)
    _ -> putStrLn "Prioridade inválida!" >> filtersMenu tasks
  where
    showFiltered filtered = do
      putStrLn "\nTarefas filtradas:"
      mapM_ print filtered
      filtersMenu tasks

-- Fluxo para filtro de status
statusFilterFlow :: [Task] -> IO [Task]
statusFilterFlow tasks = do
  putStrLn "Status disponíveis:"
  putStrLn "1 - Pendente | 2 - Concluída"
  putStr "Escolha: "
  input <- readMaybe <$> getLine
  case input of
    Just 1 -> showFiltered (filterByStatus Pending tasks)
    Just 2 -> showFiltered (filterByStatus Completed tasks)
    _ -> putStrLn "Status inválido!" >> filtersMenu tasks
  where
    showFiltered filtered = do
      putStrLn "\nTarefas filtradas:"
      mapM_ print filtered
      filtersMenu tasks

-- Fluxo para busca por palavra-chave
keywordFilterFlow :: [Task] -> IO [Task]
keywordFilterFlow tasks = do
  putStr "Digite a palavra-chave: "
  keyword <- getLine
  let filtered = keywordSearch keyword tasks
  if null filtered
    then putStrLn "Nenhuma tarefa encontrada!"
    else do
      putStrLn "\nTarefas encontradas:"
      mapM_ print filtered
  filtersMenu tasks

-- Fluxo para filtro de tag
tagFilterFlow :: [Task] -> IO [Task]
tagFilterFlow tasks = do
  putStr "Digite a tag (ex: urgente): "
  tag <- getLine
  let filtered = tagFilter (map toLower tag) tasks
  if null filtered
    then putStrLn "Nenhuma tarefa encontrada!"
    else do
      putStrLn "\nTarefas encontradas:"
      mapM_ print filtered
  filtersMenu tasks

-- Fluxo para ordenação por prioridade
prioritySortFlow :: [Task] -> IO [Task]
prioritySortFlow tasks = do
  let sorted = prioritySorter tasks
  putStrLn "\nTarefas ordenadas por prioridade:"
  mapM_ print sorted
  filtersMenu tasks
