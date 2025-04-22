module CLI.TaskManagerMenu (taskManagerMenu) where

import Data.List.Split (splitOn)
import Data.Time (Day)
import TaskManager (addTask, createTask, markCompleted, removeTask)
import Text.Read (readMaybe)
import Types

taskManagerMenu :: [Task] -> IO [Task]
taskManagerMenu tasks = do
  putStrLn "\ESC[33mGerenciador de Tarefas."
  putStr "\ESC[0m"
  putStrLn "1 - Adicionar Tarefa."
  putStrLn "2 - Remover Tarefa."
  putStrLn "3 - Marcar Tarefa como Concluída."
  putStrLn "4 - Voltar."
  opt <- getLine
  putStrLn ""
  case opt of
    "1" -> addTaskFlow tasks
    "2" -> removeTaskFlow tasks
    "3" -> markCompletedFlow tasks
    "4" -> return tasks
    _ -> putStrLn "Operação inválida" >> taskManagerMenu tasks

-- Fluxo para adicionar tarefa
addTaskFlow :: [Task] -> IO [Task]
addTaskFlow tasks = do
  maybeId <- getTaskId tasks
  maybeDesc <- getDescription
  maybeStatus <- getStatus
  maybePriority <- getPriority
  maybeCategory <- getCategory
  maybeDeadline <- getDeadline
  maybeTags <- getTags

  case (maybeId, maybeDesc, maybeStatus, maybePriority, maybeCategory, maybeTags) of
    (Just id', Just desc', Just status', Just priority', Just category', Just tags') -> do
      let novaTarefa = createTask id' desc' status' priority' category' maybeDeadline tags'
      let tarefasAtualizadas = addTask novaTarefa tasks
      putStrLn "Tarefa adicionada com sucesso!"
      taskManagerMenu tarefasAtualizadas
    _ -> do
      putStrLn "Erro: Dados inválidos para criação da tarefa!"
      taskManagerMenu tasks

-- Fluxo para remover tarefa
removeTaskFlow :: [Task] -> IO [Task]
removeTaskFlow tasks = do
  print tasks
  putStr "Insira o ID para remover: "
  input <- readMaybe <$> getLine

  case input of
    Just searchId ->
      if any (\t -> taskId t == searchId) tasks
        then do
          let tarefasAtualizadas = removeTask searchId tasks
          putStrLn "Tarefa removida com sucesso!"
          taskManagerMenu tarefasAtualizadas
        else taskManagerMenu tasks
    Nothing -> taskManagerMenu tasks

-- Fluxo para marcar tarefa como concluída
markCompletedFlow :: [Task] -> IO [Task]
markCompletedFlow tasks = do
  print tasks
  putStr "Insira o ID para marcar como concluída: "
  input <- readMaybe <$> getLine

  case input of
    Just searchId ->
      if any (\t -> taskId t == searchId) tasks
        then do
          let tarefasAtualizadas = markCompleted searchId tasks
          putStrLn "Tarefa marcada como concluída com sucesso!"
          taskManagerMenu tarefasAtualizadas
        else taskManagerMenu tasks
    Nothing -> taskManagerMenu tasks

-- Funções auxiliares para obter e validar dados
getTaskId :: [Task] -> IO (Maybe Int)
getTaskId tasks = do
  putStr "Digite o ID da Task: "
  input <- readMaybe <$> getLine
  case input of
    Nothing -> do
      putStrLn "Erro: ID inválido!"
      return Nothing
    Just searchId ->
      if any (\task -> taskId task == searchId) tasks
        then do
          putStrLn "Erro: ID já existe!"
          return Nothing
        else return (Just searchId)

getDescription :: IO (Maybe String)
getDescription = do
  putStr "Descrição da tarefa: "
  desc <- getLine
  if null desc
    then do
      putStrLn "Erro: Descrição vazia!"
      return Nothing
    else return (Just desc)

getStatus :: IO (Maybe Status)
getStatus = do
  putStrLn "Status da tarefa:"
  putStrLn "1 - Pendente | 2 - Concluída"
  putStr "Escolha: "
  input <- readMaybe <$> getLine
  case input of
    Just 1 -> return (Just Pending)
    Just 2 -> return (Just Completed)
    _ -> do
      putStrLn "Erro: Status inválido!"
      return Nothing

getPriority :: IO (Maybe Priority)
getPriority = do
  putStrLn "Prioridade da tarefa:"
  putStrLn "1 - Baixa | 2 - Média | 3 - Alta"
  putStr "Escolha: "
  input <- readMaybe <$> getLine
  case input of
    Just 1 -> return (Just Low)
    Just 2 -> return (Just Medium)
    Just 3 -> return (Just High)
    _ -> do
      putStrLn "Erro: Prioridade inválida!"
      return Nothing

getCategory :: IO (Maybe Category)
getCategory = do
  putStrLn "Categoria da tarefa:"
  putStrLn "1 - Trabalho | 2 - Estudo | 3 - Pessoal | 4 - Outro"
  putStr "Escolha: "
  input <- readMaybe <$> getLine
  case input of
    Just 1 -> return (Just Work)
    Just 2 -> return (Just Study)
    Just 3 -> return (Just Personal)
    Just 4 -> return (Just Other)
    _ -> do
      putStrLn "Erro: Categoria inválida!"
      return Nothing

getDeadline :: IO (Maybe Day)
getDeadline = do
  putStr "Prazo (AAAA-MM-DD ou vazio): "
  input <- getLine
  if null input
    then return Nothing
    else case readMaybe input of
      Just date -> return (Just date)
      Nothing -> do
        putStrLn "Erro: Formato de data inválido!"
        return Nothing

getTags :: IO (Maybe [String])
getTags = do
  putStr "Tags (separadas por vírgula ou vazio): "
  tagsStr <- getLine
  if null tagsStr
    then return Nothing
    else return $ Just (splitOn "," tagsStr)
