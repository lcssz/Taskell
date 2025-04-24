module CLI.PersistenceMenu (persistenceMenu) where

import Persistence (readTextFile, writeTextFile)
import Types

{-
Menu de persistência de dados
- Recebe: Lista atual de tarefas
- Retorna: IO [Task] (lista atual ou carregada de arquivo)
-}
persistenceMenu :: [Task] -> IO [Task]
persistenceMenu tasks = do
  putStrLn "\ESC[33mSalvar/Carregar Arquivo."
  putStr "\ESC[0m"
  putStrLn "1 - Salvar Tarefas"
  putStrLn "2 - Carregar Tarefas"
  putStrLn "3 - Voltar"
  opt <- getLine
  putStrLn ""
  case opt of
    "1" -> saveFileFlow tasks
    "2" -> loadFileFlow
    "3" -> return tasks
    _ -> putStrLn "Operação inválida" >> persistenceMenu tasks

{-
Fluxo para salvar tarefas em arquivo
- Recebe: Lista de tarefas atual
- Retorna: IO [Task] (mesma lista)
- Efeitos: Cria/sobrescreve arquivo no diretório de estado do app
-}
saveFileFlow :: [Task] -> IO [Task]
saveFileFlow tasks = do
  putStr "Nome do arquivo para salvar: "
  fileName <- getLine
  writeTextFile fileName tasks
  putStrLn $ "Arquivo '" ++ fileName ++ "' salvo com sucesso!"
  persistenceMenu tasks

{-
Fluxo para carregar tarefas de arquivo
- Recebe: Nada (via IO)
- Retorna: IO [Task] (lista carregada ou vazia em caso de erro)
- Efeitos: Lê arquivo e retorna Either com lista ou mensagem de erro
-}
loadFileFlow :: IO [Task]
loadFileFlow = do
  putStr "Nome do arquivo para carregar: "
  fileName <- getLine
  result <- readTextFile fileName
  case result of
    Right loadedTasks -> do
      putStrLn $ "Arquivo '" ++ fileName ++ "' carregado com sucesso!"
      return loadedTasks
    Left err -> do
      putStrLn $ "Erro: " ++ err
      putStrLn "Retornando à lista atual."
      return []
