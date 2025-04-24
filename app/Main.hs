module Main (main) where

import CLI.Interface
import Persistence (readTextFile)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Types

{-
Ponto de entrada principal da aplicação
- Retorna: IO ()
- Propósito:
  1. Configurar buffer de saída para comportamento interativo
  2. Perguntar se usuário deseja carregar de arquivo
  3. Inicializar lista de tarefas (vazia ou do arquivo)
  4. Iniciar loop principal
-}
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- Desativa buffering para I/O interativo
  putStrLn "Deseja iniciar com arquivo? (Deixe vazio caso contrário)"
  putStr "Nome do arquivo: "
  filename <- getLine
  tasks <- loadInitialTasks filename -- Carrega tarefas iniciais
  mainLoop tasks -- Entra no loop principal

{-
Loop principal da aplicação
- Recebe: [Task] (lista atual de tarefas)
- Retorna: IO () (loop infinito)
- Comportamento:
  - Sempre recebe e repassa a lista atualizada de tarefas
  - Executa indefinidamente até usuário sair (via menu)
-}
mainLoop :: [Task] -> IO ()
mainLoop tasks = do
  newTasks <- mainMenu tasks -- Executa menu e obtém estado atualizado
  mainLoop newTasks -- Chama a si mesmo recursivamente

{-
Carregador inicial de tarefas
- Recebe: String (nome do arquivo ou string vazia)
- Retorna: IO [Task] (lista de tarefas carregada ou vazia)
- Comportamento:
  - Se filename for vazio: retorna lista vazia
  - Se filename existir: carrega tarefas do arquivo
  - Se erro ocorrer: mostra mensagem e retorna lista vazia
-}
loadInitialTasks :: String -> IO [Task]
loadInitialTasks fileName = do
  if null fileName
    then do
      putStrLn "Iniciando com lista vazia."
      return [] -- Estado inicial padrão
    else do
      result <- readTextFile fileName
      case result of
        Right tasks -> return tasks -- Sucesso no carregamento
        Left err -> do
          putStrLn $ "Erro: " ++ err
          putStrLn "Iniciando com lista vazia."
          return [] -- Fallback para lista vazia em caso de erro
