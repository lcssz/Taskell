module Persistence (writeTextFile, readTextFile) where

import System.Directory
import System.FilePath (replaceExtension, takeExtension, (</>))
import Types

{-
Converte lista de Tasks em String
Usa instância Show da Task
-}
serializeTasks :: [Task] -> String
serializeTasks = show

{-
Converte String em lista de Tasks
Retorna Nothing se falhar o parsing
-}
deserializeTasks :: String -> Maybe [Task]
deserializeTasks str =
  case reads str of
    [(tasks, "")] -> Just tasks
    _ -> Nothing

{-
Obtém diretório de armazenamento do app:
1. Usa XDG_STATE_HOME/taskell
2. Cria diretório se não existir
-}
getAppPath :: IO FilePath
getAppPath = do
  dir <- getXdgDirectory XdgState "taskell"
  createDirectoryIfMissing True dir
  return dir

{-
Garante extensão .dat no nome do arquivo
- Exemplo: "tarefas" -> "tarefas.dat"
-}
ensureDatExtension :: FilePath -> FilePath
ensureDatExtension file_name
  | takeExtension file_name == ".dat" = file_name
  | otherwise = replaceExtension file_name ".dat"

{-
Salva tarefas em arquivo:
1. Constrói caminho completo (diretório + nome.dat)
2. Serializa e grava dados
-}
writeTextFile :: FilePath -> [Task] -> IO ()
writeTextFile file_name tasks = do
  dir <- getAppPath
  let full_path = dir </> ensureDatExtension file_name
  writeFile full_path (serializeTasks tasks)

{-
Carrega tarefas de arquivo:
1. Verifica se arquivo existe
2. Lê e desserializa ou retorna erro
- Retorna Either com lista de tasks ou mensagem de erro
-}
readTextFile :: FilePath -> IO (Either String [Task])
readTextFile file_name = do
  dir <- getAppPath
  let full_path = dir </> ensureDatExtension file_name

  existFile <- doesFileExist full_path

  if existFile
    then do
      contents <- readFile full_path
      return $ case deserializeTasks contents of
        Just tasks -> Right tasks
        Nothing -> Left "Erro de formatação no arquivo!"
    else return (Left "Arquivo não encontrado!")
