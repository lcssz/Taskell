module Persistence (writeTextFile, readTextFile) where

import System.Directory
import System.FilePath (replaceExtension, takeExtension, (</>))
import Types

serializeTasks :: [Task] -> String
serializeTasks = show

deserializeTasks :: String -> Maybe [Task]
deserializeTasks str =
  case reads str of
    [(tasks, "")] -> Just tasks
    _ -> Nothing

getAppPath :: IO FilePath
getAppPath = do
  dir <- getXdgDirectory XdgState "taskell"
  createDirectoryIfMissing True dir
  return dir

ensureDatExtension :: FilePath -> FilePath
ensureDatExtension file_name
  | takeExtension file_name == ".dat" = file_name
  | otherwise = replaceExtension file_name ".dat"

writeTextFile :: FilePath -> [Task] -> IO ()
writeTextFile file_name tasks = do
  dir <- getAppPath
  let full_path = dir </> ensureDatExtension file_name
  writeFile full_path (serializeTasks tasks)

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
