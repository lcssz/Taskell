module Filters (categoryFilter, priorityFilter, prioritySorter, statusFilter, keywordSearch, tagFilter, tagCloud) where

import Data.Char (toLower)
import Data.List (group, isInfixOf, sort, sortBy)
import Data.Ord (comparing)
import Types

{-
Filtra as tarefas pela categoria especificada.
- Recebe uma categoria e uma lista de tarefas.
- Retorna uma nova lista contendo apenas tarefas com a categoria especificada.
-}
categoryFilter :: Category -> [Task] -> [Task]
categoryFilter cat = filter (\task -> category task == cat)

{-
Filtra as tarefas pelo nível de prioridade.
- Recebe uma prioridade e uma lista de tarefas.
- Retorna uma nova lista contendo apenas tarefas com a prioridade especificada.
-}
priorityFilter :: Priority -> [Task] -> [Task]
priorityFilter prio = filter (\task -> priority task == prio)

{-
Ordena tarefas por prioridade.
- Recebe uma lista de tarefas.
- Retorna a lista ordenada usando a ordem definida na instância Ord do tipo Priority.
- Utiliza a função comparing para criar uma comparação baseada no campo priority.
-}
prioritySorter :: [Task] -> [Task]
prioritySorter = sortBy (comparing priority)

{-
Filtra tarefas pelo status (ex: concluída, pendente).
- Recebe um status e uma lista de tarefas.
- Retorna uma nova lista contendo apenas tarefas com o status especificado.
-}
statusFilter :: Status -> [Task] -> [Task]
statusFilter stat = filter (\task -> status task == stat)

toLowerStr :: String -> String
toLowerStr = map toLower

{-
Busca por palavras-chave na descrição das tarefas.
- Recebe uma palavra-chave e uma lista de tarefas.
- Retorna tarefas onde a descrição contém a palavra-chave como substring.
-}
keywordSearch :: String -> [Task] -> [Task]
keywordSearch keyword = filter (\task -> isInfixOf (toLowerStr keyword) (toLowerStr (description task)))

{-
Filtra tarefas que possuem uma tag específica.
- Recebe uma tag (String) e uma lista de tarefas.
- Retorna uma nova lista contendo apenas tarefas com a tag especificada.
-}
tagFilter :: String -> [Task] -> [Task]
tagFilter tag = filter (\task -> elem tag (tags task))

{-
Gera uma nuvem de tags com contagem de ocorrências.
- Recebe uma lista de tarefas.
- Extrai e concatena todas as tags de todas as tarefas.
- Agrupa tags idênticas e conta suas ocorrências.
- Retorna lista de tuplas (Tag, Quantidade).
-}
tagCloud :: [Task] -> [(String, Int)]
tagCloud tasks =
  let allTags = concatMap tags tasks -- Extrai todas as tags
      sortedTags = sort allTags -- Ordena para agrupamento
      groupedTags = group sortedTags -- Agrupa tags iguais
   in map (\g -> (head g, length g)) groupedTags -- Formata (tag, contagem)
