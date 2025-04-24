module Deadline (checkOverdueTasks, remainingDays) where

import Data.Time (Day, diffDays)
import Types

{-
Filtra tarefas vencidas:
- Recebe lista de tarefas e data de referência
- Retorna apenas tarefas com vencimento anterior à data informada
- Ignora tarefas sem vencimento definido (Nothing)
-}
checkOverdueTasks :: [Task] -> Day -> [Task]
checkOverdueTasks xt day = filter verifyDay xt
  where
    verifyDay task = maybe False (day >) (deadline task)

{-
Calcula dias restantes até o vencimento:
- Recebe uma tarefa e data de referência
- Retorna Nothing se não houver vencimento definido
- Retorna Just n onde n é o número de dias restantes
-}
remainingDays :: Task -> Day -> Maybe Int
remainingDays task day = fmap diffDaysInt (deadline task)
  where
    diffDaysInt = fromInteger . diffDays day
