{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Char (toLower)
import Data.List (isInfixOf, sortBy)
import Data.Ord (comparing)
import Filters
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import Types

-- Instância para gerar uma Status aleatório
instance Arbitrary Status where
  arbitrary = elements [Pending, Completed]

-- Instância para gerar uma Priority aleatória
instance Arbitrary Priority where
  arbitrary = elements [Low, Medium, High]

-- Instância para gerar uma Category aleatória
instance Arbitrary Category where
  arbitrary = elements [Work, Study, Personal, Other]

-- Instância para gerar uma Task aleatória
instance Arbitrary Task where
  arbitrary = do
    i <- arbitrary
    d <- listOf (elements ['a' .. 'z'])
    s <- arbitrary
    p <- arbitrary
    c <- arbitrary
    dl <- arbitrary
    ts <- listOf (listOf (elements ['a' .. 'z']))
    return $ Task i d s p c dl ts

-- Propriedade para testar categoryFilter
propCategoryFilter :: Category -> [Task] -> Property
propCategoryFilter cat tasks =
  let filtered = categoryFilter cat tasks
   in conjoin
        [ counterexample "Todas as tarefas filtradas devem ter a categoria correta" $
            all (\t -> category t == cat) filtered,
          counterexample "Todas as tarefas com a categoria correta devem estar no resultado" $
            all (\t -> implies (category t == cat) $ elem t filtered) tasks
        ]
  where
    implies p q = not p || q

propPriorityFilter :: Priority -> [Task] -> Property
propPriorityFilter prio tasks =
  let filtered = priorityFilter prio tasks
   in conjoin
        [ counterexample "Todas as tarefas filtradas devem ter a prioridade correta" $
            all (\t -> priority t == prio) filtered,
          counterexample "Todas as tarefas com a prioridade correta devem estar no resultado" $
            all (\t -> implies (priority t == prio) $ elem t filtered) tasks
        ]
  where
    implies p q = not p || q

propPrioritySorter :: [Task] -> Property
propPrioritySorter tasks =
  let sorted = prioritySorter tasks
   in conjoin
        [ counterexample "A lista ordenada deve estar em ordem crescente de prioridade" $
            isSortedByPriority sorted,
          counterexample "A lista ordenada deve conter os mesmos elementos da original" $
            sorted `sameElements` tasks
        ]
  where
    isSortedByPriority [] = True
    isSortedByPriority [_] = True
    isSortedByPriority (x : y : ys) = priority x <= priority y && isSortedByPriority (y : ys)

    sameElements xs ys =
      sortBy (comparing priority) xs == sortBy (comparing priority) ys

propStatusFilter :: Status -> [Task] -> Property
propStatusFilter stat tasks =
  let filtered = statusFilter stat tasks
   in conjoin
        [ counterexample "Todas as tarefas filtradas devem ter o status correto" $ all (\t -> status t == stat) filtered,
          counterexample "Todas as tarefas com o status correto devem estar no resultado" $ all (\t -> implies (status t == stat) $ elem t filtered) tasks
        ]
  where
    implies p q = not p || q

propKeywordSearch :: String -> [Task] -> Property
propKeywordSearch keyword tasks =
  let search = keywordSearch keyword tasks
      lowerKeyword = toLowerStr keyword
   in conjoin
        [ counterexample "Todas as tarefas no resultado devem conter a palavra-chave" $
            all (\t -> isInfixOf lowerKeyword $ toLowerStr (description t)) search,
          counterexample "Todas as tarefas com a palavra-chave devem estar no resultado" $ all (\t -> implies (isInfixOf lowerKeyword $ toLowerStr (description t)) $ elem t search) tasks
        ]
  where
    toLowerStr :: String -> String
    toLowerStr = map toLower
    implies p q = not p || q

propTagFilter :: String -> [Task] -> Property
propTagFilter tag tasks =
  let filtered = tagFilter tag tasks
   in conjoin
        [ counterexample "Todas as tarefas filtradas devem conter a tag correta" $
            all (\t -> elem tag (tags t)) filtered,
          counterexample "Todas as tarefas com a tag correta devem estar no resultado" $
            all (\t -> implies (elem tag (tags t)) (elem t filtered)) tasks
        ]
  where
    implies p q = not p || q

main :: IO ()
main = do
  putStrLn "Testando todas as funções de Filters.hs"
  putStrLn "Testando categoryFilter"
  quickCheck propCategoryFilter
  putStrLn "Testando priorityFilter"
  quickCheck propPriorityFilter
  putStrLn "Testando prioritySorter"
  quickCheck propPrioritySorter
  putStrLn "Testando statusFilter"
  quickCheck propStatusFilter
  putStrLn "Testando keywordSearch"
  quickCheck propKeywordSearch
  putStrLn "Testando tagFilter"
  quickCheck propTagFilter
