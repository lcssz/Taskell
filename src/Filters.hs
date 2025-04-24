module Filters (categoryFilter, priorityFilter, prioritySorter, statusFilter, keywordSearch, tagFilter, tagCloud) where

import Data.Char (toLower)
import Data.List (group, isInfixOf, sort, sortBy)
import Data.Ord (comparing)
import Types

categoryFilter :: Category -> [Task] -> [Task]
categoryFilter cat = filter (\task -> category task == cat)

priorityFilter :: Priority -> [Task] -> [Task]
priorityFilter prio = filter (\task -> priority task == prio)

prioritySorter :: [Task] -> [Task]
prioritySorter = sortBy (comparing priority)

statusFilter :: Status -> [Task] -> [Task]
statusFilter stat = filter (\task -> status task == stat)

toLowerStr :: String -> String
toLowerStr = map toLower

keywordSearch :: String -> [Task] -> [Task]
keywordSearch keyword = filter (\task -> isInfixOf (toLowerStr keyword) (toLowerStr (description task)))

tagFilter :: String -> [Task] -> [Task]
tagFilter tag = filter (\task -> elem tag (tags task))

tagCloud :: [Task] -> [(String, Int)]
tagCloud tasks =
  let allTags = concatMap tags tasks
      sortedTags = sort allTags
      groupedTags = group sortedTags
   in map (\g -> (head g, length g)) groupedTags
