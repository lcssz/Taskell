module Filters (categoryFilter, priorityFilter, prioritySorter, filterByStatus, keywordSearch, tagFilter) where

import Data.Char (toLower)
import Data.List (isInfixOf, sortBy)
import Data.Ord (comparing)
import Types

categoryFilter :: Category -> [Task] -> [Task]
categoryFilter = filter . (. category) . (==)

priorityFilter :: Priority -> [Task] -> [Task]
priorityFilter = filter . (. priority) . (==)

prioritySorter :: [Task] -> [Task]
prioritySorter = sortBy (comparing priority)

filterByStatus :: Status -> [Task] -> [Task]
filterByStatus = filter . (. status) . (==)

toLowerStr :: String -> String
toLowerStr = map toLower

keywordSearch :: String -> [Task] -> [Task]
keywordSearch = filter . (. (toLowerStr . description)) . isInfixOf . toLowerStr

tagFilter :: String -> [Task] -> [Task]
tagFilter = filter . (. tags) . elem
