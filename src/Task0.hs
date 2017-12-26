module Task0 where

import Data.Monoid (Sum(..), getSum)
import Data.Set
import Data.Text

type TaskState = String
type TaskTag = String

data Task = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [Task]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: Int
  }

taskCost :: Task -> Int
taskCost task = 
  estimate task + (sum $ fmap taskCost (dependsOn task))
