module Task1 where

import Data.List.NonEmpty
import Data.Semigroup
import Data.Set
import Data.Text

type TaskState = String
type TaskTag = String

data Task n = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [Task n]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n
  }

taskCost :: (Semigroup n) => Task n -> n
taskCost task = 
  sconcat (estimate task :| fmap taskCost (dependsOn task))

