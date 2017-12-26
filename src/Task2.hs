module Task2 where

import Data.List.NonEmpty
import Data.Map as M
import Data.Maybe
import Data.Semigroup
import Data.Set
import Data.Text

type TaskState = String
type TaskTag = String

data TaskF n a = TaskF
  { title :: Text
  , description :: Text
  , dependsOn :: [a]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n
  }

type TaskStore n a = M.Map a (TaskF n a)

findTaskF :: (Ord a) => TaskStore n a -> a -> Maybe (TaskF n a)
findTaskF = flip M.lookup

-- taskCost :: (Semigroup n) => TaskF n a -> n
-- taskCost task = 
--   sconcat (estimate task :| fmap taskCost (dependsOn task))

taskCost :: (Semigroup n, Ord a) => TaskStore n a -> TaskF n a -> n
taskCost s t = 
  let deps = catMaybes . fmap (findTaskF s) $ dependsOn t
  in  sconcat (estimate t :| fmap (taskCost s) deps)
