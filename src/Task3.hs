{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Task3 where

import Data.Functor.Foldable
import Data.List.NonEmpty
import Data.Map as M
import Data.Maybe
import Data.Semigroup
import Data.Set
import Data.Text
import Data.Validation

type TaskState = String
type TaskTag = String

data TaskF n a = TaskF
  { title :: Text
  , description :: Text
  , dependsOn :: [a]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n
  } deriving (Functor, Foldable, Traversable)

type TaskStore n a = M.Map a (TaskF n a)

findTaskF :: (Ord a) => TaskStore n a -> a -> Maybe (TaskF n a)
findTaskF = flip M.lookup

type Task n = Fix (TaskF n)

findTask :: (Ord a) => TaskStore n a -> a -> Either (NonEmpty a) (Task n)
findTask s ref = do
  root <- maybe (Left $ ref :| []) Right (findTaskF s ref)
  embed <$> (toEither $ traverse (fromEither . findTask s) root)

taskCost :: (Semigroup n) => Task n -> n
taskCost = cata (\t -> sconcat (estimate t :| dependsOn t))
