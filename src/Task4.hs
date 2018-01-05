{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Task4 where

import Control.Lens
import Control.Monad.State
import Data.Foldable as F
import Data.Functor.Foldable
import Data.List as L
import Data.List.NonEmpty
import Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import Data.Set as S
import Data.Text (Text)
import Data.Validation
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands
import Debug.Trace

data TaskType
  = Task
  | Bug
  deriving (Eq, Ord)

data TaskState 
  = Created TaskType
  | Completed
  deriving (Eq, Ord)

type TaskTag = String

data TaskF n a = TaskF
  { title :: Text
  , description :: Text
  , dependsOn :: [a]
  , taskState :: TaskState
  , tags :: Set TaskTag
  , estimate :: n
  } deriving (Functor, Foldable, Traversable)
makeLenses ''TaskF

newtype TaskStore n a = TaskStore { unTaskStore :: M.Map a (TaskF n a) }

findTaskF :: (Ord a) => TaskStore n a -> a -> Maybe (TaskF n a)
findTaskF = flip M.lookup . unTaskStore

type Task n = Fix (TaskF n)

findTask :: (Ord a) => TaskStore n a -> a -> Either (NonEmpty a) (Task n)
findTask s ref = do
  root <- maybe (Left $ ref :| []) Right (findTaskF s ref)
  embed <$> (toEither $ traverse (fromEither . findTask s) root)

insertTaskF :: (Ord a)
           => TaskStore n a
           -> a 
           -> TaskF n a 
           -> Either (NonEmpty a) (TaskStore n a)
insertTaskF = undefined

transitiveDeps :: (Ord a) => TaskStore n a -> a -> [a]
transitiveDeps s a = do
  task <- F.toList $ findTaskF s a
  dep  <- dependsOn task
  dep : transitiveDeps s dep

treeSum :: (Semigroup n) => Task n -> n
treeSum = cata (\t -> sconcat (estimate t :| dependsOn t))

spanningTree :: (Ord a) => Lens' a [a] -> a -> a
spanningTree l a =
  evalState (go a) S.empty 
    where 
      go a' = do
        seen <- get
        let retained = L.filter (not . (flip S.member) seen) (view l a') 
        put (S.union seen $ S.fromList retained)
        pruned <- traverse go retained
        pure (set l pruned a')

graph :: (Ord a, Show a) => TaskStore n a -> a -> DotGraph a
graph s a = 
  let nodes = a : transitiveDeps s a
      edges' (a, tf) = (a,,()) <$> dependsOn tf
      edges = edges' =<< (\a' -> fmap (a',) . F.toList $ findTaskF s a') =<< nodes
  in  graphElemsToDot (graphParams s) ((,()) <$> nodes) (L.nub edges)

graphParams :: (Ord a) => TaskStore n a -> GraphvizParams a al el () al
graphParams s = nonClusteredParams 
  { isDirected = True
  , globalAttributes = 
      [ GraphAttrs [RankDir FromLeft] 
      , NodeAttrs  [shape DoubleCircle]
      ]
  , fmtNode = \(a, _) -> 
      let attrs t = toLabel (title t) : taskStyle (taskState t) 
      in  maybe [] attrs $ findTaskF s a 
  }

taskStyle :: TaskState -> [Attribute]
taskStyle (Created Task) = []
taskStyle (Created Bug) = [style filled, fillColor Tomato]
taskStyle Completed = [style filled, fillColor LawnGreen]

sampleTasks :: TaskStore Int Int
sampleTasks = 
  let ctd i n d = (i, TaskF n "" d (Created Task) S.empty 1)
      cbd i n d = (i, TaskF n "" d (Created Bug) S.empty 1)
      cpd i n d = (i, TaskF n "" d Completed S.empty 1)
  in  TaskStore $ M.fromList 
    [ ctd 0 "Build a\ntask tracker" [1, 2] 
    , ctd 1 "Print a\ntask graph" [3, 7] 
    , ctd 2 "Print\ntask costs" [3, 5, 7, 8]
    , ctd 3 "Query\ntasks" [4]
    , ctd 4 "Build task\nstorage" [6]
    , cpd 5 "Compute the\ncost of a task" [6]
    , cpd 6 "Create a\ntask type" [] 
    , ctd 7 "Traverse task\ndependencies" [6] 
    , cbd 8 "Fix broken\ntask costs" [] 
    ]

renderSampleTasks :: IO FilePath
renderSampleTasks = runGraphviz (graph sampleTasks 0) Svg "test.svg"
