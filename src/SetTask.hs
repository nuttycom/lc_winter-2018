{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module SetTask where

import Control.Lens
import Control.Comonad.Trans.Env
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.List.NonEmpty
import Data.Map
import Data.Semigroup
import Data.Set as S
import Data.Text (Text)

type TaskState = String
type TaskTag = String

data TaskF n a = TaskF
  { _title :: Text
  , _description :: Text
  , _dependsOn :: Set a
  , _state :: TaskState
  , _tags :: Set TaskTag
  , _estimate :: n 
  } 
makeFieldsNoPrefix ''TaskF

type TaskStore n a = Map a (TaskF n a)

deref :: (Ord a) => TaskStore n a -> a -> Maybe (TaskF n a)
deref = undefined

newtype RefTask r n a = RefTask { unRefTask :: EnvT r (TaskF n) a }

type Task r n = Fix (RefTask r n)

instance (Eq r) => Eq (RefTask r n a) where
  t == t' = (ask . unRefTask $ t) == (ask . unRefTask $ t')

instance (Ord r) => Ord (RefTask r n a) where
  compare t t' = compare (ask . unRefTask $ t) (ask . unRefTask $ t')

instance (Eq r) => Eq1 (RefTask r n) where
  liftEq f t t' = 
    let (r, a) = (runEnvT . unRefTask $ t)
        (s, b) = (runEnvT . unRefTask $ t')
    in  r == s && and (f <$> S.toList (view dependsOn a) <*> S.toList (view dependsOn b))

instance (Ord r) => Ord1 (RefTask r n) where
  liftCompare f t t' = 
    let (r, a) = (runEnvT . unRefTask $ t)
        (s, b) = (runEnvT . unRefTask $ t')
    in  compare r s <> mconcat (f <$> S.toList (view dependsOn a) <*> S.toList (view dependsOn b))

getTask :: (Ord r) => TaskStore r n -> r -> Either (NonEmpty r) (Task r n)
getTask = undefined

taskCost :: (HasDependsOn a (Set a), HasEstimate a n, Ord a, Semigroup n) => a -> n
taskCost task = 
  sconcat $ view estimate task :| (fmap taskCost . S.toList $ view dependsOn task)

instance HasEstimate (Task r n) n where
  estimate = lens 
    (view estimate . lowerEnvT . unRefTask . unfix)
    (\t n -> 
      let (r, tf) = (runEnvT . unRefTask $ unfix t)
      in  Fix . RefTask $ EnvT r (set estimate n tf) )

transitiveDeps :: (HasDependsOn a (Set a), Ord a) => a -> Set a
transitiveDeps t = 
  let deps = view dependsOn t 
  in  deps <> foldMap transitiveDeps deps

instance HasDependsOn (Task r n) (Set (Task r n)) where
  dependsOn = lens 
    (view dependsOn . lowerEnvT . unRefTask . unfix)
    (\t ts ->
      let (r, tf) = (runEnvT . unRefTask $ unfix t)
      in  Fix . RefTask $ EnvT r (set dependsOn ts tf) )

instance HasEstimate (RefTask r n a) n where
  estimate = lens 
    (view estimate . lowerEnvT . unRefTask)
    (\t n -> 
      let (r, tf) = (runEnvT . unRefTask $ t)
      in  RefTask $ EnvT r (set estimate n tf) )

instance HasDependsOn (RefTask r n a) (Set a) where
  dependsOn = lens 
    (view dependsOn . lowerEnvT . unRefTask)
    (\t ts ->
      let (r, tf) = (runEnvT . unRefTask $ t)
      in  RefTask $ EnvT r (set dependsOn ts tf) )

-- :(
-- taskCost' :: Semigroup n => Task r n -> n
-- taskCost' = cata (\fa -> sconcat $ view estimate fa :| S.toList (view dependsOn fa)) 

-- identity vs equality
-- generalization adds options for the caller, decreases options for the implementer
-- Pay attention to the semantics of the library types you use
-- types are sets of operations
-- functions should demand the minimum structure needed to do their job
-- ideally, it should be evident from the call site what operations a function can perform.
--
