% Type-Driven Development
% Kris Nuttycombe (@nuttycom) - January, 2018

# Resources

- Slides: [nuttycom.github.io/lc_winter-2018/tdd.html](http://nuttycom.github.io/lc_winter-2018/tdd.html)
- Sources: [github.com/nuttycom/lc_winter-2018/tdd](https://github.com/nuttycom/lc_winter-2018/tdd)

# TDD?
<div class="incremental"><div>
## Test-Driven Development

> * Write down a failing test case for a specific feature. This test will be incomplete and wrong.
> * Write code until that test passes (solving the wrong problem).
> * Iterate
</div></div>

<div class="incremental"><div>
## Type-Driven Development

> * Write down the problem as some types - both data and operations. Avoid implementation concerns.
> * Look at your type. 
>     * Does it represent all the states you need? 
>         * **Write a proof.**
>     * Does it imply some states you don't want in your program? 
>         * **Introduce types to eliminate those states (recurse.)**
</div></div>

# Chicken Sexing

<div align="center">
<img width="600" src="images/chicken_sexing.jpg"/>

> -- [anecdote via \@garybernhardt](https://twitter.com/garybernhardt/status/947260894609686529)
</div>

<div class="notes">
> In the book Incognito by David Eagelman, the author discusses the strange
> nature of chicken sexing. This is the valuable process of separating female
> and male chicks as soon as possible, because each sex has different diets and
> endgames (most males are just destroyed). The mystery is that when you look
> at the vent in the chick’s rear, some people just know which are female. It
> is impossible to articulate, so the Japanese figured out how to teach this
> inarticulable knowledge. The student would pick up a chick, examine its rear,
> and toss it into a bin. The master would then say ‘yes’ or ‘no’ based on his
> generally correct observation. After a few weeks, the student’s brain was
> trained to masterful levels.  
>

This is all to say that what I'm going to be talking about and attempting to
teach today is not something that I feel that I'm capable of reducing to a
well-articulated process. Instead, I'm going to walk through the development
of a small part of an application, describing what I see, with the hope that we can have 
a conversation about the conclusions that I draw from the types in front of
us. It's my hope that this can really be an interactive process, so please
speak up whenever a question or a comment occurs to you. 

Another reason I really like this anecdote and feel strongly that it's
applicable to sofware development is that most of the time looking at a piece
of software feels a lot like looking at a chicken's butt.

</div>

# DAGs for Task Management

<img src="./dags/tasks0.svg"/>

<div class="notes">

I generally find that my talks go best when I have something concrete to talk
about, so for today's talk I decided to build an application that, while small,
is not intended as a toy.  This is something that I've been wanting to create
for use with my own projects for quite a while.

The underlying concept is that the most important feature of a task tracker for
software development is to be able to keep track of the dependencies between
tasks.  So often I've seen in my work the situation where I go to start
implementing a feature, only to discover that the prerequisites for
implementing that feature are missing or malformed. Yet, most task trackers are
*lousy* at dealing with this situation - you start on something, realize that
you ought to implement the prerequisites, have to create new tasks for those,
mark the thing you're working on as paused, shove it into the backlog while the
prerequisite goes to the top of the stack, and so on. What I want is the
ability to capture this really common process of discovery in a tool, where all
the dependencies between tasks are tracked and managed as essential
information.  I want to be able to render the DAG of dependencies, and pick off
something at the top - something with no dependencies - to work on.

Furthermore, I want to be able to add up the estimates for the dependencies
of a task to get an estimate for the overall task. 

</div>

# A simple task type

<img src="./dags/tasks1.svg"/>

# A simple task type

~~~haskell
data Task = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [Task]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: Int
  }
~~~

<div class="notes">

A tree-structured data type, pretty simple. This is of course also capable of
representing a DAG, or even a graph with cycles. 

Now, I know that you've come to a talk about type-driven development, and some
of you might be thinking that I'm about to launch into some clever attempt at
implementing type-level proofs that my dependency graphs don't contain cycles.
That's not at all what I'm going to do. I like to write what I call "dumb,
workmanlike Haskell." There are some properties, like acyclicity, that I'm just
not interested in proving, particularly in situations like this where making
updates to the structure of a given graph in response to user input is central
to the use of the tool that I want to build. These graphs are going to be
crossing a bunch of serialization boundaries, and I'm going to need to be able
to capture lots of kinds of errors at those boundaries anyway, so the function
that I eventually write to insert a node into a graph might as well make a
trivially easy runtime check, return an error if it fails, and I can go on my
way focusing on the "business problem" rather than getting tangled up in
implementing something in the cleverest way possible where the ultimate gain is
marginal at best.

A lot of Haskell is like this - we have lots of options of how to implement
things, and there's a lot of space for proving our programs correct at compile
time.  I think this is awesome - this is why I use Haskell - but we also have
the option to write really safe code that doesn't prove every single property.

This is an acceptable first cut. We can start to implement some interesting
functions with this much structure to work from.

Some additional points:
* I've left TaskState and TaskTag undefined for the moment because I don't know
  what they should be yet. But they're distinct names.

</div>

# Task Cost

<img src="./dags/tasks2.svg"/>

# Task Cost

~~~haskell
data Task = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [Task]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: Int
  }
~~~

~~~haskell
taskCost :: Task -> Int
~~~

<div class="notes">

One of the major benefits I'm looking to gain from using a DAG to represent
tasks is that I want to be able to determine the total estimate cost of a task
by aggregating the costs of its dependencies. So, the first thing I want to do
is write down the type of this operation, given what I've got so far.

</div>

# Task Cost

~~~haskell
data Task = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [Task]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: Int
  }
~~~

~~~haskell
taskCost :: Task -> Int
taskCost task = 
  estimate task + sum (fmap taskCost (dependsOn task))
~~~

<div class="notes">
Here's a proof that this type is adequate to capture the operation I want. 
However, it's a little bit too big. Here's another program that satisfies
this type:
</div>

<div class="incremental">
~~~haskell
taskCost :: Task -> Int
taskCost task = length $ dependsOn task
~~~
</div>

# Prune the space of possible implementations

~~~haskell
data Task n = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [Task n]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n
  }
~~~

~~~haskell
taskCost :: (Monoid n) => Task n -> n
taskCost task = 
  estimate task <> foldMap taskCost (dependsOn task)
~~~

<div class="notes">
This is an improvement, for sure, but the type is *still* too big. The
following implementation is pathological, but it illustrates an
important point. Anywhere that a type can imply some sort of default
value, you have to worry that you might be getting the default rather
than a real value. So, let's prune off that extra bit of state space.
</div>

<div class="incremental">
~~~haskell
taskCost :: (Monoid n) => Task -> n
taskCost = const mempty
~~~
</div>

# Get rid of every operation you don't need

~~~haskell
data Task n = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [Task n]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n
  }
~~~

~~~haskell
taskCost :: (Semigroup n) => Task n -> n
taskCost task = 
  sconcat (estimate task :| fmap taskCost (dependsOn task))
~~~

<div class="notes">

This type is pretty representative of the operation we want to perform.
It's a good business-domain level representation, and we can stop here
for now.

</div>

# Storage

<img src="./dags/tasks3.svg"/>

# Storage

~~~haskell
data Task n = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [Task n]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n 
  } 
~~~

~~~haskell
newtype TaskStore n = TaskStore { unTaskStore :: Map TaskRef (TaskF n) }

findTask :: TaskStore n 
         -> TaskRef 
         -> Maybe (Task n)
  
~~~

<div class="notes">

Here's the operation that I want to be able perform. I want some storage
for this thing!  A number of people to be able to use this task tracking tool
concurrently in the process of managing a project.

The problem right now is that if I have some collection of task trees, and I
want to update a particular task in some fashion, I want to be able to find,
and later refer, to that task by some sort of handle that is independent of the
data of the task itself.

Adding this operation, however, makes me realize something. The task type 
we have here is a little awkward for using in this context - a task value
carries around with it all of its dependencies. That's going to make things
rough when we have tasks in storage - if I have a reference to both a parent
and its child, and I update the value at the child reference, nothing happens 
to the parent. It retains its old reference. 

This ultimately means that I need these references to be stable over time, 
independent of structural equality of tasks. 

</div>

# Storage

~~~haskell
data Task n = Task 
  { title :: Text
  , description :: Text
  , dependsOn :: [TaskRef]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n 
  } 
~~~

~~~haskell
newtype TaskStore n = TaskStore { unTaskStore :: Map TaskRef (TaskF n) }

findTask :: TaskStore n 
         -> TaskRef 
         -> Maybe (Task n)
  
~~~

<div class="notes">

The first thing to do here is to remove the direct recursion from the structure
of the task. Instead, all of our relationships between tasks will be
intermediated by the data storage system.

</div>

# Storage

~~~haskell
data TaskF n a = TaskF 
  { title :: Text
  , description :: Text
  , dependsOn :: [a]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n 
  } 
~~~

~~~haskell
newtype TaskStore n a = TaskStore { unTaskStore :: Map a (TaskF n a) }

findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
  
~~~

<div class="notes">

If I want to write a proof that the `find` function is adequate, I'll need
to work out the details of TaskRef, in order to provide an ordering. 
However, I don't really want to bother with that right now. So let's just
defer that decision a bit.

</div>

# Storage

~~~haskell
data TaskF n a = TaskF 
  { title :: Text
  , description :: Text
  , dependsOn :: [a]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n 
  } 
~~~

~~~haskell
newtype TaskStore n a = TaskStore { unTaskStore :: Map a (TaskF n a) }

findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
findTaskF = flip lookup . unTaskStore
~~~

# Oops.

~~~
/Users/nuttycom/personal/lc_winter-2018-tdd/src/Task2.hs:29:44: error:
    • Couldn't match type ‘a’ with ‘TaskF n a0’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          taskCost :: forall n a. Semigroup n => TaskF n a -> n
        at src/Task2.hs:27:1-43
      Expected type: [TaskF n a0]
        Actual type: [a]
    • In the second argument of ‘fmap’, namely ‘(dependsOn task)’
      In the second argument of ‘(:|)’, namely
        ‘fmap taskCost (dependsOn task)’
      In the first argument of ‘sconcat’, namely
        ‘(estimate task :| fmap taskCost (dependsOn task))’
    • Relevant bindings include
        task :: TaskF n a (bound at src/Task2.hs:28:10)
        taskCost :: TaskF n a -> n (bound at src/Task2.hs:28:1)
   |
29 |   sconcat (estimate task :| fmap taskCost (dependsOn task))
   |                                            ^^^^^^^^^^^^^^
~~~

<div class="notes">

The change that we've made to our task type means that both the signature of
our taskCost function, and its proof of being valid, are no longer compiling!
So, we have to fix that.

</div>

# Oops.

<img src="./dags/tasks4.svg"/>

# Fixing taskCost

~~~haskell
taskCost :: (Semigroup n) => Task n -> n
taskCost task = 
  sconcat (estimate task :| fmap taskCost (dependsOn task))
~~~

<div class="incremental">
~~~haskell
findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
~~~
</div>

<div class="incremental">
~~~haskell
taskCost :: (Semigroup n, Ord a) 
         => TaskStore n a 
         -> TaskF n a 
         -> n
~~~
</div>

# Fixing taskCost

~~~haskell
taskCost :: (Semigroup n) => Task n -> n
taskCost task = 
  sconcat (estimate task :| fmap taskCost (dependsOn task))
~~~

~~~haskell
findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
~~~

~~~haskell
taskCost :: (Semigroup n, Ord a) 
         => TaskStore n a 
         -> TaskF n a 
         -> n
taskCost s task = 
  let deps = catMaybes . fmap (findTaskF s) $ dependsOn task
  in  sconcat (estimate task :| fmap (taskCost s) deps)
~~~

<div class="notes">

Now, maybe it's just me, but this strikes me as kind of gross. Our taskCost
function suddenly got a lot more complicated - and any such function that's
recursively traversing this tree is going to be similarly complicated. We're
mixing concerns, if we decide to change our TaskStore we're going to break 
things again - we really would like to recover some of our former utility.

What's more, there's something really important that got glossed over here.
What happens if we run into an invalid reference for some reason? It ends
up just being silently ignored by `catMaybes` there. That's awful, if there
are invalid references hanging around I want to know about them. 

</div>

# Fixing taskCost

~~~haskell
data TaskF n a = TaskF
  { title :: Text
  , description :: Text
  , dependsOn :: [a]
  , state :: TaskState
  , tags :: Set TaskTag
  , estimate :: n
  } deriving Functor
~~~

<div class="incremental">
~~~haskell
-- from Data.Functor.Foldable 
newtype Fix (f :: * -> *) = Fix { unfix :: f (Fix f) }
~~~
</div>

<div class="incremental">
~~~haskell
type Task n = Fix (TaskF n)
~~~
</div>

<div class="incremental">
~~~haskell
findTask :: (Ord a) 
         => TaskStore n a 
         -> a 
         -> Either (NonEmpty a) (Task n)
~~~
</div>

# Fixing taskCost

~~~haskell
type Task n = Fix (TaskF n)

findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
~~~

<div class="incremental">
~~~haskell



findTask :: (Ord a) 
         => TaskStore n a 
         -> a 
         -> Either (NonEmpty a) (Task n)
findTask store ref = 
  
  
  
~~~
</div>

# Fixing taskCost

~~~haskell
type Task n = Fix (TaskF n)

findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
~~~

~~~haskell
import Data.Semigroup (:|)


findTask :: (Ord a) 
         => TaskStore n a 
         -> a 
         -> Either (NonEmpty a) (Task n)
findTask s ref = do
  root  <- maybe (Left $ ref :| []) Right (findTaskF store ref)
  
  
~~~

# Fixing taskCost

~~~haskell
type Task n = Fix (TaskF n)

findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
~~~

~~~haskell
import Data.Semigroup (:|)
import Data.Validation as V

findTask :: (Ord a) 
         => TaskStore n a 
         -> a 
         -> Either (NonEmpty a) (Task n)
findTask s ref = do
  root  <- maybe (Left $ ref :| []) Right (findTaskF s ref)
  root' <- V.toEither $ traverse (V.fromEither . findTask s) root
  
~~~

# Fixing taskCost

~~~haskell
type Task n = Fix (TaskF n)

findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
~~~

~~~haskell
import Data.Semigroup (:|)
import Data.Validation as V

findTask :: (Ord a) 
         => TaskStore n a 
         -> a 
         -> Either (NonEmpty a) (Task n)
findTask s ref = do
  root  <- maybe (Left $ ref :| []) Right (findTaskF s ref)
  root' <- V.toEither $ traverse (V.fromEither . findTask s) root
  pure $ embed root
~~~

# Fixing taskCost

~~~haskell
type Task n = Fix (TaskF n)

findTaskF :: (Ord a) 
          => TaskStore n a 
          -> a 
          -> Maybe (TaskF n a)
~~~

~~~haskell
taskCost :: (Semigroup n) => Task n -> n
taskCost = cata (\t -> sconcat (estimate t :| dependsOn t))
~~~

# Graphing!

<img src="./dags/tasks5.svg"/>

# Graphing!

~~~haskell
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
~~~

# Graphing!

<img src="./images/graphElemsToDot.png" width="100%"/>

# Graphing!

<img src="./images/GraphvizParams.png" height="600"/>

# To Be Embellished

<img src="./dags/tasks6.svg"/>

# The Vampire Policy

<div align="center">
<img width="600" src="./images/bela-lugosi.jpg"/>

> "Bug fixing strategy: forbid yourself to fix the bug. Instead, render 
> the bug impossible by construction." 
> --[Paul Phillips](https://twitter.com/extempore2/status/417366903209091073)
</div>

# Know the size of your state space

<div class="incremental"><div>
How many different values can this function possibly return?

~~~haskell
f :: () -> Bool
~~~
</div></div>

# Know the size of your state space

How many different values can this function possibly return?

~~~haskell
f :: () -> Bool -- 2 possible values
~~~

<div class="incremental">
~~~haskell
g :: () -> Int32 
~~~
</div>

# Know the size of your state space

How many different values can this function possibly return?

~~~haskell
f :: () -> Bool -- 2 possible values
~~~

~~~haskell
g :: () -> Int32 -- 2^32 possible values
~~~

<div class="incremental">
~~~haskell
k :: () -> Either Bool Int32
~~~
</div>

# Know the size of your state space

How many different values can this function possibly return?

~~~haskell
f :: () -> Bool -- 2 possible values
~~~

~~~haskell
g :: () -> Int32 -- 2^32 possible values
~~~

~~~haskell
k :: () -> Either Bool Int32 -- 2 + 2^32 possible values
~~~

<div class="incremental">
~~~haskell
h :: () -> (Bool, Int32)
~~~
</div>

# Know the size of your state space

How many different values can this function possibly return?

~~~haskell
f :: () -> Bool -- 2 possible values
~~~

~~~haskell
g :: () -> Int32 -- 2^32 possible values
~~~

~~~haskell
k :: () -> Either Bool Int32 -- 2 + 2^32 possible values
~~~

~~~haskell
h :: () -> (Bool, Int32) -- 2 * 2^32 = 2^33 possible values
~~~

<div class="incremental">
~~~haskell
h :: () -> Either Int32 Int32 -- 2^32 + 2^32 = 2^33 possible values
~~~
</div>

<div class="incremental">
~~~haskell
k :: () -> Text
~~~
</div>

# Know the size of your state space

How many different values can this function possibly return?

~~~haskell
f :: () -> Bool -- 2 possible values
~~~

~~~haskell
g :: () -> Int32 -- 2^32 possible values
~~~

~~~haskell
k :: () -> Either Bool Int32 -- 2 + 2^32 possible values
~~~

~~~haskell
h :: () -> (Bool, Int32) -- 2 * 2^32 = 2^33 possible values
~~~

~~~haskell
h :: () -> Either Int32 Int32 -- 2^32 + 2^32 = 2^33 possible values
~~~

~~~haskell
k :: () -> Text -- 😬😖😩😷
~~~

# Use smart constructors (aka parsers) wherever errors can occur

~~~haskell
insertTaskF :: (Ord a)
           => TaskStore n a
           -> a 
           -> TaskF n a 
           -> Either (NonEmpty a) (TaskStore n a)
~~~

# Final principles to code by

> - Make invalid states unrepresentable.
> - Give the minimum possible power to a function's implementer, and the maximum possible flexibility to its caller.
> - Type polymorphism reduces the number of things a function can possibly do. Use it.
> - Strings should appear in your program only where they're being show to a human being.

# Extra bonus quiz!

<div class="incremental">
~~~haskell
type T a b = forall c. (a -> b -> c) -> c

type E a b = forall c. (a -> c) -> (b -> c) -> c
~~~
</div>

<div class="incremental"><div>
How many different values can these functions possibly return?

~~~haskell
f :: () -> T Bool Int32

g :: () -> E Bool Int32
~~~
</div></div>

# Extra bonus quiz!

~~~haskell
type T a b = forall c. (a -> b -> c) -> c

type E a b = forall c. (a -> c) -> (b -> c) -> c
~~~

How many different values can these functions possibly return?

~~~haskell
f :: () -> T Bool Int32 -- 2 * 2^32 = 2^33 possible values!

g :: () -> E Bool Int32 -- 2 + 2^32 possible values!
~~~
