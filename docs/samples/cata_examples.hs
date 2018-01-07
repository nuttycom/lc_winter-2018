Prelude> type T a b = forall c. (a -> b -> c) -> c

<interactive>:1:22: error:
    Illegal symbol '.' in type
    Perhaps you intended to use RankNTypes or a similar language
    extension to enable explicit-forall syntax: forall <tvs>. <type>
Prelude> :set -XRankNTypes
Prelude> type T a b = forall c. (a -> b -> c) -> c
Prelude>
Prelude> type T' a b c = (a -> b -> c) - c

<interactive>:5:17: error:
    Not in scope: type constructor or class ‘-’

<interactive>:5:17: error:
    Illegal operator ‘-’ in type ‘(a -> b -> c) - c’
      Use TypeOperators to allow operators in types
Prelude> type T' a b c = (a -> b -> c) -> c
Prelude> let v = (1, 2)
Prelude> fst v + snd v
3
Prelude> :{
Prelude| let tuple :: a -> b -> T a b
Prelude|     tuple a b = \f -> f a b
Prelude| :}
Prelude> let v' = tuple 1 2
Prelude> v' (\a b -> a + b)
3
Prelude> :{
Prelude| let fst' :: T a b -> a
Prelude|     fst t = t (\a b -> a)
Prelude| :}

<interactive>:16:5: error:
    The type signature for ‘fst'’ lacks an accompanying binding
Prelude> let fst' t = t (\a b -> a)
Prelude> fst' $ tuple 1 2
1
Prelude> let sub (a, b) = a - b
Prelude> sub (3, 2)
1
Prelude> let sub' t = t (\a b -> a - b)
Prelude> sub $ tuple 3 2

<interactive>:24:7: error:
    • Couldn't match expected type ‘(a, a)’
                  with actual type ‘(Integer -> Integer -> c0) -> c0’
    • Probable cause: ‘tuple’ is applied to too few arguments
      In the second argument of ‘($)’, namely ‘tuple 3 2’
      In the expression: sub $ tuple 3 2
      In an equation for ‘it’: it = sub $ tuple 3 2
    • Relevant bindings include it :: a (bound at <interactive>:24:1)
Prelude> sub' $ tuple 3 2
1
Prelude> let add1 x = x + 1
Prelude> let addn n x = n + x
Prelude> addn 1

<interactive>:28:1: error:
    • No instance for (Show (Integer -> Integer))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
Prelude> let add1 = addn 1
Prelude> add1 3
4
Prelude>
Prelude>
Prelude> type E a b = forall c. (a -> c) -> (b -> c) -> c
Prelude> :{
Prelude| let left :: a -> E a b
Prelude|     left a = (\l r -> l a)
Prelude|     right :: b -> E a b
Prelude|     right b = (\l r -> r b)
Prelude| :}
Prelude> let addorsub e = e (\a -> a - 1) (\b -> b + 1)
Prelude> addorsub (left 1)
0
Prelude> addorsub (right 2)
3
Prelude> :t either
either :: (a -> c) -> (b -> c) -> Either a b -> c
Prelude> let addorsub' = either (\a -> a - 1) (\b -> b + 1)
Prelude> addorsub' (Left 1)
0
Prelude>

