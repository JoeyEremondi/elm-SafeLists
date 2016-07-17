module List.Safe exposing
  ( Safe, null, cons, uncons, toList, fromList
  , map, map2, map3, map4, map5, unzip
  , mapl, mapr, reverseMapr, scanl
  , maximum, minimum, head, tail, last
  , member, reverse, all, any
  , sort, sortBy, sortWith
  )


{-|
This module provides a form of list which encode their length
in their type, using the TypeNats library. For example:

    someLength3 : List.Safe Int (OnePlus (OnePlus (OnePlus (Zero))) )
    someLength3 = 2 `cons` 3 `cons` 4 `cons` null

List whose length doesn't match their type are forbidden.
For example:

    --Gives type mismatch
    badLength3 : Safe Int (OnePlus (OnePlus  (Zero)) )
    badLength3 = 2 `cons` 3 `cons` 4 `cons` null

Because Elm doesn't have GADTs or DataKinds, it's impossible
to pattern match on SafeLists. Instead, we provide an "uncons"
function, which splits a list which we know to be non-empty
into its head and tail.

For example:

    sumFirstLast =
      let
        (h1, t1) = uncons someLength3
        (_, t2) = uncons t1
        (h3, t3) = uncons t2
        tailCheck : Safe Int Zero
        tailCheck = t3
      in
        h1 + h3

This notation is awkward, so it's assumed this library
will be used on small lists only.


As well, many functions on List.List have been ported over,
though filter, foldl and foldr are notably absent, as they
do not preserve the length of the list.

Because Elm is not a higher-kinded language, it is not generally
possible to use this library when you don't know your list size in advance,
unless only generic functions like `map` are used.

# Our main type and pseudo-constructors
@docs Safe, null, cons

# In lieu of pattern matching:
@docs uncons, toList, fromList

# The usual map functions
@docs map, map2, map3, map4, map5, unzip

# Length-preserving fold-like functions
@docs mapl, mapr, reverseMapr, scanl

# Functions for non-empty lists
@docs maximum, minimum, head, tail, last

# Utility functions
@docs  member, reverse, all, any

#Length-preserving sorting functions
@docs sort, sortBy, sortWith
-}

import Debug
import TypeNat exposing (..)


{-| A list with length encoded in its type,
supporting a restricted set of operations. -}
type Safe a n
  = SafeList (List a)


{-| A list of length 0 -}
null : Safe a Zero
null = SafeList []


{-| Given a new element and a list of length n, make a list of length n+1.
Has the same infix precedence as (::) -}
cons : a -> Safe a n -> Safe a (OnePlus n)
cons h (SafeList t) = SafeList (h :: t)

infixr 5 `cons`


{-| Split a non-empty list into a head and a tail -}
uncons : Safe a (OnePlus n) -> (a, Safe a n)
uncons l =
  case l of
    (SafeList (h :: t)) ->
      (h, SafeList t)

    (SafeList []) ->
      Debug.crash "this should never happen"


{-| Drop type-level information about this list -}
toList : Safe a n -> List a
toList (SafeList l) = l


{-| Given a some SafeList, try to convert
a normal list into a SafeList of the same length -}
fromList : Safe a n -> List b -> Maybe (Safe b n)
fromList (SafeList lengthSpec) unsafeList =
  if (List.length lengthSpec == List.length unsafeList)
  then (Just <| SafeList unsafeList)
  else Nothing


{-
Apply a list operation to a Safe-length list.
For internal use only: the given function MUST preserve
the length of the list.
-}
internalMap : (List a -> List b) -> Safe a n -> Safe b n
internalMap f (SafeList innerList) =
  SafeList (f innerList)


{-| Works as List.map -}
map : (a -> b) -> Safe a n -> Safe b n
map f =
  internalMap (List.map f)




{-|
Given a mapping function and an initial context value,
traverse the list left to right, applying our map function with the previously
computed context as argument.
General folds don't preserve the length of a list, so we use
this restricted form of a fold.
-}
mapl : ((a,c) -> (b,c)) -> c -> Safe a n -> Safe b n
mapl f initialContext =
  let
    foldFn elem (restOfList, context) =
      let
        (newElem, newContext) = f (elem, context)
      in
        (newElem :: restOfList, newContext)
  in
    internalMap (fst << List.foldl foldFn ([], initialContext))



{-|
Given a mapping function and an initial context value,
traverse the list right to left, applying our map function with the previously
computed context as argument.
General folds don't preserve the length of a list, so we use
this restricted form of a fold.
-}
mapr : ((a,c) -> (b,c)) -> c -> Safe a n -> Safe b n
mapr f initialContext =
  let
    foldFn elem (restOfList, context) =
      let
        (newElem, newContext) = f (elem, context)
      in
        (restOfList ++ [newElem], newContext)
  in
    internalMap (fst << List.foldr foldFn ([], initialContext))


{-|
Like mapr, but the resulting elements are in reverse order.
Generally faster than mapr, use this when order doesn't matter.
-}
reverseMapr : ((a,c) -> (b,c)) -> c -> Safe a n -> Safe b n
reverseMapr f initialContext =
  let
    foldFn elem (restOfList, context) =
      let
        (newElem, newContext) = f (elem, context)
      in
        (newElem :: restOfList, newContext)
  in
    internalMap (fst << List.foldr foldFn ([], initialContext))


{-|
Just like List.scanl, except we now have a guarantee that we increase the list lenght
by exactly one, since we always put our initial value in the list.
-}
scanl : (a -> b -> b) -> b -> Safe a n -> Safe b (OnePlus n)
scanl f init (SafeList l) =
  SafeList <| List.scanl f init l

{-| Given a list of comparable containing at least one element,
  return its largest element -}
maximum : Safe comparable (OnePlus n) -> comparable
maximum (SafeList l) =
  case List.maximum l of
    Nothing ->
      Debug.crash "maximum: Type-leak somewhere in List.Safe"
    Just m ->
      m


{-| Given a list of comparable containing at least one element,
return its smallest element -}
minimum : Safe comparable (OnePlus n) -> comparable
minimum (SafeList l) =
  case List.minimum l of
    Nothing ->
      Debug.crash "minimum: Type-leak somewhere in List.Safe"
    Just m ->
      m

{-| Safe way to get the first element of a list -}
head : Safe a (OnePlus n) -> a
head (SafeList l) =
  case l of
    [] ->
      Debug.crash "head: Type-leak somewhere in List.safe"
    (h :: _) -> h


{-| Safe way to get the last element of a list -}
last : Safe a (OnePlus n) -> a
last (SafeList innerList) =
  let
    lastHelper l =
      case l of
        [] ->
          Debug.crash "head: Type-leak somewhere in List.safe"
        [a] ->
          a
        (_ :: t) ->
          lastHelper t
  in
    lastHelper innerList


{-| Safe way to remove the first element of a list -}
tail : Safe a (OnePlus n) -> Safe a n
tail (SafeList l) =
  case l of
    [] ->
      Debug.crash "head: Type-leak somewhere in List.safe"
    (_ :: t) -> SafeList t




{-|
Length-preserving list functions, identical to operations on List.List
-}

member : a -> Safe a n -> Bool
member x (SafeList l) =
  List.member x l


{-|-}
reverse : Safe a n -> Safe a n
reverse =
  internalMap List.reverse


{-|-}
all : (a -> Bool) -> Safe a n -> Bool
all f (SafeList l) =
  List.all f l


{-|-}
any : (a -> Bool) -> Safe a n -> Bool
any f (SafeList l) =
  List.any f l


{-|-}
map2 : (a -> b -> c) -> Safe a n -> Safe b n -> Safe c n
map2 f (SafeList l1) (SafeList l2) =
  SafeList <| List.map2 f l1 l2


{-|-}
map3 : (a -> b -> c -> d) -> Safe a n -> Safe b n -> Safe c n -> Safe d n
map3 f (SafeList l1) (SafeList l2) (SafeList l3) =
  SafeList <| List.map3 f l1 l2 l3


{-|-}
map4 : (a -> b -> c -> d -> e) -> Safe a n -> Safe b n -> Safe c n -> Safe d n -> Safe e n
map4 f (SafeList l1) (SafeList l2) (SafeList l3) (SafeList l4) =
  SafeList <| List.map4 f l1 l2 l3 l4


{-|-}
map5 : (a -> b -> c -> d -> e -> f) -> Safe a n -> Safe b n -> Safe c n -> Safe d n -> Safe e n -> Safe f n
map5 f (SafeList l1) (SafeList l2) (SafeList l3) (SafeList l4) (SafeList l5) =
  SafeList <| List.map5 f l1 l2 l3 l4 l5


{-|-}
unzip : Safe (a,b) n -> (Safe a n, Safe b n)
unzip (SafeList l) =
  let
    (l1, l2) = List.unzip l
  in
    (SafeList l1, SafeList l2)


{-| Just like List.sort, but with a guarantee that length is preserved -}
sort : Safe comparable n -> Safe comparable n
sort = internalMap List.sort


{-| Sort based on comparable representatives -}
sortBy : (a -> comparable) -> Safe a n -> Safe a n
sortBy f = internalMap <| List.sortBy f


{-| Sort based on an arbitrary comparison of elements -}
sortWith : (a -> a -> Order) -> Safe a n -> Safe a n
sortWith f = internalMap <| List.sortWith f

--Not exported, but included to make sure our example compiles
someLength3 : Safe Int (OnePlus (OnePlus (OnePlus (Zero))) )
someLength3 = 2 `cons` 3 `cons` 4 `cons` null

sumFirstLast =
  let
    (h1, t1) = uncons someLength3
    (_, t2) = uncons t1
    (h3, t3) = uncons t2
    tailCheck : Safe Int Zero
    tailCheck = t3
  in
    h1 + h3

--Not exported, but included to make sure bad example doesn't compile
--badLength3 : Safe Int (OnePlus (OnePlus  (Zero)) )
--badLength3 = 2 `cons` 3 `cons` 4 `cons` null
