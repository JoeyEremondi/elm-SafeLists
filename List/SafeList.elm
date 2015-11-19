module List.SafeList
  ( SafeList, null, cons, uncons, toList, fromList
  , map, map2, map3, map4, map5, unzip
  , mapl, mapr, reverseMapr, scanl
  , maximum, minimum, head, tail, last
  , member, reverse, all, any
  , sort, sortBy, sortWith
  ) where


{-|
This module provides a form of list which encode their length
in their type, using the TypeNats library. For example:

    someLength3 : List.SafeList Int (OnePlus (OnePlus (OnePlus (Zero))) )
    someLength3 = 2 `cons` 3 `cons` 4 `cons` null

List whose length doesn't match their type are forbidden.
For example:

    --Gives type mismatch
    badLength3 : SafeList Int (OnePlus (OnePlus  (Zero)) )
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
        tailCheck : SafeList Int Zero
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
@docs SafeList, null, cons

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
type SafeList a n =
  SafeList (List a)


{-| A list of length 0 -}
null : SafeList a Zero
null = SafeList []


{-| Given a new element and a list of length n, make a list of length n+1.
Has the same infix precedence as (::) -}
cons : a -> SafeList a n -> SafeList a (OnePlus n)
cons h (SafeList t) = SafeList (h :: t)

infixr 5 `cons`


{-| Split a non-empty list into a head and a tail -}
uncons : SafeList a (OnePlus n) -> (a, SafeList a n)
uncons (SafeList (h :: t)) =
  (h, SafeList t)


{-| Drop type-level information about this list -}
toList : SafeList a n -> List a
toList (SafeList l) = l


{-| Given a some SafeList, try to convert
a normal list into a SafeList of the same length -}
fromList : SafeList a n -> List b -> Maybe (SafeList b n)
fromList (SafeList lengthSpec) unSafeList =
  if (List.length lengthSpec == List.length unSafeList)
  then (Just <| SafeList unSafeList)
  else Nothing


{-
Apply a list operation to a SafeList-length list.
For internal use only: the given function MUST preserve
the length of the list.
-}
internalMap : (List a -> List b) -> SafeList a n -> SafeList b n
internalMap f (SafeList innerList) =
  SafeList (f innerList)


{-| Works as List.map -}
map : (a -> b) -> SafeList a n -> SafeList b n
map f =
  internalMap (List.map f)




{-|
Given a mapping function and an initial context value,
traverse the list left to right, applying our map function with the previously
computed context as argument.
General folds don't preserve the length of a list, so we use
this restricted form of a fold.
-}
mapl : ((a,c) -> (b,c)) -> c -> SafeList a n -> SafeList b n
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
mapr : ((a,c) -> (b,c)) -> c -> SafeList a n -> SafeList b n
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
reverseMapr : ((a,c) -> (b,c)) -> c -> SafeList a n -> SafeList b n
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
scanl : (a -> b -> b) -> b -> SafeList a n -> SafeList b (OnePlus n)
scanl f init (SafeList l) =
  SafeList <| List.scanl f init l

{-| Given a list of comparable containing at least one element,
  return its largest element -}
maximum : SafeList comparable (OnePlus n) -> comparable
maximum (SafeList l) =
  case List.maximum l of
    Nothing ->
      Debug.crash "maximum: Type-leak somewhere in List.SafeList"
    Just m ->
      m


{-| Given a list of comparable containing at least one element,
return its smallest element -}
minimum : SafeList comparable (OnePlus n) -> comparable
minimum (SafeList l) =
  case List.minimum l of
    Nothing ->
      Debug.crash "minimum: Type-leak somewhere in List.SafeList"
    Just m ->
      m

{-| SafeList way to get the first element of a list -}
head : SafeList a (OnePlus n) -> a
head (SafeList l) =
  case l of
    [] ->
      Debug.crash "head: Type-leak somewhere in List.SafeList"
    (h :: _) -> h


{-| SafeList way to get the last element of a list -}
last : SafeList a (OnePlus n) -> a
last (SafeList innerList) =
  let
    lastHelper l =
      case l of
        [] ->
          Debug.crash "head: Type-leak somewhere in List.SafeList"
        [a] ->
          a
        (_ :: t) ->
          lastHelper t
  in
    lastHelper innerList


{-| SafeList way to remove the first element of a list -}
tail : SafeList a (OnePlus n) -> SafeList a n
tail (SafeList l) =
  case l of
    [] ->
      Debug.crash "head: Type-leak somewhere in List.SafeList"
    (_ :: t) -> SafeList t




{-|
Length-preserving list functions, identical to operations on List.List
-}

member : a -> SafeList a n -> Bool
member x (SafeList l) =
  List.member x l


{-|-}
reverse : SafeList a n -> SafeList a n
reverse =
  internalMap List.reverse


{-|-}
all : (a -> Bool) -> SafeList a n -> Bool
all f (SafeList l) =
  List.all f l


{-|-}
any : (a -> Bool) -> SafeList a n -> Bool
any f (SafeList l) =
  List.any f l


{-|-}
map2 : (a -> b -> c) -> SafeList a n -> SafeList b n -> SafeList c n
map2 f (SafeList l1) (SafeList l2) =
  SafeList <| List.map2 f l1 l2


{-|-}
map3 : (a -> b -> c -> d) -> SafeList a n -> SafeList b n -> SafeList c n -> SafeList d n
map3 f (SafeList l1) (SafeList l2) (SafeList l3) =
  SafeList <| List.map3 f l1 l2 l3


{-|-}
map4 : (a -> b -> c -> d -> e) -> SafeList a n -> SafeList b n -> SafeList c n -> SafeList d n -> SafeList e n
map4 f (SafeList l1) (SafeList l2) (SafeList l3) (SafeList l4) =
  SafeList <| List.map4 f l1 l2 l3 l4


{-|-}
map5 : (a -> b -> c -> d -> e -> f) -> SafeList a n -> SafeList b n -> SafeList c n -> SafeList d n -> SafeList e n -> SafeList f n
map5 f (SafeList l1) (SafeList l2) (SafeList l3) (SafeList l4) (SafeList l5) =
  SafeList <| List.map5 f l1 l2 l3 l4 l5


{-|-}
unzip : SafeList (a,b) n -> (SafeList a n, SafeList b n)
unzip (SafeList l) =
  let
    (l1, l2) = List.unzip l
  in
    (SafeList l1, SafeList l2)


{-| Just like List.sort, but with a guarantee that length is preserved -}
sort : SafeList comparable n -> SafeList comparable n
sort = internalMap List.sort


{-| Sort based on comparable representatives -}
sortBy : (a -> comparable) -> SafeList a n -> SafeList a n
sortBy f = internalMap <| List.sortBy f


{-| Sort based on an arbitrary comparison of elements -}
sortWith : (a -> a -> Order) -> SafeList a n -> SafeList a n
sortWith f = internalMap <| List.sortWith f

--Not exported, but included to make sure our example compiles
someLength3 : SafeList Int (OnePlus (OnePlus (OnePlus (Zero))) )
someLength3 = 2 `cons` 3 `cons` 4 `cons` null

sumFirstLast =
  let
    (h1, t1) = uncons someLength3
    (_, t2) = uncons t1
    (h3, t3) = uncons t2
    tailCheck : SafeList Int Zero
    tailCheck = t3
  in
    h1 + h3

--Not exported, but included to make sure bad example doesn't compile
--badLength3 : SafeList Int (OnePlus (OnePlus  (Zero)) )
--badLength3 = 2 `cons` 3 `cons` 4 `cons` null
