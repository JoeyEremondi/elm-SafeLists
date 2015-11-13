module List.Safe
  ( Safe, null, cons, uncons, toList
  , map, map2, map3, map4, map5, unzip
  , mapl, mapr, reverseMapr, scanl
  , maximum, minimum, member, reverse, all, any
  , sort, sortBy, sortWith
  ) where


{-|
# Our main type and pseudo-constructors
@docs Safe, null, cons

# In lieu of pattern matching:
@docs uncons, toList

# The usual map functions
@docs map, map2, map3, map4, map5, unzip

# Length-preserving fold-like functions
@docs mapl, mapr, reverseMapr, scanl

# Type-safe min and max (No need for `Maybe`!)
@docs maximum, minimum

# Utility functions
@docs  member, reverse, all, any

#Length-preserving sorting functions
@docs sort, sortBy, sortWith


-}

import Debug
import TypeNat exposing (..)


{-| A list with length encoded in its type,
supporting a restricted set of operations. -}
type Safe a n =
  IFL (List a)


{-| A list of length 0 -}
null : Safe a Zero
null = IFL []


{-| Given a new element and a list of length n, make a list of length n+1 -}
cons : a -> Safe a n -> Safe a (OnePlus n)
cons h (IFL t) = IFL (h :: t)


{-| Split a non-empty list into a head and a tail -}
uncons : Safe a (OnePlus n) -> (a, Safe a n)
uncons (IFL (h :: t)) = (h, IFL t)


{-| Drop type-level information about this list -}
toList : Safe a n -> List a
toList (IFL l) = l


{-|
Apply a list operation to a Safe-length list.
For internal use only: the given function MUST preserve
the length of the list.
-}
internalMap : (List a -> List b) -> Safe a n -> Safe b n
internalMap f (IFL innerList) = IFL (f innerList)


{-| Works as List.map -}
map : (a -> b) -> Safe a n -> Safe b n
map f = internalMap (List.map f)




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
scanl f init (IFL l) =
  IFL <| List.scanl f init l

{-| Given a list of comparable containing at least one element,
  return its largest element -}
maximum : Safe comparable (OnePlus n) -> comparable
maximum (IFL l) =
  case List.maximum l of
    Nothing -> Debug.crash "maximum: Type-leak somewhere in List.Safe"
    Just m -> m


{-| Given a list of comparable containing at least one element,
return its smallest element -}
minimum : Safe comparable (OnePlus n) -> comparable
minimum (IFL l) =
  case List.minimum l of
    Nothing -> Debug.crash "minimum: Type-leak somewhere in List.Safe"
    Just m -> m


{-|
Length-preserving list functions, identical to operations on List.List
-}

member : a -> Safe a n -> Bool
member x (IFL l) = List.member x l


{-|-}
reverse : Safe a n -> Safe a n
reverse = internalMap List.reverse


{-|-}
all : (a -> Bool) -> Safe a n -> Bool
all f (IFL l) = List.all f l


{-|-}
any : (a -> Bool) -> Safe a n -> Bool
any f (IFL l) = List.any f l


{-|-}
map2 : (a -> b -> c) -> Safe a n -> Safe b n -> Safe c n
map2 f (IFL l1) (IFL l2) = IFL <| List.map2 f l1 l2


{-|-}
map3 : (a -> b -> c -> d) -> Safe a n -> Safe b n -> Safe c n -> Safe d n
map3 f (IFL l1) (IFL l2) (IFL l3) = IFL <| List.map3 f l1 l2 l3


{-|-}
map4 : (a -> b -> c -> d -> e) -> Safe a n -> Safe b n -> Safe c n -> Safe d n -> Safe e n
map4 f (IFL l1) (IFL l2) (IFL l3) (IFL l4) = IFL <| List.map4 f l1 l2 l3 l4


{-|-}
map5 : (a -> b -> c -> d -> e -> f) -> Safe a n -> Safe b n -> Safe c n -> Safe d n -> Safe e n -> Safe f n
map5 f (IFL l1) (IFL l2) (IFL l3) (IFL l4) (IFL l5) = IFL <| List.map5 f l1 l2 l3 l4 l5


{-|-}
unzip : Safe (a,b) n -> (Safe a n, Safe b n)
unzip (IFL l) =
  let
    (l1, l2) = List.unzip l
  in
    (IFL l1, IFL l2)

{-| Just like List.sort, but with a guarantee that length is preserved -}
sort : Safe comparable n -> Safe comparable n
sort = internalMap List.sort

{-| Sort based on comparable representatives -}
sortBy : (a -> comparable) -> Safe a n -> Safe a n
sortBy f = internalMap <| List.sortBy f


{-| Sort based on an arbitrary comparison of elements -}
sortWith : (a -> a -> Order) -> Safe a n -> Safe a n
sortWith f = internalMap <| List.sortWith f
