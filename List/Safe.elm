import Debug

type Zero = Zero


type OnePlus t = OneAnd


type InternalFixedList a n = IFL (List a)


{-| A list with length encoded in its type,
supporting a restricted set of operations. |-}
type alias Fixed a n = InternalFixedList a n


{-| A list of length 0 |-}
null : Fixed a Zero
null = IFL []


{-| Given a new element and a list of length n, make a list of length n+1 |-}
cons : a -> Fixed a n -> Fixed a (OnePlus n)
cons h (IFL t) = IFL (h :: t)


{-| Split a non-empty list into a head and a tail |-}
uncons : Fixed a (OnePlus n) -> (a, Fixed a n)
uncons (IFL (h :: t)) = (h, IFL t)


{-| Drop type-level information about this list |-}
toList : Fixed a n -> List a
toList (IFL l) = l


{-|
Apply a list operation to a fixed-length list.
For internal use only: the given function MUST preserve
the length of the list.
|-}
internalMap : (List a -> List b) -> Fixed a n -> Fixed b n
internalMap f (IFL innerList) = IFL (f innerList)


{-| Works as List.map |-}
map : (a -> b) -> Fixed a n -> Fixed b n
map f = internalMap (List.map f)


    

{-|
Given a mapping function and an initial context value,
traverse the list left to right, applying our map function with the previously
computed context as argument.
General folds don't preserve the length of a list, so we use
this restricted form of a fold.
|-}
mapl : ((a,c) -> (b,c)) -> c -> Fixed a n -> Fixed b n
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
|-}
mapr : ((a,c) -> (b,c)) -> c -> Fixed a n -> Fixed b n
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
|-}
reverseMapr : ((a,c) -> (b,c)) -> c -> Fixed a n -> Fixed b n
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
|-}
scanl : (a -> b -> b) -> b -> Fixed a n -> Fixed b (OnePlus n)
scanl f init (IFL l) = 
  IFL <| List.scanl f init l

{-|
Maximum and minimum are now safe operations
|-}
maximum : Fixed comparable (OnePlus n) -> comparable
maximum (IFL l) = 
  case List.maximum l of
    Nothing -> Debug.crash "maximum: Type-leak somewhere in List.Fixed"
    Just m -> m


minimum : Fixed comparable (OnePlus n) -> comparable
minimum (IFL l) = 
  case List.minimum l of
    Nothing -> Debug.crash "minimum: Type-leak somewhere in List.Fixed"
    Just m -> m


{-|
Length-preserving list functions, identical to operations on List.List
|-}

member : a -> Fixed a n -> Bool
member x (IFL l) = List.member x l

{-||-}
reverse : Fixed a n -> Fixed a n
reverse = internalMap List.reverse


{-||-}
all : (a -> Bool) -> Fixed a n -> Bool
all f (IFL l) = List.all f l  


{-||-}
any : (a -> Bool) -> Fixed a n -> Bool
any f (IFL l) = List.any f l


{-||-}
map2 : (a -> b -> c) -> Fixed a n -> Fixed b n -> Fixed c n
map2 f (IFL l1) (IFL l2) = IFL <| List.map2 f l1 l2 


{-||-}
map3 : (a -> b -> c -> d) -> Fixed a n -> Fixed b n -> Fixed c n -> Fixed d n
map3 f (IFL l1) (IFL l2) (IFL l3) = IFL <| List.map3 f l1 l2 l3 


{-||-}
map4 : (a -> b -> c -> d -> e) -> Fixed a n -> Fixed b n -> Fixed c n -> Fixed d n -> Fixed e n
map4 f (IFL l1) (IFL l2) (IFL l3) (IFL l4) = IFL <| List.map4 f l1 l2 l3 l4 


{-||-}
map5 : (a -> b -> c -> d -> e -> f) -> Fixed a n -> Fixed b n -> Fixed c n -> Fixed d n -> Fixed e n -> Fixed f n
map5 f (IFL l1) (IFL l2) (IFL l3) (IFL l4) (IFL l5) = IFL <| List.map5 f l1 l2 l3 l4 l5 


{-||-}
unzip : Fixed (a,b) n -> (Fixed a n, Fixed b n)
unzip (IFL l) = 
  let
    (l1, l2) = List.unzip l
  in
    (IFL l1, IFL l2)
    

sort : Fixed comparable n -> Fixed comparable n
sort = internalMap List.sort


sortBy : (a -> comparable) -> Fixed a n -> Fixed a n
sortBy f = internalMap <| List.sortBy f


sortWith : (a -> a -> Order) -> Fixed a n -> Fixed a n
sortWith f = internalMap <| List.sortWith f
