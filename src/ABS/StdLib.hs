{-# LANGUAGE NoImplicitPrelude #-}

module ABS.StdLib
    (
     -- * ABS builtin types

     -- | The ABS standard datatypes, most coming from standard Haskell, except Fut coming from @habs-runtime@ package
     Int, Rat, Prelude.Bool (..), Unit, List, Prelude.String, -- Fut
     -- * Operations on numbers
     (Prelude.<), (Prelude.<=), (Prelude.>=), (Prelude.>), (Prelude.+), (Prelude.-), (Prelude.*), (%), Prelude.abs, pow,
     -- * Rational division. Takes any number but always returns a Rat.
     (Prelude./),
     -- * Rat to Int conversion
     Prelude.truncate,
     -- * Boolean Operations 
     (Prelude.||), (Prelude.&&), (Prelude.==), Prelude.not,
     -- * Built-in Pairs and Triples and their functions
     Pair, Prelude.fst, Prelude.snd, Triple, fstT, sndT, trd,
     -- * Maybe, Either datatypes and their functions
     Prelude.Maybe (..), fromJust, isJust,
     Prelude.Either (..), left, right, isLeft, isRight,
     -- * Functions for "List" datastructures
     list, Prelude.tail, Prelude.head, length, isEmpty, nth, concatenate, appendright, without, Prelude.repeat, Prelude.reverse, copy,
     -- * The ABS Map datatype and its functions
     M.Map, map, _emptyMap, put, insert, lookupUnsafe, lookupMaybe, lookupDefault, removeKey, keys, values,
     -- * The ABS Set datatype and its functions
     S.Set, set, _emptySet, emptySet, S.size, contains, S.union, S.intersection, S.difference, insertElement, remove, take,
     -- * Printing to Strings and to standard-output
     toString, intToString, substr, strlen,
     -- * Lifting ABS pure code to ABS object layer

     -- | Haskell is pure by default. These are necessary functions for lifting pure ABS expressions (of the functional core) to the ABS' object layer (monadic statements).

     -- ** Haskell's return 

     -- | is an expression taking a pure value and lifting it to the monadic world.
     Prelude.return,
    ) where

import qualified Prelude as Prelude
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)
import Data.List (length)
import qualified Data.Ratio
import Data.Either (isLeft, isRight)

-- | Modulo operation. Takes two 'Rat's and returns an integer. 
--
-- Truncated towards 0, so it is Haskell's 'rem'.
{-# INLINE (%) #-}
(%) :: Rat -> Rat -> Int
x % y = let res = x Prelude./ y
        in Data.Ratio.numerator res `Prelude.rem` Data.Ratio.denominator res

-- | Raising a number to a non-negative integer power
--
-- Note: deviation, abstools defines pow also for negative integral powers, but then
-- the result always will be a Rat: int/rat conversion issue
-- 
-- Another way is to always return a Rat with (^^) and then to explicitly truncate if you want Int.
{-# INLINE pow #-}
pow :: Prelude.Num a => a -> Int -> a
pow = (Prelude.^)

type Unit = ()

type Int = Prelude.Int

type Bool = Prelude.Bool

type Rat = Data.Ratio.Ratio Prelude.Int

-------- LISTS--------------
----------------------------

-- | An ABS synonym to underneath Haskell lists. It is the same as defining in ABS:
--
-- > data List a = Nil | Cons a (List a)
type List = [] 

-- | Returns the element of the list positioned at the given index.
{-# INLINE nth #-}
nth :: List a -> Int -> a
nth = (Prelude.!!)

-- | Checks if the list is empty.
{-# INLINE isEmpty #-}
isEmpty :: List a -> Bool
isEmpty = Prelude.null

-- | Replicating an element 'n' times, forming a list of length n.
{-# INLINE copy #-}
copy :: a -> Int -> List a
copy = Prelude.flip Prelude.replicate

-- | Removes all occurences of an element from a list
{-# INLINE without #-}
without :: Prelude.Eq t => [t] -> t -> [t]
without [] _ = []
without (x:xs) a | x Prelude.== a = without xs a 
                 | Prelude.otherwise = x : without xs a

{-# INLINE concatenate #-}
concatenate :: [a] -> [a] -> [a]
concatenate = (Prelude.++)

{-# INLINE appendright #-}
appendright :: [a] -> a -> [a]
appendright l p = l Prelude.++ [p]

-- | dummy function for ABS n-ary constructors
{-# INLINE list #-}
list :: [a] -> [a]
list = Prelude.id

-------- MAPS---------------
----------------------------

{-# INLINE put #-}
put :: Prelude.Ord k => M.Map k v -> k -> v -> M.Map k v
put m k v = M.insert k v m

{-# INLINE insert #-}
insert :: Prelude.Ord k => M.Map k v -> (k,v) -> M.Map k v
insert m (k,v) = M.insert k v m

{-# INLINE lookupUnsafe #-}
lookupUnsafe :: Prelude.Ord k => M.Map k v -> k -> v
lookupUnsafe m k = m M.! k

{-# INLINE lookupMaybe #-}
lookupMaybe :: Prelude.Ord k => M.Map k v -> k -> Prelude.Maybe v
lookupMaybe = Prelude.flip M.lookup

-- | Returns the value associated with key 'k' in map 'ms', or the value 'd'
-- if 'k' has no entry in 'ms'.
{-# INLINE lookupDefault #-}
lookupDefault :: Prelude.Ord k => M.Map k a -> k -> a -> a
lookupDefault ms k d = M.findWithDefault d k ms

{-# INLINE removeKey #-}
removeKey :: Prelude.Ord k => M.Map k v -> k -> M.Map k v
removeKey = Prelude.flip M.delete

-- | Constructing maps from an association list.
{-# INLINE map #-}
map :: Prelude.Ord k => List (Pair k a) -> M.Map k a
map = M.fromList

{-# INLINE values #-}
values :: M.Map k a -> (List a)
values = M.elems

-- | Constructor of empty Maps, 'EmptyMap' in ABS
{-# INLINE _emptyMap #-}
_emptyMap :: M.Map k a
_emptyMap = M.empty


{-# INLINE keys #-}
keys :: M.Map k a -> S.Set k
keys = M.keysSet

-------- SETS---------------
----------------------------

{-# INLINE set #-}
set :: Prelude.Ord a => List a -> S.Set a
set = S.fromList

{-# INLINE contains #-}
contains :: Prelude.Ord a => S.Set a -> a -> Bool
contains = Prelude.flip S.member

{-# INLINE emptySet #-}
emptySet :: S.Set a -> Bool
emptySet = S.null

{-# INLINE _emptySet #-}
_emptySet :: S.Set a
_emptySet = S.empty

{-# INLINE insertElement #-}
insertElement :: Prelude.Ord a => S.Set a -> a -> S.Set a
insertElement = Prelude.flip S.insert

{-# INLINE remove #-}
remove :: Prelude.Ord a => S.Set a -> a -> S.Set a
remove = Prelude.flip S.delete

-- | Returns one (arbitrary) element from a set.
-- To iterate over a set, take one element and remove it from the set.
-- Repeat until set is empty.
{-# INLINE take #-}
take :: Prelude.Ord a => S.Set a -> a
take = S.findMin

-------- TUPLES-------------
----------------------------

type Pair a b = (a,b)

type Triple a b c = (a,b,c)
fstT :: Triple a b c -> a
fstT (a,_,_) = a
sndT :: Triple a b c -> b
sndT (_,b,_) = b
trd :: Triple a b c -> c
trd (_,_,c) = c

-- | Deconstructs _unsafely_ the left part of an Either
left :: Prelude.Either a b -> a
left (Prelude.Left a ) = a
left _ = Prelude.error "not a left-Either"

-- | Deconstructs _unsafely_  the right part of an Either
right :: Prelude.Either a b -> b
right (Prelude.Right a) = a
right _ = Prelude.error "not a right-Either"

-------- STRINGS------------
----------------------------


-- these two are better situated as statements in the habs-runtime: ABS.Runtime haskell library
-- println :: ContT () Prelude.IO Prelude.String -> ContT () Prelude.IO ()
-- println act = act Prelude.>>= \ s -> liftIO (Prelude.putStrLn s)
-- readln :: ContT () Prelude.IO Prelude.String
-- readln = liftIO Prelude.getLine

{-# INLINE toString #-}
toString :: Prelude.Show a => a -> Prelude.String
toString = Prelude.show

-- | Returns a string with the base-10 textual representation of 'n'.
-- Note: Will work the same as toString. Just a carry-over from the other frontend.
{-# INLINE intToString #-}
intToString :: Int -> Prelude.String
intToString = Prelude.show

-- | Returns a substring of string str of the given length starting from start (inclusive)
-- Where the first character has index 0
-- 
-- Example:
--    substr("abcde",1,3) => "bcd"
{-# INLINE substr #-}
substr :: Prelude.String -> Int -> Int -> Prelude.String
substr str d len = Prelude.take len (Prelude.drop d str)

{-# INLINE strlen #-}
strlen :: Prelude.String -> Int
strlen = Prelude.length
