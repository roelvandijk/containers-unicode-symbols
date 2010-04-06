{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

{-|
Module     : Data.IntSet.Unicode
Copyright  : (c) 2009–2010 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}

module Data.IntSet.Unicode
    ( (∈), (∋), (∉), (∌)
    , (∅)
    , (∪), (∩)
    , (⊆), (⊇), (⊈), (⊉)
    , (⊂), (⊃), (⊄), (⊅)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool, not )
import Data.Function ( flip )
import Data.Int      ( Int )

-- from base-unicode-symbols:
import Data.Eq.Unicode   ( (≢) )
import Data.Bool.Unicode ( (∧) )

-- from containers:
import Data.IntSet ( IntSet
                   , member, notMember
                   , empty
                   , union, intersection
                   , isSubsetOf, isProperSubsetOf
                   )


-------------------------------------------------------------------------------
-- Fixities
-------------------------------------------------------------------------------

infix  4 ∈
infix  4 ∋
infix  4 ∉
infix  4 ∌


-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

{- |
(&#x2208;) = 'member'

U+2208, ELEMENT OF
-}
(∈) ∷ Int -> IntSet → Bool
(∈) = member

{- |
(&#x220B;) = 'flip' (&#x2208;)

U+220B, CONTAINS AS MEMBER
-}
(∋) ∷ IntSet → Int → Bool
(∋) = flip (∈)

{- |
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Int -> IntSet → Bool
(∉) = notMember

{- |
(&#x220C;) = 'flip' (&#x2209;)

U+220C, DOES NOT CONTAIN AS MEMBER
-}
(∌) ∷ IntSet → Int → Bool
(∌) = flip (∉)

{- |
(&#x2205;) = 'empty'

U+2205, EMPTY SET
-}
(∅) ∷ IntSet
(∅) = empty

{- |
(&#x222A;) = 'union'

U+222A, UNION
-}
(∪) ∷ IntSet → IntSet → IntSet
(∪) = union

{- |
(&#x2229;) = 'intersection'

U+2229, INTERSECTION
-}
(∩) ∷ IntSet → IntSet → IntSet
(∩) = intersection

{- |
(&#x2286;) = 'isSubsetOf'

U+2286, SUBSET OF OR EQUAL TO
-}
(⊆) ∷ IntSet → IntSet → Bool
(⊆) = isSubsetOf

{- |
(&#x2287;) = 'flip' (&#x2286;)

U+2287, SUPERSET OF OR EQUAL TO
-}
(⊇) ∷ IntSet → IntSet → Bool
(⊇) = flip (⊆)

{- |
x &#x2288; y = (x &#x2262; y) &#x2227; (x &#x2284; y)

U+2288, NEITHER A SUBSET OF NOR EQUAL TO
-}
(⊈) ∷ IntSet → IntSet → Bool
x ⊈ y = (x ≢ y) ∧ (x ⊄ y)

{- |
x &#x2289; y = (x &#x2262; y) &#x2227; (x &#x2285; y)

U+2289, NEITHER A SUPERSET OF NOR EQUAL TO
-}
(⊉) ∷ IntSet → IntSet → Bool
x ⊉ y = (x ≢ y) ∧ (x ⊅ y)

{- |
(&#x2282;) = 'isProperSubsetOf'

U+2282, SUBSET OF
-}
(⊂) ∷ IntSet → IntSet → Bool
(⊂) = isProperSubsetOf

{- |
(&#x2283;) = 'flip' (&#x2282;)

U+2283, SUPERSET OF
-}
(⊃) ∷ IntSet → IntSet → Bool
(⊃) = flip (⊂)

{- |
x &#x2284; y = 'not' (x &#x2282; y)

U+2284, NOT A SUBSET OF
-}
(⊄) ∷ IntSet → IntSet → Bool
x ⊄ y = not (x ⊂ y)

{- |
x &#x2285; y = 'not' (x &#x2283; y)

U+2285, NOT A SUPERSET OF
-}
(⊅) ∷ IntSet → IntSet → Bool
x ⊅ y = not (x ⊃ y)
