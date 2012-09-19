{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

{-|
Module     : Data.IntSet.Unicode
Copyright  : 2009–2012 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}

module Data.IntSet.Unicode
    ( (∈), (∋), (∉), (∌)
    , (∅)
    , (∪), (∖), (∆), (∩)
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
                   , union, difference, intersection
                   , isSubsetOf, isProperSubsetOf
                   )


-------------------------------------------------------------------------------
-- Fixities
-------------------------------------------------------------------------------

infix  4 ∈
infix  4 ∋
infix  4 ∉
infix  4 ∌
infix  4 ⊆
infix  4 ⊇
infix  4 ⊈
infix  4 ⊉
infix  4 ⊂
infix  4 ⊃
infix  4 ⊄
infix  4 ⊅
infixl 6 ∪
infixr 6 ∩
infixl 9 ∖
infixl 9 ∆


-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

{-|
(&#x2208;) = 'member'

U+2208, ELEMENT OF
-}
(∈) ∷ Int → IntSet → Bool
(∈) = member
{-# INLINE (∈) #-}

{-|
(&#x220B;) = 'flip' (&#x2208;)

U+220B, CONTAINS AS MEMBER
-}
(∋) ∷ IntSet → Int → Bool
(∋) = flip (∈)
{-# INLINE (∋) #-}

{-|
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Int → IntSet → Bool
(∉) = notMember
{-# INLINE (∉) #-}

{-|
(&#x220C;) = 'flip' (&#x2209;)

U+220C, DOES NOT CONTAIN AS MEMBER
-}
(∌) ∷ IntSet → Int → Bool
(∌) = flip (∉)
{-# INLINE (∌) #-}

{-|
(&#x2205;) = 'empty'

U+2205, EMPTY SET
-}
(∅) ∷ IntSet
(∅) = empty
{-# INLINE (∅) #-}

{-|
(&#x222A;) = 'union'

U+222A, UNION
-}
(∪) ∷ IntSet → IntSet → IntSet
(∪) = union
{-# INLINE (∪) #-}

{-|
(&#x2216;) = 'difference'

U+2216, SET MINUS
-}
(∖) ∷ IntSet → IntSet → IntSet
(∖) = difference
{-# INLINE (∖) #-}

{-|
Symmetric difference

a &#x2206; b = (a &#x2216; b) &#x222A; (b &#x2216; a)

U+2206, INCREMENT
-}
(∆) ∷ IntSet → IntSet → IntSet
a ∆ b = (a ∖ b) ∪ (b ∖ a)
{-# INLINE (∆) #-}

{-|
(&#x2229;) = 'intersection'

U+2229, INTERSECTION
-}
(∩) ∷ IntSet → IntSet → IntSet
(∩) = intersection
{-# INLINE (∩) #-}

{-|
(&#x2286;) = 'isSubsetOf'

U+2286, SUBSET OF OR EQUAL TO
-}
(⊆) ∷ IntSet → IntSet → Bool
(⊆) = isSubsetOf
{-# INLINE (⊆) #-}

{-|
(&#x2287;) = 'flip' (&#x2286;)

U+2287, SUPERSET OF OR EQUAL TO
-}
(⊇) ∷ IntSet → IntSet → Bool
(⊇) = flip (⊆)
{-# INLINE (⊇) #-}

{-|
a &#x2288; b = (a &#x2262; b) &#x2227; (a &#x2284; b)

U+2288, NEITHER A SUBSET OF NOR EQUAL TO
-}
(⊈) ∷ IntSet → IntSet → Bool
a ⊈ b = (a ≢ b) ∧ (a ⊄ b)
{-# INLINE (⊈) #-}

{-|
a &#x2289; b = (a &#x2262; b) &#x2227; (a &#x2285; b)

U+2289, NEITHER A SUPERSET OF NOR EQUAL TO
-}
(⊉) ∷ IntSet → IntSet → Bool
a ⊉ b = (a ≢ b) ∧ (a ⊅ b)
{-# INLINE (⊉) #-}

{-|
(&#x2282;) = 'isProperSubsetOf'

U+2282, SUBSET OF
-}
(⊂) ∷ IntSet → IntSet → Bool
(⊂) = isProperSubsetOf
{-# INLINE (⊂) #-}

{-|
(&#x2283;) = 'flip' (&#x2282;)

U+2283, SUPERSET OF
-}
(⊃) ∷ IntSet → IntSet → Bool
(⊃) = flip (⊂)
{-# INLINE (⊃) #-}

{-|
a &#x2284; b = 'not' (a &#x2282; b)

U+2284, NOT A SUBSET OF
-}
(⊄) ∷ IntSet → IntSet → Bool
a ⊄ b = not (a ⊂ b)
{-# INLINE (⊄) #-}

{-|
a &#x2285; b = 'not' (a &#x2283; b)

U+2285, NOT A SUPERSET OF
-}
(⊅) ∷ IntSet → IntSet → Bool
a ⊅ b = not (a ⊃ b)
{-# INLINE (⊅) #-}
