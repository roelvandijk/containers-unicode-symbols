{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

{-|
Module     : Data.Set.Unicode
Copyright  : 2009–2011 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}

module Data.Set.Unicode
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
import Data.Ord      ( Ord )

-- from base-unicode-symbols:
import Data.Eq.Unicode   ( (≢) )
import Data.Bool.Unicode ( (∧) )

-- from containers:
import Data.Set ( Set
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
(∈) ∷ Ord α ⇒ α → Set α → Bool
(∈) = member
{-# INLINE (∈) #-}

{-|
(&#x220B;) = 'flip' (&#x2208;)

U+220B, CONTAINS AS MEMBER
-}
(∋) ∷ Ord α ⇒ Set α → α → Bool
(∋) = flip (∈)
{-# INLINE (∋) #-}

{-|
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Ord α ⇒ α → Set α → Bool
(∉) = notMember
{-# INLINE (∉) #-}

{-|
(&#x220C;) = 'flip' (&#x2209;)

U+220C, DOES NOT CONTAIN AS MEMBER
-}
(∌) ∷ Ord α ⇒ Set α → α → Bool
(∌) = flip (∉)
{-# INLINE (∌) #-}

{-|
(&#x2205;) = 'empty'

U+2205, EMPTY SET
-}
(∅) ∷ Set α
(∅) = empty
{-# INLINE (∅) #-}

{-|
(&#x222A;) = 'union'

U+222A, UNION
-}
(∪) ∷ Ord α ⇒ Set α → Set α → Set α
(∪) = union
{-# INLINE (∪) #-}

{-|
(&#x2216;) = 'difference'

U+2216, SET MINUS
-}
(∖) ∷ Ord α ⇒ Set α → Set α → Set α
(∖) = difference
{-# INLINE (∖) #-}

{-|
Symmetric difference

a &#x2206; b = (a &#x2216; b) &#x222A; (b &#x2216; a)

U+2206, INCREMENT
-}
(∆) ∷ Ord α ⇒ Set α → Set α → Set α
a ∆ b = (a ∖ b) ∪ (b ∖ a)
{-# INLINE (∆) #-}

{-|
(&#x2229;) = 'intersection'

U+2229, INTERSECTION
-}
(∩) ∷ Ord α ⇒ Set α → Set α → Set α
(∩) = intersection
{-# INLINE (∩) #-}

{-|
(&#x2286;) = 'isSubsetOf'

U+2286, SUBSET OF OR EQUAL TO
-}
(⊆) ∷ Ord α ⇒ Set α → Set α → Bool
(⊆) = isSubsetOf
{-# INLINE (⊆) #-}

{-|
(&#x2287;) = 'flip' (&#x2286;)

U+2287, SUPERSET OF OR EQUAL TO
-}
(⊇) ∷ Ord α ⇒ Set α → Set α → Bool
(⊇) = flip (⊆)
{-# INLINE (⊇) #-}

{-|
a &#x2288; b = (a &#x2262; b) &#x2227; (a &#x2284; b)

U+2288, NEITHER A SUBSET OF NOR EQUAL TO
-}
(⊈) ∷ Ord α ⇒ Set α → Set α → Bool
a ⊈ b = (a ≢ b) ∧ (a ⊄ b)
{-# INLINE (⊈) #-}

{-|
a &#x2289; b = (a &#x2262; b) &#x2227; (a &#x2285; b)

U+2289, NEITHER A SUPERSET OF NOR EQUAL TO
-}
(⊉) ∷ Ord α ⇒ Set α → Set α → Bool
a ⊉ b = (a ≢ b) ∧ (a ⊅ b)
{-# INLINE (⊉) #-}

{-|
(&#x2282;) = 'isProperSubsetOf'

U+2282, SUBSET OF
-}
(⊂) ∷ Ord α ⇒ Set α → Set α → Bool
(⊂) = isProperSubsetOf
{-# INLINE (⊂) #-}

{-|
(&#x2283;) = 'flip' (&#x2282;)

U+2283, SUPERSET OF
-}
(⊃) ∷ Ord α ⇒ Set α → Set α → Bool
(⊃) = flip (⊂)
{-# INLINE (⊃) #-}

{-|
a &#x2284; b = 'not' (a &#x2282; b)

U+2284, NOT A SUBSET OF
-}
(⊄) ∷ Ord α ⇒ Set α → Set α → Bool
a ⊄ b = not (a ⊂ b)
{-# INLINE (⊄) #-}

{-|
a &#x2285; b = 'not' (a &#x2283; b)

U+2285, NOT A SUPERSET OF
-}
(⊅) ∷ Ord α ⇒ Set α → Set α → Bool
a ⊅ b = not (a ⊃ b)
{-# INLINE (⊅) #-}
