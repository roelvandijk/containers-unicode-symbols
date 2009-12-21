{-# LANGUAGE UnicodeSyntax #-}

module Data.IntSet.Unicode
    ( (∈), (∉)
    , (∅)
    , (∪), (∩)
    , (⊆), (⊇), (⊈), (⊉)
    , (⊂), (⊃), (⊄), (⊅)
    ) where

import Prelude.Unicode ( (≢), (∧) )
import Data.IntSet ( IntSet
                   , member, notMember
                   , empty
                   , union, intersection
                   , isSubsetOf, isProperSubsetOf
                   )

{- |
(&#x2208;) = 'member'

U+2208, ELEMENT OF
-}
(∈) ∷ Int -> IntSet → Bool
(∈) = member

{- |
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Int -> IntSet → Bool
(∉) = notMember

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
