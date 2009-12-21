{-# LANGUAGE UnicodeSyntax #-}

module Data.Map.Unicode
    ( (∈), (∉)
    , (∅)
    , (∪), (∩)
    ) where

import Data.Map ( Map
                , member, notMember
                , empty
                , union, intersection
                )

{- |
(&#x2208;) = 'member'

U+2208, ELEMENT OF
-}
(∈) ∷ Ord k ⇒ k → Map k α → Bool
(∈) = member

{- |
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Ord k ⇒ k → Map k α → Bool
(∉) = notMember

{- |
(&#x2205;) = 'empty'

U+2205, EMPTY SET
-}
(∅) ∷ Map k α
(∅) = empty

{- |
(&#x222A;) = 'union'

U+222A, UNION
-}
(∪) ∷ Ord k ⇒ Map k α → Map k α → Map k α
(∪) = union

{- |
(&#x2229;) = 'intersection'

U+2229, INTERSECTION
-}
(∩) ∷ Ord k ⇒ Map k α → Map k β → Map k α
(∩) = intersection

