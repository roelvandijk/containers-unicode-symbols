{-# LANGUAGE UnicodeSyntax #-}

module Data.IntMap.Unicode
    ( (∈), (∉)
    , (∅)
    , (∪), (∩)
    ) where

import Data.IntMap ( IntMap
                   , member, notMember
                   , empty
                   , union, intersection
                   )

{- |
(&#x2208;) = 'member'

U+2208, ELEMENT OF
-}
(∈) ∷ Int → IntMap α → Bool
(∈) = member

{- |
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Int → IntMap α → Bool
(∉) = notMember

{- |
(&#x2205;) = 'empty'

U+2205, EMPTY SET
-}
(∅) ∷ IntMap α
(∅) = empty

{- |
(&#x222A;) = 'union'

U+222A, UNION
-}
(∪) ∷ IntMap α → IntMap α → IntMap α
(∪) = union

{- |
(&#x2229;) = 'intersection'

U+2229, INTERSECTION
-}
(∩) ∷ IntMap α → IntMap β → IntMap α
(∩) = intersection

