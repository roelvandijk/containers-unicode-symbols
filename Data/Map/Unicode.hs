{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

{-|
Module     : Data.Map.Unicode
Copyright  : (c) 2009–2010 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}

module Data.Map.Unicode
    ( (∈), (∋), (∉), (∌)
    , (∅)
    , (∪), (∩)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool )
import Data.Ord      ( Ord )
import Data.Function ( flip )

-- from containers:
import Data.Map ( Map
                , member, notMember
                , empty
                , union, intersection
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
(∈) ∷ Ord k ⇒ k → Map k α → Bool
(∈) = member

{- |
(&#x220B;) = 'flip' (&#x2208;)

U+220B, CONTAINS AS MEMBER
-}
(∋) ∷ Ord k ⇒ Map k α → k → Bool
(∋) = flip (∈)

{- |
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Ord k ⇒ k → Map k α → Bool
(∉) = notMember

{- |
(&#x220C;) = 'flip' (&#x2209;)

U+220C, DOES NOT CONTAIN AS MEMBER
-}
(∌) ∷ Ord k ⇒ Map k α → k → Bool
(∌) = flip (∉)

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

