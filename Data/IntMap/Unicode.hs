{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

{-|
Module     : Data.IntMap.Unicode
Copyright  : (c) 2009–2010 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}

module Data.IntMap.Unicode
    ( (∈), (∋), (∉), (∌)
    , (∅)
    , (∪), (∖), (∆), (∩)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool )
import Data.Int      ( Int )
import Data.Function ( flip )

-- from containers:
import Data.IntMap ( IntMap
                   , member, notMember
                   , empty
                   , union, difference, intersection
                   )


-------------------------------------------------------------------------------
-- Fixities
-------------------------------------------------------------------------------

infix  4 ∈
infix  4 ∋
infix  4 ∉
infix  4 ∌
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
(∈) ∷ Int → IntMap α → Bool
(∈) = member

{-|
(&#x220B;) = 'flip' (&#x2208;)

U+220B, CONTAINS AS MEMBER
-}
(∋) ∷ IntMap α → Int → Bool
(∋) = flip (∈)

{-|
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Int → IntMap α → Bool
(∉) = notMember

{-|
(&#x220C;) = 'flip' (&#x2209;)

U+220C, DOES NOT CONTAIN AS MEMBER
-}
(∌) ∷ IntMap α → Int → Bool
(∌) = flip (∉)

{-|
(&#x2205;) = 'empty'

U+2205, EMPTY SET
-}
(∅) ∷ IntMap α
(∅) = empty

{-|
(&#x222A;) = 'union'

U+222A, UNION
-}
(∪) ∷ IntMap α → IntMap α → IntMap α
(∪) = union

{-|
(&#x2216;) = 'difference'

U+2216, SET MINUS
-}
(∖) ∷ IntMap α → IntMap β → IntMap α
(∖) = difference

{-|
Symmetric difference

a &#x2206; b = (a &#x2216; b) &#x222A; (b &#x2216; a)

U+2206, INCREMENT
-}
(∆) ∷ IntMap α → IntMap α → IntMap α
a ∆ b = (a ∖ b) ∪ (b ∖ a)

{-|
(&#x2229;) = 'intersection'

U+2229, INTERSECTION
-}
(∩) ∷ IntMap α → IntMap β → IntMap α
(∩) = intersection

