{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

{-|
Module     : Data.IntMap.Unicode
Copyright  : (c) 2009–2010 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}

module Data.IntMap.Unicode
    ( (∈), (∉)
    , (∅)
    , (∪), (∩)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool ( Bool  )
import Data.Int  ( Int )

-- from containers:
import Data.IntMap ( IntMap
                   , member, notMember
                   , empty
                   , union, intersection
                   )


-------------------------------------------------------------------------------
-- Fixities
-------------------------------------------------------------------------------

infix  4 ∈
infix  4 ∉


-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

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

