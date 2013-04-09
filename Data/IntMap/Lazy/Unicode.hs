{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-|
Module     : Data.IntMap.Lazy.Unicode
Copyright  : 2009–2012 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}

module Data.IntMap.Lazy.Unicode
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
#ifdef CONTAINERS_OLD
#define MODULE Data.IntMap
#else
#define MODULE Data.IntMap.Lazy
#endif
import MODULE ( IntMap
              , member, notMember
              , empty
              , union, difference, intersection
              )
#undef MODULE


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
{-# INLINE (∈) #-}

{-|
(&#x220B;) = 'flip' (&#x2208;)

U+220B, CONTAINS AS MEMBER
-}
(∋) ∷ IntMap α → Int → Bool
(∋) = flip (∈)
{-# INLINE (∋) #-}

{-|
(&#x2209;) = 'notMember'

U+2209, NOT AN ELEMENT OF
-}
(∉) ∷ Int → IntMap α → Bool
(∉) = notMember
{-# INLINE (∉) #-}

{-|
(&#x220C;) = 'flip' (&#x2209;)

U+220C, DOES NOT CONTAIN AS MEMBER
-}
(∌) ∷ IntMap α → Int → Bool
(∌) = flip (∉)
{-# INLINE (∌) #-}

{-|
(&#x2205;) = 'empty'

U+2205, EMPTY SET
-}
(∅) ∷ IntMap α
(∅) = empty
{-# INLINE (∅) #-}

{-|
(&#x222A;) = 'union'

U+222A, UNION
-}
(∪) ∷ IntMap α → IntMap α → IntMap α
(∪) = union
{-# INLINE (∪) #-}

{-|
(&#x2216;) = 'difference'

U+2216, SET MINUS
-}
(∖) ∷ IntMap α → IntMap β → IntMap α
(∖) = difference
{-# INLINE (∖) #-}

{-|
Symmetric difference

a &#x2206; b = (a &#x2216; b) &#x222A; (b &#x2216; a)

U+2206, INCREMENT
-}
(∆) ∷ IntMap α → IntMap α → IntMap α
a ∆ b = (a ∖ b) ∪ (b ∖ a)
{-# INLINE (∆) #-}

{-|
(&#x2229;) = 'intersection'

U+2229, INTERSECTION
-}
(∩) ∷ IntMap α → IntMap β → IntMap α
(∩) = intersection
{-# INLINE (∩) #-}

