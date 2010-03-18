{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

{-|
Module     : Data.Sequence.Unicode
Copyright  : (c) 2009–2010 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}

module Data.Sequence.Unicode
    ( (∅)
    , (⊲), (⊳)
    , (⋈)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from containers:
import Data.Sequence ( Seq
                     , empty
                     , (<|), (|>)
                     , (><)
                     )


-------------------------------------------------------------------------------
-- Fixities
-------------------------------------------------------------------------------

infixr 5 ⋈
infixr 5 ⊲
infixl 5 ⊳


-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

{- |
(&#x2205;) = 'empty'

U+2205, EMPTY SET
-}
(∅) ∷ Seq α
(∅) = empty

{- |
(&#x22B2;) = ('<|')

U+22B2, NORMAL SUBGROUP OF
-}
(⊲) ∷ α → Seq α → Seq α
(⊲) = (<|)

{- |
(&#x22B3;) = ('|>')

U+22B3, CONTAINS AS NORMAL SUBGROUP
-}
(⊳) ∷ Seq α → α → Seq α
(⊳) = (|>)

{- |
(&#x22C8;) = ('><')

U+22C8, BOWTIE
-}
(⋈) ∷ Seq α → Seq α → Seq α
(⋈) = (><)
