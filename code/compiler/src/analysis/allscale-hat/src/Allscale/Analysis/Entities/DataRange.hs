
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Allscale.Analysis.Entities.DataRange (
    DataPoint(..),
    DataSpan(..),
    DataRange(..),
    point,
    containsIterator,
    defineIteratorRange,
    printRange,
) where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics (Generic)
import Insieme.Adapter.Utils (pprintTree)
import Insieme.Inspire (substitutePrunable)

import qualified Data.AbstractSet as Set
import qualified Insieme.Inspire as IR
import qualified Insieme.Query as Q
import qualified Insieme.Utils.BoundSet as BSet


--
-- * Data Range
--

data DataPoint = DataPoint IR.Tree
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

printPoint :: DataPoint -> String
printPoint (DataPoint t) = pprintTree t


data DataSpan = DataSpan {
                    from :: DataPoint,
                    to   :: DataPoint
              }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

data DataRange = DataRange (BSet.UnboundSet DataSpan)
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)


point :: IR.Tree -> DataRange
point t = DataRange $ BSet.singleton $ DataSpan p p
  where
    p = DataPoint t


containsIterator :: IR.Tree -> DataRange -> Bool
containsIterator _ (DataRange spans) | BSet.isUniverse spans = False
containsIterator i (DataRange spans) = any f $ BSet.toSet spans
  where
    f (DataSpan a b) = g a || g b
    g (DataPoint t) = Set.member i $ Q.getFreeVariables t



substituteVariable :: IR.Tree -> IR.Tree -> IR.Tree -> IR.Tree
substituteVariable v e t = if isFree then res else t
  where
    isFree = Set.member v $ Q.getFreeVariables t
    res = substitutePrunable p s t

    p n | Q.isType n = True
    p n | Q.isLambdaExpr n = True
    p n = not (Set.member v $ Q.getFreeVariables n)
    p _ = False

    s n | n == v = e
    s n = n



defineIteratorRange ::  IR.Tree             -- ^ a IR iterator variable
                     -> IR.Tree             -- ^ the start value of the iterator variable
                     -> IR.Tree             -- ^ the end value of the iterator variable
                     -> DataRange           -- ^ the data range to modify
                     -> DataRange           -- ^ the resulting range, fixing the iterator to the given value
defineIteratorRange var from to (DataRange spans) = DataRange $ define spans
  where
    define BSet.Universe = BSet.Universe
    define     spans     = BSet.map go spans
      where
        go (DataSpan (DataPoint f) (DataPoint t)) = DataSpan (DataPoint $ sub' var from f) (DataPoint $ sub' var to t)

        sub' = substituteVariable


printRange :: DataRange -> String
printRange (DataRange BSet.Universe) = "-all-"
printRange (DataRange set) = concat $ go <$> BSet.toList set
  where
    go (DataSpan a b) | a == b = printPoint a
    go (DataSpan a b)          = "span(" ++ (printPoint a) ++ "," ++ (printPoint b) ++ ")"
