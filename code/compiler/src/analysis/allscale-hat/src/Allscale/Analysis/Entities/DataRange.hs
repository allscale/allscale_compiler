
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Analysis.Entities.DataRange (
    DataPoint(..),
    DataSpan(..),
    DataRange(..),
    point,
    defineIteratorRange,
    printRange,
) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Insieme.Adapter.Utils (pprintTree)
import Insieme.Inspire (substitute)

import qualified Data.Map as Map
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet


--
-- * Data Range
--

data DataPoint = DataPoint IR.Tree
    deriving (Eq,Ord,Show,Generic,NFData)

printPoint :: DataPoint -> String
printPoint (DataPoint t) = pprintTree t

    
data DataSpan = DataSpan {
                    from :: DataPoint,
                    to   :: DataPoint
              }
    deriving (Eq,Ord,Show,Generic,NFData)

data DataRange = DataRange (BSet.UnboundSet DataSpan)
    deriving (Eq,Ord,Show,Generic,NFData)


point :: IR.Tree -> DataRange
point t = DataRange $ BSet.singleton $ DataSpan p p
  where
    p = DataPoint t


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
        
        sub' o n t = substitute (Map.singleton o n) t

printRange :: DataRange -> String
printRange (DataRange BSet.Universe) = "-all-"
printRange (DataRange set) = concat $ go <$> BSet.toList set
  where
    go (DataSpan a b) | a == b = printPoint a
    go (DataSpan a b)          = "span(" ++ (printPoint a) ++ "," ++ (printPoint b) ++ ")"
