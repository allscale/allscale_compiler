
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Analysis.Entities.DataRange(
    DataPoint,
    DataSpan,
    DataRange,
    point,
    defineIteratorRange,
    passDataRange,
    printRange,
) where

import Control.DeepSeq
import Foreign.C.Types
import GHC.Generics (Generic)
import Insieme.Adapter (CRepPtr,CRepArr,CSetPtr,dumpIrTree,passBoundSet,pprintTree)
import Insieme.Inspire.Transform (substitute)

import qualified Data.Map as Map
import qualified Insieme.Context as Ctx
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

--
-- * FFI
--

foreign import ccall "hat_c_mk_data_point"
  mkCDataPoint :: CRepPtr IR.Tree -> IO (CRepPtr DataPoint)

foreign import ccall "hat_c_mk_data_span"
  mkCDataSpan :: CRepPtr DataPoint -> CRepPtr DataPoint -> IO (CRepPtr DataSpan)

foreign import ccall "hat_c_mk_data_span_set"
  mkCDataSpanSet :: CRepArr DataSpan -> CLLong -> IO (CSetPtr DataSpan)

foreign import ccall "hat_c_mk_data_range"
  mkCDataRange :: CSetPtr DataSpan -> IO (CRepPtr DataRange)

passDataRange :: Ctx.CContext -> DataRange -> IO (CRepPtr DataRange)
passDataRange ctx (DataRange s) = do
    s_c <- passBoundSet passDataSpan mkCDataSpanSet s
    mkCDataRange s_c
  where

    passDataSpan :: DataSpan -> IO (CRepPtr DataSpan)
    passDataSpan (DataSpan f t) = do
        f_c <- passDataPoint f
        t_c <- passDataPoint t
        mkCDataSpan f_c t_c

    passDataPoint :: DataPoint -> IO (CRepPtr DataPoint)
    passDataPoint (DataPoint irtree) = do
        irtree_c <- dumpIrTree ctx irtree
        mkCDataPoint irtree_c
