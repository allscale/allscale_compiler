
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Analysis.Entities.DataRange(
    DataPoint,
    DataSpan,
    DataRange,
    point,
    passDataRange,
) where

import Control.DeepSeq
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics (Generic)
import Insieme.Adapter (CRepPtr,CRepArr,CSetPtr,dumpIrTree,passBoundSet,updateContext)

import qualified Data.ByteString as BS
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet


--
-- * Data Range
--

data DataPoint = DataPoint IR.Tree
    deriving (Eq,Ord,Show,Generic,NFData)

data DataSpan = DataSpan {
                    form :: DataPoint,
                    to   :: DataPoint
              }
    deriving (Eq,Ord,Show,Generic,NFData)

data DataRange = DataRange (BSet.UnboundSet DataSpan)
    deriving (Eq,Ord,Show,Generic,NFData)


point :: IR.Tree -> DataRange
point t = DataRange $ BSet.singleton $ DataSpan p p
  where
    p = DataPoint t



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
