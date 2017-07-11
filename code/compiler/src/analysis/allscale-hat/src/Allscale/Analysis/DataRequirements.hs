
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Analysis.DataRequirements where

import Control.DeepSeq
import Control.Monad
import Data.Foldable (or)
import Data.List
import Data.Set
import Data.Maybe
import Data.Typeable
import Debug.Trace
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics (Generic)
import Insieme.Adapter (passBoundSet,updateContext)
import Insieme.Analysis.Arithmetic (arithmeticValue,SymbolicFormulaSet)
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.SymbolicFormula (SymbolicFormula)
import Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toComposed,toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Inspire.BinaryDumper (dumpBinaryDump)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import Insieme.Inspire.Visit

import qualified Data.ByteString as BS
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

--
-- * Data Requirements
--

data AccessMode = ReadOnly
                | ReadWrite
    deriving (Eq,Ord,Show,Generic,NFData)

data DataPoint = DataPoint IR.Tree
    deriving (Eq,Ord,Show,Generic,NFData)

data DataSpan = DataSpan {
                    form :: DataPoint,
                    to   :: DataPoint
              }
    deriving (Eq,Ord,Show,Generic,NFData)

data DataRange = DataRange (BSet.UnboundSet DataSpan)
    deriving (Eq,Ord,Show,Generic,NFData)

data DataRequirement = DataRequirement {
                            dataItemRef :: IR.Tree,
                            range       :: DataRange,
                            accessMode  :: AccessMode
                        }
    deriving (Eq,Ord,Show,Generic,NFData)



data DataRequirements = DataRequirements (BSet.UnboundSet DataRequirement)
    deriving (Eq,Ord,Show,Generic,NFData)



--
-- * Data Requirements Lattice
--


instance Solver.Lattice DataRequirements where
    bot   = DataRequirements $ BSet.empty
    merge (DataRequirements a) (DataRequirements b) = DataRequirements $ BSet.union a b

instance Solver.ExtLattice DataRequirements where
    top   = DataRequirements $ BSet.Universe


data DataRequirementAnalysis = DataRequirementAnalysis
    deriving (Typeable)



dataRequirements :: NodeAddress -> Solver.TypedVar DataRequirements
dataRequirements addr = var
    where
        var = Solver.mkVariable (idGen addr) [] Solver.top

        analysis = Solver.mkAnalysisIdentifier DataRequirementAnalysis "DR"

        idGen = Solver.mkIdentifierFromExpression analysis

-- * FFI

type CDataRequirement = ()
type CDataRequirementSet = ()
type CDataRequirements = ()
type CDataRange = ()
type CDataSpan = ()
type CDataSpanSet = ()
type CDataPoint = ()
type CIrTree = ()

foreign export ccall "hat_hs_data_requirements"
  hsDataRequirements :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (Ptr CDataRequirements)

hsDataRequirements ctx_hs stmt_hs = do
    ctx  <- deRefStablePtr ctx_hs
    stmt <- deRefStablePtr stmt_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (dataRequirements stmt)
    let ctx_c =  Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    passDataRequirements ctx_c res

foreign import ccall "hat_c_mk_ir_tree"
  mkCIrTree :: Ctx.CContext -> CString -> CSize -> IO (Ptr CIrTree)

foreign import ccall "hat_c_mk_data_point"
  mkCDataPoint :: Ptr CIrTree -> IO (Ptr CDataPoint)

foreign import ccall "hat_c_mk_data_span"
  mkCDataSpan :: Ptr CDataPoint -> Ptr CDataPoint -> IO (Ptr CDataSpan)

foreign import ccall "hat_c_mk_data_span_set"
  mkCDataSpanSet :: Ptr (Ptr CDataSpan) -> CLLong -> IO (Ptr CDataSpanSet)

foreign import ccall "hat_c_mk_data_range"
  mkCDataRange :: Ptr CDataSpanSet -> IO (Ptr CDataRange)

foreign import ccall "hat_c_mk_data_requirement"
  mkCDataRequirement :: Ptr CIrTree -> Ptr CDataRange -> CInt -> IO (Ptr CDataRequirement)

foreign import ccall "hat_c_mk_data_requirement_set"
  mkCDataRequirementSet :: Ptr (Ptr CDataRequirement) -> CLLong -> IO (Ptr CDataSpanSet)

foreign import ccall "hat_c_mk_data_requirements"
  mkCDataRequirements :: Ptr CDataRequirementSet -> IO (Ptr CDataRequirements)

passDataRequirements :: Ctx.CContext -> DataRequirements -> IO (Ptr CDataRequirements)
passDataRequirements ctx (DataRequirements s) = do
    s_c <- passBoundSet passDataRequirement mkCDataRequirementSet s
    mkCDataRequirements s_c
  where
    passDataRequirement :: DataRequirement -> IO (Ptr CDataRequirement)
    passDataRequirement (DataRequirement d r a) = do
        d_c <- BS.useAsCStringLen (dumpBinaryDump d) (mkCIrTree' ctx)
        r_c <- passDataRange r
        mkCDataRequirement d_c r_c (convertAccessMode a)

    passDataRange :: DataRange -> IO (Ptr CDataRange)
    passDataRange (DataRange s) = do
        s_c <- passBoundSet passDataSpan mkCDataSpanSet s
        mkCDataRange s_c

    passDataSpan :: DataSpan -> IO (Ptr CDataSpan)
    passDataSpan (DataSpan f t) = do
        f_c <- passDataPoint f
        t_c <- passDataPoint t
        mkCDataSpan f_c t_c

    passDataPoint :: DataPoint -> IO (Ptr CDataPoint)
    passDataPoint (DataPoint irtree) = do
        irtree_c <- BS.useAsCStringLen (dumpBinaryDump irtree) (mkCIrTree' ctx)
        mkCDataPoint irtree_c

    mkCIrTree' :: Ctx.CContext -> CStringLen -> IO (Ptr CIrTree)
    mkCIrTree' ctx (sz,s) = mkCIrTree ctx sz (fromIntegral s)

    convertAccessMode :: AccessMode -> CInt
    convertAccessMode ReadOnly = 0
    convertAccessMode ReadWrite = 1
