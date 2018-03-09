module Allscale.Adapter where

import Insieme.Adapter

import Control.Exception
import Control.Exception.Base (evaluate)
import Control.Monad
import Data.Bits.Bitwise (toListLE)
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Insieme.Inspire.NodeAddress
import System.Timeout (timeout)

import qualified Allscale.Analysis.DataItemAccesses as DIaccess
import qualified Allscale.Analysis.DataRequirements as Dreq
import qualified Allscale.Analysis.Diagnostics as Diag
import qualified Allscale.Analysis.Entities.DataRange as DR
import qualified Allscale.Analysis.OutOfBounds as OOB
import qualified Data.Set as Set
import qualified Insieme.Solver as Solver
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR

-- * Exports

foreign export ccall "hat_out_of_bounds"
  outOfBounds :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (AnalysisResultPtr CInt)

foreign export ccall "hat_hs_diagnostics"
  diagnostics :: StablePtr Ctx.Context -> StablePtr NodeAddress -> DiagnosisFlags -> IO (AnalysisResultPtr (CRepPtr Diag.Issues))

foreign export ccall "hat_hs_data_requirements"
  dataRequirements :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (AnalysisResultPtr (CRepPtr Dreq.DataRequirements))

foreign export ccall "hat_hs_data_item_accesses"
  dataItemAccesses :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (AnalysisResultPtr (CSetPtr NodeAddress))

-- * Out Of Bounds

outOfBounds :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (AnalysisResultPtr CInt)
outOfBounds ctx_hs expr_hs = do
    ctx  <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let (result,ns) = OOB.outOfBounds (Ctx.getSolverState ctx) expr
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ serialize result
    case result of
        Just r  -> allocAnalysisResult ctx_nhs False r
        Nothing -> allocAnalysisResult ctx_hs  True =<< serialize OOB.MayBeOutOfBounds
  where
    serialize = evaluate . fromIntegral . fromEnum

-- * Diagnostisc

type DiagnosisFlags = CULong

diagnostics :: StablePtr Ctx.Context -> StablePtr NodeAddress -> DiagnosisFlags -> IO (AnalysisResultPtr (CRepPtr Diag.Issues))
diagnostics ctx_hs addr_hs diags = do
    ctx  <- deRefStablePtr ctx_hs
    addr <- deRefStablePtr addr_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let (res,ns) = Diag.runDiagnostics (Ctx.getSolverState ctx) addr (selectDiags diags)
    let (ctx_c) = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ passIssues ctx_c res
    case result of
        Just r  -> allocAnalysisResult ctx_nhs False r
        Nothing -> allocAnalysisResult ctx_hs  True =<< passIssues ctx_c (Diag.mkIssues [])

foreign import ccall "hat_c_mk_issue"
  mkCIssue :: CRepPtr NodeAddress -> CInt -> CString -> IO (CRepPtr Diag.Issue)

foreign import ccall "hat_c_del_issue"
  delCIssue :: CRepPtr Diag.Issue -> IO ()

foreign import ccall "hat_c_mk_issues"
  mkCIssues :: CRepArr Diag.Issue -> CSize -> IO (CRepPtr Diag.Issues)

passIssues :: Ctx.CContext -> Diag.Issues -> IO (CRepPtr Diag.Issues)
passIssues ctx (Diag.Issues is) = bracket
    (forM (Set.toList is) passIssue)
    (mapM_ delCIssue)
    (\is_c -> withArrayUnsignedLen is_c mkCIssues)
  where
    passIssue :: Diag.Issue -> IO (CRepPtr Diag.Issue)
    passIssue (Diag.Issue t e m) = bracket
        (passNodeAddress ctx t)
        (delCNodeAddress)
        (\t_c -> withCString m $ mkCIssue t_c (convertErrorCode e))

    convertErrorCode :: Diag.ErrorCode -> CInt
    convertErrorCode = fromIntegral . fromEnum

selectDiags :: DiagnosisFlags -> [Diag.DiagnosisFunction]
selectDiags = catMaybes . map sel . zip [0..] . toListLE
  where
    sel (0, True) = Just Diag.unknownReferenceDiagnosis
    sel (1, True) = Just Diag.globalVariableDiagnosis
    sel (2, True) = Just Diag.uncertainAccessDiagnosis
    sel _         = Nothing

-- * Data Range

foreign import ccall "hat_c_mk_data_point"
  mkCDataPoint :: CRepPtr IR.Tree -> IO (CRepPtr DR.DataPoint)

foreign import ccall "hat_c_del_data_point"
  delCDataPoint :: CRepPtr DR.DataPoint -> IO ()

foreign import ccall "hat_c_mk_data_span"
  mkCDataSpan :: CRepPtr DR.DataPoint -> CRepPtr DR.DataPoint -> IO (CRepPtr DR.DataSpan)

foreign import ccall "hat_c_mk_data_span_set"
  mkCDataSpanSet :: CRepArr DR.DataSpan -> CLLong -> IO (CSetPtr DR.DataSpan)

foreign import ccall "hat_c_del_data_span_set"
  delCDataSpanSet :: CSetPtr DR.DataSpan -> IO ()

foreign import ccall "hat_c_mk_data_range"
  mkCDataRange :: CSetPtr DR.DataSpan -> IO (CRepPtr DR.DataRange)

foreign import ccall "hat_c_del_data_range"
  delCDataRange :: CRepPtr DR.DataRange -> IO ()

passDataRange :: Ctx.CContext -> DR.DataRange -> IO (CRepPtr DR.DataRange)
passDataRange ctx (DR.DataRange s) = bracket
    (passBoundSet passDataSpan mkCDataSpanSet s)
    (delCDataSpanSet)
    (mkCDataRange)
  where

    passDataSpan :: DR.DataSpan -> IO (CRepPtr DR.DataSpan)
    passDataSpan (DR.DataSpan f t) = bracket
        ((,) <$> passDataPoint f <*> passDataPoint t)
        (\(f_c, t_c) -> delCDataPoint f_c >> delCDataPoint t_c)
        (\(f_c, t_c) -> mkCDataSpan f_c t_c)

    passDataPoint :: DR.DataPoint -> IO (CRepPtr DR.DataPoint)
    passDataPoint (DR.DataPoint irtree) = bracket
        (dumpIrTree ctx irtree)
        (delCIrTree)
        (mkCDataPoint)

-- * Data Requirements

dataRequirements :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (AnalysisResultPtr (CRepPtr Dreq.DataRequirements))
dataRequirements ctx_hs stmt_hs = do
    ctx  <- deRefStablePtr ctx_hs
    stmt <- deRefStablePtr stmt_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (Dreq.dataRequirements stmt)
    let ctx_c =  Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ passDataRequirements ctx_c res
    case result of
        Just r  -> allocAnalysisResult ctx_nhs False r
        Nothing -> allocAnalysisResult ctx_hs  True =<< passDataRequirements ctx_c Solver.top

foreign import ccall "hat_c_mk_data_requirement"
  mkCDataRequirement :: CRepPtr IR.Tree -> CRepPtr DR.DataRange -> CInt -> IO (CRepPtr Dreq.DataRequirement)

foreign import ccall "hat_c_mk_data_requirement_set"
  mkCDataRequirementSet :: CRepArr Dreq.DataRequirement -> CLLong -> IO (CSetPtr Dreq.DataRequirement)

foreign import ccall "hat_c_del_data_requirement_set"
  delCDataRequirementSet :: CSetPtr Dreq.DataRequirement -> IO ()

foreign import ccall "hat_c_mk_data_requirements"
  mkCDataRequirements :: CSetPtr Dreq.DataRequirement -> IO (CRepPtr Dreq.DataRequirements)

passDataRequirements :: Ctx.CContext -> Dreq.DataRequirements -> IO (CRepPtr Dreq.DataRequirements)
passDataRequirements ctx (Dreq.DataRequirements s) = bracket
    (passBoundSet passDataRequirement mkCDataRequirementSet s)
    (delCDataRequirementSet)
    (mkCDataRequirements)
  where

    passDataRequirement :: Dreq.DataRequirement -> IO (CRepPtr Dreq.DataRequirement)
    passDataRequirement (Dreq.DataRequirement d r a) = bracket
        ((,) <$> dumpIrTree ctx d <*> passDataRange ctx r)
        (\(d_c, r_c) -> delCIrTree d_c >> delCDataRange r_c)
        (\(d_c, r_c) -> mkCDataRequirement d_c r_c (convertAccessMode a))

    convertAccessMode :: Dreq.AccessMode -> CInt
    convertAccessMode Dreq.ReadOnly = 0
    convertAccessMode Dreq.ReadWrite = 1


-- * Data Item Accesses

dataItemAccesses :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (AnalysisResultPtr (CSetPtr NodeAddress))
dataItemAccesses ctx_hs stmt_hs = do
    ctx  <- deRefStablePtr ctx_hs
    stmt <- deRefStablePtr stmt_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let ctx_c = Ctx.getCContext ctx

    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (DIaccess.dataItemAccesses stmt)
    ctx_new_hs <- newStablePtr $ ctx { Ctx.getSolverState = ns }

    --let results = Ref.unRS $ ComposedValue.toValue res :: BSet.UnboundSet (Ref.Reference FieldIndex.SimpleFieldIndex)
    result <- timeout timelimit $ serialize ctx_c res
    case result of
        Just r  -> allocAnalysisResult ctx_new_hs False r
        Nothing -> allocAnalysisResult ctx_hs True =<< serialize ctx_c Solver.top
  where
    serialize ctx_c results = passBoundSet (passNodeAddress ctx_c) mkCNodeAddressSet
                            $ case results of
                                DIaccess.DataItemAccesses s -> s

