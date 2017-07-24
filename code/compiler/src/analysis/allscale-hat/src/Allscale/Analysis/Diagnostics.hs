{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Allscale.Analysis.Diagnostics where

import Control.Monad
import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics (Generic)
import Insieme.Adapter (CRep,CSet,CRepPtr,CSetPtr,CRepArr,updateContext,passNodeAddress,withArrayUnsignedLen)
import Insieme.Inspire.NodeAddress

import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Context as Ctx

-- * Issues

data Severity = Warning | Error
  deriving (Eq, Ord, Show, Generic, NFData)

data Categroy = Basic
  deriving (Eq, Ord, Show, Generic, NFData)

data Issue = Issue { target   :: NodeAddress
                   , severity :: Severity
                   , category :: Categroy
                   , message  :: String
                   }
  deriving (Eq, Ord, Show, Generic, NFData)

newtype Issues = Issues [Issue]
  deriving (Eq, Ord, Show, Generic, NFData)

instance Solver.Lattice Issues where
    bot = Issues []
    Issues x `merge` Issues y = Issues $ x ++ y

-- * Analysis

data DataRequirementAnalysis = DataRequirementAnalysis
  deriving (Typeable)

diagnosticsAnalysis :: NodeAddress -> Solver.TypedVar Issues
diagnosticsAnalysis addr = Solver.mkVariable (idGen addr) []
                         $ Issues [ Issue addr Warning Basic "Diagnostics output not implemented."
                                  , Issue addr Error Basic "You Must Construct Additional Pylons!"
                                  ]
  where
    analysis = Solver.mkAnalysisIdentifier DataRequirementAnalysis "DIAG"

    idGen = Solver.mkIdentifierFromExpression analysis

-- * FFI

foreign export ccall "hat_hs_diagnostics"
  hsDiagnostics :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (CRepPtr Issues)

hsDiagnostics ctx_hs expr_hs = do
    ctx  <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (diagnosticsAnalysis expr)
    let (ctx_c) = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    passIssues ctx_c res

foreign import ccall "hat_c_mk_issue"
  mkCIssue :: CRepPtr NodeAddress -> CInt -> CInt -> CString -> IO (CRepPtr Issue)

foreign import ccall "hat_c_mk_issues"
  mkCIssues :: CRepArr Issue -> CSize -> IO (CRepPtr Issues)

passIssues :: Ctx.CContext -> Issues -> IO (CRepPtr Issues)
passIssues ctx (Issues is) = do
    is_c <- forM is passIssue
    withArrayUnsignedLen is_c mkCIssues
  where
    passIssue :: Issue -> IO (CRepPtr Issue)
    passIssue (Issue t s c m) = do
        t_c <- passNodeAddress ctx t
        withCString m $ mkCIssue t_c (convertSeverity s) (convertCategory c)

    convertCategory :: Categroy -> CInt
    convertCategory Basic = 0

    convertSeverity :: Severity -> CInt
    convertSeverity Warning = 0
    convertSeverity Error = 1
