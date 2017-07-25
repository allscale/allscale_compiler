{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Allscale.Analysis.Diagnostics where

import Debug.Trace

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Maybe
import Data.Set (Set)
import Data.Typeable (Typeable)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics (Generic)
import Insieme.Adapter (CRep,CSet,CRepPtr,CSetPtr,CRepArr,updateContext,passNodeAddress,withArrayUnsignedLen)
import Insieme.Analysis.Callable
import Insieme.Analysis.Entities.FieldIndex (SimpleFieldIndex)
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toComposed,toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Reference
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query

import qualified Data.Set as Set
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet

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

newtype Issues = Issues (Set Issue)
  deriving (Eq, Ord, Show, Generic, NFData)

instance Solver.Lattice Issues where
    bot = Issues Set.empty
    Issues x `merge` Issues y = Issues $ Set.union x y

mkOneIssue = Issues . Set.singleton

mkIssues = Issues . Set.fromList

-- * Analysis Dispatcher

-- TODO parameterize sub analysis
runDiagnostics :: Solver.SolverState -> NodeAddress -> (Issues, Solver.SolverState)
runDiagnostics s addr = (Solver.join issues, ns)
  where
    (issues, ns) = Solver.resolveAll s [allFineAnalysis addr, globalVariableDiagnosis addr]

-- * Generic Diagnosis

data Diagnosis = Diagnosis { varGen        :: NodeAddress -> Solver.TypedVar Issues
                           , analysis      :: Solver.AnalysisIdentifier
                           , opHandler     :: [OperatorHandler Issues]
                           , unknownTarget :: NodeAddress -> Issues
                           }

diagnosis :: Diagnosis -> NodeAddress -> Solver.TypedVar Issues
diagnosis diag addr = case getNode addr of

    -- types have no issues
    n | isType n -> noIssues

    -- also literals have no issues
    IR.Node IR.Literal _ -> noIssues

    -- also variables have no issues
    IR.Node IR.Variable _ -> noIssues

    -- also lambda expressions have no issues
    IR.Node IR.LambdaExpr _ | not (isEntryPoint addr) -> noIssues

    -- for call expressions, we have to apply special rules
    IR.Node IR.CallExpr _ -> var
      where
        var = Solver.mkVariable varId cons Solver.bot
        cons = callTargetConstraint : childConstraints var

        callTargetConstraint = Solver.createConstraint dep val var
          where
            dep a = Solver.toVar callableVar : (localIssueVars a) ++ (Solver.toVar <$> callableBodyVars a)

            val a = Solver.join (callTargetIssues a : unknownTargetIssues a : localIssues a)

            -- get access to functions targeted by this call
            callableVar = callableValue $ goDown 1 addr
            callableVal a = toValue $ Solver.get a callableVar

            callableBodyVars a = case () of
                _ | BSet.isUniverse callTargets -> []
                _                               -> foldr go [] $ BSet.toList callTargets
                  where
                    go (Lambda addr) bs = (varGen diag $ goDown 2 addr) : bs
                    go _ bs = bs
              where
                callTargets = callableVal a

            -- aggregate data requirements of call targets
            callTargetIssues a = Solver.join $ Solver.get a <$> callableBodyVars a

            -- add data requirements in case of unknown call targets
            unknownTargetIssues a = case () of
                _ | BSet.isUniverse $ callableVal a -> unknownTarget diag $ goDown 1 addr
                _                                   -> Solver.bot

            getActiveOperators a = if BSet.isUniverse targets then [] else concatMap f (opHandler diag)
                where
                    targets = callableVal a
                    f o = mapMaybe go $ BSet.toList targets
                        where
                            go l = if covers o trg then Just (o,trg) else Nothing
                                where
                                    trg = toAddress l

            localIssueVars a = concat $ map go $ getActiveOperators a
                where
                    go (o,t) = dependsOn o t a

            localIssues a = map go $ getActiveOperators a
                where
                    go (o,t) = getValue o t a

    -- for everything else we aggregate the requirements of the child nodes
    _ -> var
      where
        var = Solver.mkVariable varId (childConstraints var) Solver.bot

  where
    -- the standard-child-aggregation constraints
    childConstraints var = map go $ getChildren addr
      where
        go addr = Solver.forward (varGen diag addr) var

    noIssues = Solver.mkVariable varId [] Solver.bot
    idGen = Solver.mkIdentifierFromExpression $ analysis diag
    varId = idGen addr


-- * Diagnoses

data DataRequirementAllFineAnalysis = DataRequirementAllFineAnalysis
  deriving (Typeable)

allFineAnalysis :: NodeAddress -> Solver.TypedVar Issues
allFineAnalysis addr = Solver.mkVariable (idGen addr) [] Solver.bot
  where
    analysis = Solver.mkAnalysisIdentifier DataRequirementAllFineAnalysis "DIAG_fine"
    idGen = Solver.mkIdentifierFromExpression analysis


data DataRequirementMorePylonsAnalysis = DataRequirementMorePylonsAnalysis
  deriving (Typeable)

morePylonsAnalysis :: NodeAddress -> Solver.TypedVar Issues
morePylonsAnalysis addr = Solver.mkVariable (idGen addr) []
                        $ mkOneIssue $ Issue addr Error Basic "You Must Construct Additional Pylons!"
  where
    analysis = Solver.mkAnalysisIdentifier DataRequirementMorePylonsAnalysis "DIAG_pylon"
    idGen = Solver.mkIdentifierFromExpression analysis


data GlobalVariableDiagnosis = GlobalVariableDiagnosis
  deriving (Typeable)

globalVariableDiagnosis :: NodeAddress -> Solver.TypedVar Issues
globalVariableDiagnosis addr = diagnosis diag addr
  where
    diag = Diagnosis globalVariableDiagnosis analysis ops ut
    analysis = Solver.mkAnalysisIdentifier GlobalVariableDiagnosis "DIAG_global"

    ut addr' = mkOneIssue $ Issue addr' Warning Basic "Call to unknown function"

    ops = [readHandler, writeHandler]

    readHandler = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "ref_deref"
        val _ = handleOp "Read"

    writeHandler = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "ref_assign"
        val _ = handleOp "Write"

    dep _ a = Solver.toVar <$> [referenceVar]

    referenceVar :: Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
    referenceVar = referenceValue $ goDown 1 $ goDown 2 addr

    referenceVal a = toValue $ Solver.get a referenceVar

    globalAccess (Reference l _) = IR.Literal == getNodeType l
    globalAccess _ = False

    handleOp access a = mkIssues $ if any globalAccess (BSet.toList $ referenceVal a)
                                   then [Issue addr Error Basic (access ++ " access to global")]
                                   else []

-- * FFI

foreign export ccall "hat_hs_diagnostics"
  hsDiagnostics :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (CRepPtr Issues)

hsDiagnostics ctx_hs addr_hs = do
    ctx  <- deRefStablePtr ctx_hs
    addr <- deRefStablePtr addr_hs
    let (res,ns) = runDiagnostics (Ctx.getSolverState ctx) addr
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
    is_c <- forM (Set.toList is) passIssue
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
