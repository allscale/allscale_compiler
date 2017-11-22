{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Allscale.Analysis.Diagnostics where

--import Debug.Trace

import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.Entities.FieldIndex (SimpleFieldIndex)
import Insieme.Analysis.Framework.ExecutionTree
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Reference
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver

import Allscale.Analysis.DataItemElementReference

-- * Issues

data Issue = Issue { target    :: NodeAddress
                   , errorCode :: ErrorCode
                   , message   :: String
                   }
  deriving (Eq, Ord, Show, Generic, NFData)

newtype Issues = Issues (Set Issue)
  deriving (Eq, Ord, Show, Generic, NFData)

instance Solver.Lattice Issues where
    bot = Issues Set.empty
    Issues x `merge` Issues y = Issues $ Set.union x y

instance Solver.ExtLattice Issues where
    top = undefined


mkOneIssue :: Issue -> Issues
mkOneIssue = Issues . Set.singleton

mkIssues :: [Issue] -> Issues
mkIssues = Issues . Set.fromList

-- * Error Codes

data ErrorCode = Timeout
               | CallToUnknownFunction
               | ReadAccessToUnknownLocation
               | WriteAccessToUnknownLocation
               | ReadAccessToGlobal
               | WriteAccessToGlobal
               | ReadAccessToPotentialDataItemElementReference
               | WriteAccessToPotentialDataItemElementReference
               | UnobtainableDataRequirement
               | ObtainedDataRequirement
               | ConvertParRegionToSharedMemoryParRuntimeCode
               | UnableToInstrumentVariantForDataItemAccessCheck
               | InstrumentedVariantForDataItemAccessCheck
               | CallToInvalidFunctionForDistributedMemory
               | InvalidUseOfGlobalForDistributedMemory
               | ValidForDistributedMemory
               | InvalidForDistributedMemory
               | RefOrPtrFoundInCaptureList
  deriving (Eq, Ord, Enum, Show, Read, Generic, NFData)

-- * Analysis Dispatcher

runDiagnostics :: Solver.SolverState -> NodeAddress -> [DiagnosisFunction] -> (Issues, Solver.SolverState)
runDiagnostics s addr diags = (Solver.join issues, ns)
  where
    (issues, ns) = Solver.resolveAll s $ diags <*> pure addr

-- * Generic Diagnosis

type DiagnosisFunction = NodeAddress -> Solver.TypedVar Issues

-- a configuration struct for the generic diagnosis interface
data Diagnosis a = Diagnosis { analysisToken        :: a
                             , analysisName         :: String
                             , varGen               :: NodeAddress -> Solver.TypedVar Issues
                             , operatorHandler      :: [OperatorHandler Issues]
                             }

-- a generic diagnosis implementation
diagnosis :: (Typeable a) => Diagnosis a -> NodeAddress -> Solver.TypedVar Issues
diagnosis diag addr = executionTreeValue analysis addr
  where

    -- configure the underlying execution tree analysis
    analysis = (mkExecutionTreeAnalysis (analysisToken diag) ("DIAG_" ++ analysisName diag) (varGen diag)) {

        -- register analysis specific operator handler
        opHandler = operatorHandler diag,

        -- all unhandled operators have no issues
        unhandledOperatorHandler = \_ -> Solver.bot,

        -- all diagnoses have problems with unknown targets
        unknownTargetHandler = \addr -> mkOneIssue $ Issue addr CallToUnknownFunction ""
    }


-- * Diagnoses

data UnknownReferenceDiagnosis = UnknownReferenceDiagnosis
  deriving (Typeable)

unknownReferenceDiagnosis :: NodeAddress -> Solver.TypedVar Issues
unknownReferenceDiagnosis addr = diagnosis diag addr
  where
    diag = Diagnosis UnknownReferenceDiagnosis "urd" unknownReferenceDiagnosis ops

    ops = [readHandler, writeHandler]

    readHandler = OperatorHandler cov dep val
      where
        cov a = Q.isBuiltin a "ref_deref"
        val _ = handleOp ReadAccessToUnknownLocation

    writeHandler = OperatorHandler cov dep val
      where
        cov a = Q.isBuiltin a "ref_assign"
        val _ = handleOp WriteAccessToUnknownLocation

    dep _ _ = Solver.toVar <$> [referenceVar]

    referenceVar :: Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
    referenceVar = referenceValue $ I.goDown 1 $ I.goDown 2 addr

    referenceVal a = unRS $ toValue $ Solver.get a referenceVar

    isUnknown = BSet.isUniverse . referenceVal

    handleOp err a = mkIssues $ if isUnknown a
                                then [Issue addr err ""]
                                else []

data GlobalVariableDiagnosis = GlobalVariableDiagnosis
  deriving (Typeable)

globalVariableDiagnosis :: NodeAddress -> Solver.TypedVar Issues
globalVariableDiagnosis addr = diagnosis diag addr
  where
    diag = Diagnosis UnknownReferenceDiagnosis "global" globalVariableDiagnosis ops

    ops = [readHandler, writeHandler]

    readHandler = OperatorHandler cov dep val
      where
        cov a = Q.isBuiltin a "ref_deref"
        val _ = handleOp ReadAccessToGlobal

    writeHandler = OperatorHandler cov dep val
      where
        cov a = Q.isBuiltin a "ref_assign"
        val _ = handleOp WriteAccessToGlobal

    dep _ _ = Solver.toVar <$> [referenceVar]

    referenceVar :: Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
    referenceVar = referenceValue $ I.goDown 1 $ I.goDown 2 addr

    referenceVal a = unRS $ toValue $ Solver.get a referenceVar

    globalAccess (Reference l _) = I.Literal == Q.getNodeType l
    globalAccess _ = False

    handleOp _ a | BSet.isUniverse $ referenceVal a = Solver.bot
    handleOp err a = mkIssues $ if any globalAccess (BSet.toList $ referenceVal a)
                                then [Issue addr err ""]
                                else []

data UncertainAccessDiagnosis = UncertainAccessDiagnosis
  deriving (Typeable)

uncertainAccessDiagnosis :: NodeAddress -> Solver.TypedVar Issues
uncertainAccessDiagnosis addr = diagnosis diag addr
  where
    diag = Diagnosis UnknownReferenceDiagnosis "uncAcc" uncertainAccessDiagnosis ops

    ops = [readHandler, writeHandler]

    readHandler = OperatorHandler cov dep val
      where
        cov a = Q.isBuiltin a "ref_deref"
        val _ = handleOp ReadAccessToPotentialDataItemElementReference

    writeHandler = OperatorHandler cov dep val
      where
        cov a = Q.isBuiltin a "ref_assign"
        val _ = handleOp WriteAccessToPotentialDataItemElementReference

    dep _ _ = [Solver.toVar diReferenceVar, Solver.toVar referenceVar]

    diReferenceVar :: Solver.TypedVar (ValueTree.Tree SimpleFieldIndex ElementReferenceSet)
    diReferenceVar = elementReferenceValue $ I.goDown 1 $ I.goDown 2 addr

    diReferenceVal a = unERS $ toValue $ Solver.get a diReferenceVar

    referenceVar :: Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
    referenceVar = referenceValue $ I.goDown 1 $ I.goDown 2 addr

    referenceVal a = unRS $ toValue $ Solver.get a referenceVar

    handleOp _   a | referenceValIsUnknown   a = mkIssues []
    handleOp err a | diReferenceValIsUnknown a = mkOneIssue $ Issue addr err ""
    handleOp _   _ = mkIssues []

    referenceValIsUnknown   = BSet.isUniverse . referenceVal
    diReferenceValIsUnknown = BSet.isUniverse . diReferenceVal
