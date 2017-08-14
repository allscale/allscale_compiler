{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Allscale.Analysis.Diagnostics where

--import Debug.Trace

import Control.DeepSeq (NFData)
import Data.Maybe
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Insieme.Analysis.Callable
import Insieme.Analysis.Entities.FieldIndex (SimpleFieldIndex)
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Reference
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query

import qualified Data.Set as Set
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Solver as Solver
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

mkOneIssue :: Issue -> Issues
mkOneIssue = Issues . Set.singleton

mkIssues :: [Issue] -> Issues
mkIssues = Issues . Set.fromList

-- * Analysis Dispatcher

runDiagnostics :: Solver.SolverState -> NodeAddress -> [DiagnosisFunction] -> (Issues, Solver.SolverState)
runDiagnostics s addr diags = (Solver.join issues, ns)
  where
    (issues, ns) = Solver.resolveAll s $ diags <*> pure addr

-- * Generic Diagnosis

type DiagnosisFunction = NodeAddress -> Solver.TypedVar Issues

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

data UnknownReferenceDiagnosis = UnknownReferenceDiagnosis
  deriving (Typeable)

unknownReferenceDiagnosis :: NodeAddress -> Solver.TypedVar Issues
unknownReferenceDiagnosis addr = diagnosis diag addr
  where
    diag = Diagnosis unknownReferenceDiagnosis analysis ops ut
    analysis = Solver.mkAnalysisIdentifier UnknownReferenceDiagnosis "DIAG_urd"

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

    dep _ _ = Solver.toVar <$> [referenceVar]

    referenceVar :: Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
    referenceVar = referenceValue $ goDown 1 $ goDown 2 addr

    referenceVal a = unRS $ toValue $ Solver.get a referenceVar

    isUnknown = BSet.isUniverse . referenceVal

    handleOp access a = mkIssues $ if isUnknown a
                                   then [Issue addr Error Basic (access ++ " access to unknown location")]
                                   else []


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

    dep _ _ = Solver.toVar <$> [referenceVar]

    referenceVar :: Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (ReferenceSet SimpleFieldIndex))
    referenceVar = referenceValue $ goDown 1 $ goDown 2 addr

    referenceVal a = unRS $ toValue $ Solver.get a referenceVar

    globalAccess (Reference l _) = IR.Literal == getNodeType l
    globalAccess _ = False

    handleOp _ a | BSet.isUniverse $ referenceVal a = Solver.bot
    handleOp access a = mkIssues $ if any globalAccess (BSet.toList $ referenceVal a)
                                   then [Issue addr Error Basic (access ++ " access to global")]
                                   else []
