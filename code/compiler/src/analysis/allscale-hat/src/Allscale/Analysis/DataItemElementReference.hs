
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Allscale.Analysis.DataItemElementReference (
    ElementReference(..),
    ElementReferenceSet(..),
    elementReferenceValue
) where

import Allscale.Analysis.Entities.DataRange
import Control.DeepSeq
import Data.Typeable
import GHC.Generics (Generic)

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Adapter.Utils (pprintTree)
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toComposed, toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.SymbolicValue (SymbolicValueSet(..), SymbolicValueLattice, symbolicValue, genericSymbolicValue)
import Insieme.Analysis.Reference (isMaterializingDeclaration, isMaterializingCall)
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree





--
-- * A symbolic value analysis for ranges, including loop iterators.
--

data SymbolicValueWithIteratorsAnalysis = SymbolicValueWithIteratorsAnalysis
    deriving (Typeable)

symbolicValueWithIterators :: NodeAddress -> Solver.TypedVar SymbolicValueLattice
symbolicValueWithIterators = genericSymbolicValue analysis
  where
    -- we just re-use the default version of the generic symbolic value analysis
    analysis = (mkDataFlowAnalysis SymbolicValueWithIteratorsAnalysis "SwI" symbolicValueWithIterators) {
        iteratorVariableHandler = iterVarHandler
    }

    iterVarHandler iter = Solver.mkVariable varId [] val
      where

        varId = mkVarIdentifier analysis iter

        val = toComposed $ SymbolicValueSet $ BSet.singleton $ I.node iter



--
-- * Data Item Element Reference
--

data ElementReference = ElementReference {
            reference :: I.Tree,
            range     :: DataRange
        }
    deriving (Eq,Ord,Show,Generic,NFData)

--
-- * A lattice for the data item reference analysis
--

data ElementReferenceSet = ElementReferenceSet { unERS :: BSet.UnboundSet ElementReference }
    deriving (Eq,Ord,Show,Generic,NFData)

instance Solver.Lattice ElementReferenceSet where
    bot   = ElementReferenceSet $ BSet.empty
    merge (ElementReferenceSet a) (ElementReferenceSet b) = ElementReferenceSet $ BSet.union a b
    print = printReferenceSet

instance Solver.ExtLattice ElementReferenceSet where
    top   = ElementReferenceSet $ BSet.Universe


printReferenceSet :: ElementReferenceSet -> String
printReferenceSet (ElementReferenceSet BSet.Universe) = "{ - all - }"
printReferenceSet (ElementReferenceSet set) = "{" ++ (concat $ go <$> BSet.toList set) ++ "}"
  where
    go (ElementReference ref range) = (pprintTree ref) ++ "[" ++ (printRange range) ++ "]"

--
-- * Element Reference Analysis token
--

data ElementReferenceAnalysis = ElementReferenceAnalysis
    deriving (Typeable)


--
-- * Element Reference Variable Generator
--

elementReferenceValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex ElementReferenceSet)
elementReferenceValue addr = case Q.getNodeType addr of

    -- literals do not produce element references
    I.Literal -> noReference

    -- materializing declarations do not produce element references
    I.Declaration | isMaterializingDeclaration (I.node addr) -> noReference

    -- materializing calls do not produce element references
    I.CallExpr | isMaterializingCall (I.node addr) -> noReference

    -- default handling through data flow
    _ -> dataflowValue addr analysis opsHandler

  where

    noReferenceGen a = Solver.mkVariable (idGen a) [] (compose $ ElementReferenceSet $ BSet.empty)
    noReference = noReferenceGen addr

    analysis = (mkDataFlowAnalysis ElementReferenceAnalysis "DataItem_ElemRef" elementReferenceValue) {
        freeVariableHandler = noReferenceGen,
        unknownOperatorHandler = const Solver.top       -- all unknown targets are considered non-reference sources
    }

    idGen = mkVarIdentifier analysis

    opsHandler = [ refElementHandler, refCreationHandler, refForwardHandler ]

    refElementHandler = OperatorHandler cov dep val
      where
        cov a = Q.isBuiltin a "data_item_element_access"
        dep _ _ = Solver.toVar <$> [refVar,rangeVar]
        val _ a = compose $ ElementReferenceSet $ BSet.map go $ BSet.cartProduct (refVal a) (rangeVal a)
          where
            go (a,b) = ElementReference a (point b)

        refVar = symbolicValue $ I.goDown 1 $ I.goDown 2 addr
        refVal a = BSet.changeBound $ unSVS $ toValue $ Solver.get a refVar

        rangeVar = symbolicValueWithIterators $ I.goDown 1 $ I.goDown 3 addr
        rangeVal a = BSet.changeBound $ unSVS $ toValue $ Solver.get a rangeVar

    refCreationHandler = OperatorHandler cov noDep val
      where
        cov a = any (Q.isBuiltin a) [ "ref_decl", "ref_alloc" ]
        val _ _ = compose $ ElementReferenceSet $ BSet.empty

    refForwardHandler = OperatorHandler cov dep val
      where
        cov a = any (Q.isBuiltin a) [ "ref_member_access", "ref_component_access", "ref_cast", "ref_narrow", "ref_expand"]
        dep _ _ = [Solver.toVar refVar]
        val _ a = Solver.get a refVar

        refVar = elementReferenceValue $ I.goDown 1 $ I.goDown 2 addr


    noDep _ _ = []

    compose = toComposed
