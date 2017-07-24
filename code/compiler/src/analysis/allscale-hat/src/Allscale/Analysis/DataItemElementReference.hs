
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
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toComposed,toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.SymbolicValue (SymbolicValueLattice,symbolicValue,genericSymbolicValue)
import Insieme.Analysis.Reference (isMaterializingDeclaration,isMaterializingCall)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query

import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet
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
        
        val = toComposed $ BSet.singleton $ getNode iter 
     


--
-- * Data Item Element Reference
--

data ElementReference = ElementReference {
            reference :: IR.Tree,
            range     :: DataRange
        } 
    deriving (Eq,Ord,Generic,NFData)

instance Show ElementReference where
    show (ElementReference _ r) = "ElementReference{some-ir-tree," ++ (show r) ++ "}"

--
-- * A lattice for the data item reference analysis
--

data ElementReferenceSet = ElementReferenceSet (BSet.UnboundSet ElementReference)
    deriving (Eq,Ord,Show,Generic,NFData)

instance Solver.Lattice ElementReferenceSet where
    bot   = ElementReferenceSet $ BSet.empty
    merge (ElementReferenceSet a) (ElementReferenceSet b) = ElementReferenceSet $ BSet.union a b

instance Solver.ExtLattice ElementReferenceSet where
    top   = ElementReferenceSet $ BSet.Universe


--
-- * Element Reference Analysis token
--

data ElementReferenceAnalysis = ElementReferenceAnalysis
    deriving (Typeable)


--
-- * Element Reference Variable Generator
--

elementReferenceValue :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex ElementReferenceSet)
elementReferenceValue addr = case getNodeType addr of

    -- literals do not produce element references
    IR.Literal -> noReference

    -- materializing declarations do not produce element references
    IR.Declaration | isMaterializingDeclaration (getNode addr) -> noReference

    -- materializing calls do not produce element references
    IR.CallExpr | isMaterializingCall (getNode addr) -> noReference

    -- default handling through data flow
    _ -> dataflowValue addr analysis opsHandler
    
  where

    noReference = Solver.mkVariable (idGen addr) [] (compose $ ElementReferenceSet $ BSet.empty)

    analysis = mkDataFlowAnalysis ElementReferenceAnalysis "DataItem_ElemRef" elementReferenceValue

    idGen = mkVarIdentifier analysis

    opsHandler = [ refElementHandler, refDeclHandler ]
    
    refElementHandler = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "data_item_element_access"
        dep _ _ = Solver.toVar <$> [refVar,rangeVar] 
        val _ a = compose $ ElementReferenceSet $ BSet.map go $ BSet.cartProduct (refVal a) (rangeVal a)
          where
            go (a,b) = ElementReference a (point b)
        
        refVar = symbolicValue $ goDown 1 $ goDown 2 addr
        refVal a = BSet.changeBound $ toValue $ Solver.get a refVar
        
        rangeVar = symbolicValueWithIterators $ goDown 1 $ goDown 3 addr
        rangeVal a = BSet.changeBound $ toValue $ Solver.get a rangeVar
    
    refDeclHandler = OperatorHandler cov noDep val
      where
        cov a = any (isBuiltin a) [ "ref_decl", "ref_alloc" ]
        val _ _ = compose $ ElementReferenceSet $ BSet.empty
        
    noDep _ _ = []
    
    compose = toComposed