
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
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query

import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree




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
    IR.Literal -> Solver.mkVariable (idGen addr) [] (compose $ ElementReferenceSet $ BSet.empty)

    -- default handling through data flow
    _ -> dataflowValue addr analysis opsHandler
    
  where

    analysis = mkDataFlowAnalysis ElementReferenceAnalysis "DI_ER" elementReferenceValue

    idGen = mkVarIdentifier analysis

    opsHandler = [ refElementHandler, refDeclHandler ]
    
    refElementHandler = OperatorHandler cov noDep val
      where
        cov a = isBuiltin a "data_item_element_access"
        val _ = compose $ ElementReferenceSet $ BSet.singleton $ ElementReference ref range
        ref   = getNode $ goDown 1 $ goDown 2 addr
        range = point $ getNode $ goDown 1 $ goDown 3 addr
    
    refDeclHandler = OperatorHandler cov noDep val
      where
        cov a = isBuiltin a "ref_decl"
        val _ = compose $ ElementReferenceSet $ BSet.empty
        
    noDep a = []
    
    compose = toComposed