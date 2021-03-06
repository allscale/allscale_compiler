
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Allscale.Analysis.DataItemAccesses (
    DataItemAccesses(..),
    dataItemAccesses
) where


--import Debug.Trace

import Allscale.Analysis.DataItemElementReference hiding (range)
import Control.DeepSeq
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.Framework.ExecutionTree
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Solver as Solver



--
-- * Data Item Accesses Lattice
--

-- the set of accesses is universe, if there is some access to an unknown location
data DataItemAccesses = DataItemAccesses (BSet.UnboundSet NodeAddress)
    deriving (Eq, Show, Generic, NFData, Hashable)

instance Solver.Lattice DataItemAccesses where

    bot   = DataItemAccesses BSet.empty

    merge (DataItemAccesses a) (DataItemAccesses b) = DataItemAccesses $ BSet.union a b

    print (DataItemAccesses a) = "Accesses: " ++ (show a)

instance Solver.ExtLattice DataItemAccesses where
    top   = DataItemAccesses $ BSet.Universe


--
-- * Data Item Accesses analysis token
--

data DataItemAccessesAnalysis = DataItemAccessesAnalysis
    deriving (Typeable)


--
-- * Data Item Accesses variable and constraint generator
--

dataItemAccesses :: NodeAddress -> Solver.TypedVar DataItemAccesses
dataItemAccesses addr = executionTreeValue analysis addr
  where

    -- configure the underlying execution tree analysis
    analysis = (mkExecutionTreeAnalysis DataItemAccessesAnalysis "DI_Access" dataItemAccesses (const Solver.top) (const Solver.top)) {

        -- register analysis specific operator handler
        opHandler = [accessHandler],

        -- all unhandled operators have no effect
        unhandledOperatorHandler = \_ -> Solver.bot

    }

    -- an operator handler handling read/write accesses
    accessHandler = OperatorHandler cov dep val
      where
        cov a = any (Q.isBuiltin a) ["ref_deref","ref_assign"]
        dep _ _ = [Solver.toVar referenceVar]
        val _ a = accessVal a

        referenceVar = elementReferenceValue $ I.goDown 1 $ I.goDown 2 addr
        referenceVal a = toSet $ toValue $ Solver.get a referenceVar
          where
            toSet (ElementReferenceSet s) = s

        accessVal a = DataItemAccesses $ case refVals of
            BSet.Universe -> BSet.Universe
            _ | BSet.isUniverse refVals -> BSet.Universe
              | BSet.null refVals       -> BSet.empty
              | otherwise               -> BSet.singleton addr
          where
            refVals = referenceVal a

