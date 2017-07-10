
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Allscale.Analysis.DataRequirements (
--    outOfBounds,
--    elementCount,
--    OutOfBoundsResult(..),
) where


import Control.DeepSeq
import Control.Monad
import Data.Foldable (or)
import Data.List
import Data.Set
import Data.Maybe
import Data.Typeable
import Debug.Trace
import GHC.Generics (Generic)
import Insieme.Analysis.Arithmetic (arithmeticValue,SymbolicFormulaSet)
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.SymbolicFormula (SymbolicFormula)
import Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toComposed,toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import Insieme.Inspire.Visit
import Insieme.Utils.ParseInt

import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Solver as Solver
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

