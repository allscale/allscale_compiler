
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Analysis.DataRequirements where

-- import Debug.Trace

import Allscale.Analysis.Entities.DataRange
import Allscale.Analysis.DataItemElementReference hiding (range)
import Control.DeepSeq
import Data.List
import Data.Typeable
import GHC.Generics (Generic)

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.BoundSet as BSet
import Insieme.Adapter.Utils (pprintTree)

import Insieme.Analysis.Framework.ExecutionTree
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.SymbolicValue (SymbolicValueSet(..), symbolicValue)
import qualified Insieme.Analysis.Solver as Solver



--
-- * Access Modes
--

data AccessMode = ReadOnly
                | ReadWrite
    deriving (Eq,Ord,Show,Generic,NFData)


--
-- * Data Requirements
--

data DataRequirement = DataRequirement {
                            dataItemRef :: I.Tree,
                            range       :: DataRange,
                            accessMode  :: AccessMode
                        }
    deriving (Eq,Ord,Show,Generic,NFData)


printRequirement :: DataRequirement -> String
printRequirement (DataRequirement i r a) = "Requirement{" ++ (pprintTree i) ++ "," ++ (printRange r) ++ "," ++ (show a) ++ "}"

--
-- * Data Requirements Lattice
--

data DataRequirements = DataRequirements (BSet.UnboundSet DataRequirement)
    deriving (Eq,Ord,Show,Generic,NFData)

instance Solver.Lattice DataRequirements where
    bot   = DataRequirements $ BSet.empty
    merge (DataRequirements a) (DataRequirements b) = DataRequirements $ BSet.union a b
    
    print (DataRequirements BSet.Universe) = "Universe"
    print (DataRequirements b)             = "{" ++ (intercalate "," $ printRequirement <$> BSet.toList b) ++ "}"

instance Solver.ExtLattice DataRequirements where
    top   = DataRequirements $ BSet.Universe


--
-- * Data Requirements analysis token
--

data DataRequirementAnalysis = DataRequirementAnalysis
    deriving (Typeable)


--
-- * Data Requirements variable and constraint generator
--

dataRequirements :: NodeAddress -> Solver.TypedVar DataRequirements
dataRequirements addr = case I.getNode addr of

    -- special case: for for-loops, iterator bounds need to be included
    I.Node I.ForStmt _ -> var
      where
        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val var

        dep _ = ( Solver.toVar <$> [beginDepVar,endDepVar,stepDepVar,bodyDepVar] )
             ++ ( Solver.toVar <$> [beginValVar,endValVar] )

        val a = Solver.join [beginDepVal a,endDepVal a,stepDepVal a,bodyDepVal a]

        -- get the addresses of sub-elements
        iter  = I.getNode $ I.goDown 1 $ I.goDown 0 addr
        begin = I.goDown 1 $ I.goDown 0 $ I.goDown 0 addr
        end   = I.goDown 1 addr
        step  = I.goDown 2 addr
        body  = I.goDown 3 addr

        -- get data requirement variables
        beginDepVar = dataRequirements begin
        endDepVar   = dataRequirements end
        stepDepVar  = dataRequirements step
        bodyDepVar  = dataRequirements body

        -- get the symbolic value variables for the iterators
        beginValVar = symbolicValue begin
        endValVar   = symbolicValue end

        -- get the data requirement values
        beginDepVal a = Solver.get a beginDepVar
        endDepVal   a = Solver.get a endDepVar
        stepDepVal  a = Solver.get a stepDepVar

        bodyDepVal  a = DataRequirements val
          where

            DataRequirements bodyRequirements  = (Solver.get a bodyDepVar)

            val = if (BSet.isUniverse bodyRequirements) || (BSet.isUniverse fromVals) || (BSet.isUniverse toVals)  
                  then bodyRequirements 
                  else BSet.fromList $ concat (fixRange <$> BSet.toList bodyRequirements )  

            fixRange req = [ req { range = defineIteratorRange iter f t (range req) } | f <- BSet.toList fromVals , t <- BSet.toList toVals ]

            fromVals = unSVS $ toValue $ Solver.get a beginValVar
            toVals   = unSVS $ toValue $ Solver.get a endValVar


    -- default handling through execution tree value analysis 
    _ -> executionTreeValue analysis addr

  where

    -- configure the underlying execution tree analysis
    analysis = (mkExecutionTreeAnalysis DataRequirementAnalysis "DR" dataRequirements) {

        -- register analysis specific operator handler
        opHandler = [accessHandler],

        -- all unhandled operators cause no data requirements
        unhandledOperatorHandler = const Solver.bot

    }

    -- an operator handler handling read/write accesses
    accessHandler = OperatorHandler cov dep val
      where
        cov a = any (Q.isBuiltin a) ["ref_deref","ref_assign"]
        dep _ _ = [Solver.toVar referenceVar]
        val = dataRequirements

        referenceVar = elementReferenceValue $ I.goDown 1 $ I.goDown 2 addr
        referenceVal a = toSet $ toValue $ Solver.get a referenceVar
          where
            toSet (ElementReferenceSet s) = s

        dataRequirements o a = DataRequirements $ BSet.map go $ referenceVal a
          where
            go (ElementReference ref range) = DataRequirement {
                dataItemRef = ref,
                range       = range,
                accessMode  = mode
            }

            mode = if Q.isBuiltin o "ref_deref" then ReadOnly else ReadWrite

    -- utilities --
    idGen = Solver.mkIdentifierFromExpression $ analysisIdentifier analysis
    varId = idGen addr

