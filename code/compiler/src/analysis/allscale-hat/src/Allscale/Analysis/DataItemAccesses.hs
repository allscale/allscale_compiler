
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
import GHC.Generics (Generic)
import Insieme.Analysis.Callable
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toValue)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query

import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.BoundSet as BSet


--
-- * Data Item Accesses Lattice
--

-- the set of accesses is universe, if there is some access to an unknown location
data DataItemAccesses = DataItemAccesses (BSet.UnboundSet NodeAddress)
    deriving (Eq,Ord,Show,Generic,NFData)

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
dataItemAccesses addr = case getNode addr of

    -- TODO: this is a modified copy from diagnoses; unify it in a common base analysis!

    -- types have no issues
    n | isType n -> noAccesses

    -- also literals have no issues
    IR.Node IR.Literal _ -> noAccesses

    -- also variables have no issues
    IR.Node IR.Variable _ -> noAccesses

    -- also lambda expressions have no issues
    IR.Node IR.LambdaExpr _ | not (isEntryPoint addr) -> noAccesses

    -- for call expressions, we have to apply special rules
    -- TODO: this is a modified copy of the data requirement analysis; unify it!
    IR.Node IR.CallExpr _ -> var
      where
        var = Solver.mkVariable varId cons Solver.bot
        
        cons = callTargetConstraint : childConstraints
        
        -- the standard-child-aggregation constraints 
        
        childConstraints = map go $ getChildren addr
          where
            go addr = Solver.forward (dataItemAccesses addr) var
        
        
        -- constraints considering the call targets
        
        callTargetConstraint = Solver.createConstraint dep val var
          where
            dep a = (Solver.toVar callableVar) : (Solver.toVar <$> referenceVars a) ++ (Solver.toVar <$> callableBodyVars a)
            val a = Solver.join [(callTargetRequirements a),(localAccess a),(unknownTargetRequirements a)]
            
            
            -- get access to functions targeted by this call
            
            callableVar = callableValue $ goDown 1 addr  
            callableVal a = toValue $ Solver.get a callableVar  
            
            callableBodyVars a = case () of 
            
                _ | BSet.isUniverse callTargets -> [] 
                _                               -> foldr go [] $ BSet.toList callTargets
                    where
                                
                        go (Lambda addr) bs = (dataItemAccesses $ goDown 2 addr) : bs
                        
                        go _ bs = bs
                        
                
              where
                
                callTargets = callableVal a
                
                
            -- aggregate data item accesses of call targets 
            
            callTargetRequirements a = Solver.join $ Solver.get a <$> callableBodyVars a
            
            
            -- add data requirements in case of unknown call targets
            
            unknownTargetRequirements a = case () of 
                _ | BSet.isUniverse $ callableVal a -> Solver.top
                _                                   -> Solver.bot
            
            
            -- also add data requirements if this call is targeting a ref_deref or ref_assign
            
            referenceVar = elementReferenceValue $ goDown 1 $ goDown 2 addr
            referenceVal a = toSet $ toValue $ Solver.get a referenceVar
              where
                toSet (ElementReferenceSet s) = s
            
            referenceVars a = case () of
                _ | isRead a || isWrite a -> [referenceVar]
                _                         -> []
            
            localAccess a = if isRead a || isWrite a then val else Solver.bot
              where
                val = DataItemAccesses $ case refVals of
                    BSet.Universe -> BSet.Universe
                    _ | BSet.isUniverse refVals -> BSet.Universe
                      | BSet.null refVals       -> BSet.empty
                      | otherwise               -> BSet.singleton addr

                refVals = referenceVal a

            -- utilities
            
            mayCall a lit = case () of 
                _ | BSet.isUniverse targets -> False
                _                           -> any ((\n -> isBuiltin n lit) . toAddress) $ BSet.toList targets
              where
                targets = callableVal a
                
            isRead a = mayCall a "ref_deref"
            
            isWrite a = mayCall a "ref_assign"

    -- for everything else we aggregate the requirements of the child nodes
    _ -> var
      where
        var = Solver.mkVariable varId (childConstraints var) Solver.bot

  where
    -- the standard-child-aggregation constraints
    childConstraints var = map go $ getChildren addr
      where
        go addr = Solver.forward (dataItemAccesses addr) var

    noAccesses = Solver.mkVariable varId [] Solver.bot
    analysis = Solver.mkAnalysisIdentifier DataItemAccessesAnalysis "DI_Accesses"
    idGen = Solver.mkIdentifierFromExpression analysis
    varId = idGen addr

