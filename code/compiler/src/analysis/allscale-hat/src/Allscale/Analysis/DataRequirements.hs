
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Analysis.DataRequirements where

import Allscale.Analysis.Entities.DataRange
import Control.DeepSeq
import Control.Monad
import Data.Foldable (or)
import Data.List
import Data.Maybe
import Data.Typeable
import Debug.Trace
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics (Generic)
import Insieme.Adapter (CRep,CSet,CRepPtr,CSetPtr,CRepArr,passBoundSet,updateContext)
import Insieme.Analysis.Arithmetic (arithmeticValue,SymbolicFormulaSet)
import Insieme.Analysis.Callable
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.SymbolicFormula (SymbolicFormula)
import Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toComposed,toValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Inspire.BinaryDumper (dumpBinaryDump)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import Insieme.Inspire.Visit

import qualified Data.ByteString as BS
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

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
dataRequirements addr = case getNode addr of

    -- types have no data requirements
    n | isType n -> noRequirements
    
    -- also literals have no requirements
    IR.Node IR.Literal _ -> noRequirements 
    
    -- for call expressions, we have to apply special rules
    IR.Node IR.CallExpr _ -> var
      where
        var = Solver.mkVariable varId cons Solver.bot
        
        cons = callTargetConstraint : childConstraints
        
        -- the standard-child-aggregation constraints 
        
        childConstraints = map go $ getChildren addr
          where
            go addr = Solver.forward (dataRequirements addr) var
        
        
        -- constraints considering the call targets
        
        callTargetConstraint = Solver.createConstraint dep val var
          where
            dep a = (Solver.toVar callableVar) : (Solver.toVar <$> callableBodyVars a)
            val a = Solver.join [(callTargetRequirements a),(localAccess a),(unknownTargetRequirements a)]
            
            
            -- get access to functions targeted by this call
            
            callableVar = callableValue $ goDown 1 addr  
            callableVal a = toValue $ Solver.get a callableVar  
            
            callableBodyVars a = case () of 
            
                _ | BSet.isUniverse callTargets -> [] 
                _                               -> foldr go [] $ BSet.toList callTargets
                    where
                                
                        go (Lambda addr) bs = (dataRequirements $ goDown 2 addr) : bs
                        
                        go _ bs = bs
                        
                
              where
                
                callTargets = callableVal a
                
                
            -- compute data requirements of call targets 
            
            callTargetRequirements a = Solver.join $ Solver.get a <$> callableBodyVars a
            
            
            -- add data requirements in case of unknown call targets
            
            unknownTargetRequirements a = case () of 
                _ | BSet.isUniverse $ callableVal a -> Solver.top
                _                                   -> Solver.bot
            
            
            -- also add data requirements if this call is targeting a ref_deref or ref_assign
            
            localAccess a = Solver.join [readAccess,writeAccess]
              where
                
                targets = callableVal a
                
                contains lit = case () of 
                    _ | BSet.isUniverse targets -> False
                    _                           -> any ((\n -> isBuiltin n lit) . toAddress) $ BSet.toList targets
                
                readAccess = case () of
                    _ | contains "ref_deref" -> requirements ReadOnly
                    _                        -> Solver.bot

                writeAccess = case () of
                    _ | contains "ref_assign" -> requirements ReadWrite
                    _                         -> Solver.bot
            
                requirements mode = DataRequirements $ BSet.singleton $ requirement mode
            
                requirement mode = DataRequirement {
                            dataItemRef = getNode addr,
                            range       = point $ getNode addr,
                            accessMode  = mode
                    }
            
    
    -- for everything else we aggregate the requirements of the child nodes
    _ -> var
      where
        var = Solver.mkVariable varId cons Solver.bot
        
        cons = map go $ getChildren addr
          where
            go addr = Solver.forward (dataRequirements addr) var  
        
  where

    noRequirements = Solver.mkVariable varId [] Solver.bot

    analysis = Solver.mkAnalysisIdentifier DataRequirementAnalysis "DR"

    idGen = Solver.mkIdentifierFromExpression analysis
    
    varId = idGen addr





-- * FFI

foreign export ccall "hat_hs_data_requirements"
  hsDataRequirements :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (CRepPtr DataRequirements)

hsDataRequirements ctx_hs stmt_hs = do
    ctx  <- deRefStablePtr ctx_hs
    stmt <- deRefStablePtr stmt_hs
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (dataRequirements stmt)
    let ctx_c =  Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    passDataRequirements ctx_c res

foreign import ccall "hat_c_mk_data_requirement"
  mkCDataRequirement :: CRepPtr IR.Tree -> CRepPtr DataRange -> CInt -> IO (CRepPtr DataRequirement)

foreign import ccall "hat_c_mk_data_requirement_set"
  mkCDataRequirementSet :: CRepArr DataRequirement -> CLLong -> IO (CSetPtr DataRequirement)

foreign import ccall "hat_c_mk_data_requirements"
  mkCDataRequirements :: CSetPtr DataRequirement -> IO (CRepPtr DataRequirements)

passDataRequirements :: Ctx.CContext -> DataRequirements -> IO (CRepPtr DataRequirements)
passDataRequirements ctx (DataRequirements s) = do
    s_c <- passBoundSet passDataRequirement mkCDataRequirementSet s
    mkCDataRequirements s_c
  where
    passDataRequirement :: DataRequirement -> IO (CRepPtr DataRequirement)
    passDataRequirement (DataRequirement d r a) = do
        d_c <- BS.useAsCStringLen (dumpBinaryDump d) (mkCIrTree' ctx)
        r_c <- passDataRange ctx r
        mkCDataRequirement d_c r_c (convertAccessMode a)

    mkCIrTree' :: Ctx.CContext -> CStringLen -> IO (CRepPtr IR.Tree)
    mkCIrTree' ctx (sz,s) = mkCIrTree ctx sz (fromIntegral s)

    convertAccessMode :: AccessMode -> CInt
    convertAccessMode ReadOnly = 0
    convertAccessMode ReadWrite = 1
