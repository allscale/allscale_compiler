
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Analysis.DataRequirements where

--import Debug.Trace

import Allscale.Analysis.Entities.DataRange
import Allscale.Analysis.DataItemElementReference hiding (range)
import Control.DeepSeq
import Control.Exception (bracket)
import Data.List
import Data.Typeable
import Foreign
import Foreign.C.Types
import GHC.Generics (Generic)
import Insieme.Adapter (AnalysisResultPtr,CRepPtr,CSetPtr,CRepArr,allocAnalysisResult,dumpIrTree,delCIrTree,getTimelimit,passBoundSet,pprintTree)
import Insieme.Analysis.Callable
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toValue)
import Insieme.Analysis.SymbolicValue (SymbolicValueSet(..), symbolicValue)
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import System.Timeout (timeout)

import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR
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
dataRequirements addr = case getNode addr of

    -- types have no data requirements
    n | isType n -> noRequirements
    
    -- also literals have no requirements
    IR.Node IR.Literal _ -> noRequirements 
    
    -- also variables have no requirements
    IR.Node IR.Variable _ -> noRequirements 
    
    -- also lambda expressions have no data requirements
    IR.Node IR.LambdaExpr _ -> noRequirements
    
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
            dep a = (Solver.toVar callableVar) : (Solver.toVar <$> referenceVars a) ++ (Solver.toVar <$> callableBodyVars a)
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
                
                
            -- aggregate data requirements of call targets 
            
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
            
            
            localAccess a = Solver.join [readAccess,writeAccess]
              where
                
                readAccess = case () of
                    _ | isRead a -> requirements ReadOnly
                    _                        -> Solver.bot

                writeAccess = case () of
                    _ | isWrite a -> requirements ReadWrite
                    _                         -> Solver.bot
            
                requirements mode = DataRequirements $ BSet.map go $ referenceVal a
                  where
                    go (ElementReference ref range) = DataRequirement {
                                dataItemRef = ref,
                                range       = range,
                                accessMode  = mode
                        } 
            
            
            -- utilities
            
            mayCall a lit = case () of 
                _ | BSet.isUniverse targets -> False
                _                           -> any ((\n -> isBuiltin n lit) . toAddress) $ BSet.toList targets
              where
                targets = callableVal a
                
            isRead a = mayCall a "ref_deref"
            
            isWrite a = mayCall a "ref_assign"
            
            
    -- for for-loops, iterator bounds need to be included  
    IR.Node IR.ForStmt _ -> var
      where
        var = Solver.mkVariable varId [con] Solver.bot
        con = Solver.createConstraint dep val var
        
        dep _ = ( Solver.toVar <$> [beginDepVar,endDepVar,stepDepVar,bodyDepVar] )
             ++ ( Solver.toVar <$> [beginValVar,endValVar] )
        
        val a = Solver.join [beginDepVal a,endDepVal a,stepDepVal a,bodyDepVal a]
        
        -- get the addresses of sub-elements
        iter  = getNode $ goDown 1 $ goDown 0 addr
        begin = goDown 1 $ goDown 0 $ goDown 0 addr
        end   = goDown 1 addr
        step  = goDown 2 addr
        body  = goDown 3 addr
        
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
  hsDataRequirements :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (AnalysisResultPtr (CRepPtr DataRequirements))

hsDataRequirements :: StablePtr Ctx.Context -> StablePtr NodeAddress -> IO (AnalysisResultPtr (CRepPtr DataRequirements))
hsDataRequirements ctx_hs stmt_hs = do
    ctx  <- deRefStablePtr ctx_hs
    stmt <- deRefStablePtr stmt_hs
    timelimit <- fromIntegral <$> getTimelimit (Ctx.getCContext ctx)
    let (res,ns) = Solver.resolve (Ctx.getSolverState ctx) (dataRequirements stmt)
    let ctx_c =  Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    result <- timeout timelimit $ passDataRequirements ctx_c res
    case result of
        Just r  -> allocAnalysisResult ctx_nhs False r
        Nothing -> allocAnalysisResult ctx_hs  True =<< passDataRequirements ctx_c Solver.top

foreign import ccall "hat_c_mk_data_requirement"
  mkCDataRequirement :: CRepPtr IR.Tree -> CRepPtr DataRange -> CInt -> IO (CRepPtr DataRequirement)

foreign import ccall "hat_c_mk_data_requirement_set"
  mkCDataRequirementSet :: CRepArr DataRequirement -> CLLong -> IO (CSetPtr DataRequirement)

foreign import ccall "hat_c_del_data_requirement_set"
  delCDataRequirementSet :: CSetPtr DataRequirement -> IO ()

foreign import ccall "hat_c_mk_data_requirements"
  mkCDataRequirements :: CSetPtr DataRequirement -> IO (CRepPtr DataRequirements)

passDataRequirements :: Ctx.CContext -> DataRequirements -> IO (CRepPtr DataRequirements)
passDataRequirements ctx (DataRequirements s) = bracket
    (passBoundSet passDataRequirement mkCDataRequirementSet s)
    (delCDataRequirementSet)
    (mkCDataRequirements)
  where

    passDataRequirement :: DataRequirement -> IO (CRepPtr DataRequirement)
    passDataRequirement (DataRequirement d r a) = bracket
        ((,) <$> dumpIrTree ctx d <*> passDataRange ctx r)
        (\(d_c, r_c) -> delCIrTree d_c >> delCDataRange r_c)
        (\(d_c, r_c) -> mkCDataRequirement d_c r_c (convertAccessMode a))

    convertAccessMode :: AccessMode -> CInt
    convertAccessMode ReadOnly = 0
    convertAccessMode ReadWrite = 1
