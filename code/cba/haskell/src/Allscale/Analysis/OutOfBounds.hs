
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Allscale.Analysis.OutOfBounds (
    outOfBounds,
    elementCount,
    OutOfBoundsResult(..),
) where

import Control.DeepSeq
import Control.Monad
import Data.Foldable (or)
import Data.List
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

-- * Out Of Bounds

data OutOfBoundsResult = MayBeOutOfBounds
                       | IsNotOutOfBounds
                       | IsOutOfBounds
  deriving (Eq,Ord,Enum,Show,Generic,NFData)

outOfBounds :: Solver.SolverState -> NodeAddress -> (OutOfBoundsResult,Solver.SolverState)
outOfBounds init addr = (result,ns')
  where
    result = case () of
        _ | BSet.isUniverse arrayIndex -> MayBeOutOfBounds
          | BSet.isUniverse arraySize  -> MayBeOutOfBounds
          | or oobs                    -> IsOutOfBounds
          | otherwise                  -> IsNotOutOfBounds

    oobs = BSet.toList $ BSet.lift2 isOutOfBound arrayIndex arraySize

    isOutOfBound :: ArrayAccess -> SymbolicFormula -> Bool
    isOutOfBound (ArrayAccess i)    s = Ar.numCompare i s /= Ar.NumLT
    isOutOfBound InvalidArrayAccess _ = True

    (arrayIndex',ns) = Solver.resolve init
                     $ Ref.referenceValue
                     $ goDown 1 $ goDown 2 addr

    arrayIndex :: BSet.UnboundSet ArrayAccess
    arrayIndex = convertArrayIndex $ toValue arrayIndex'

    (arraySize',ns') = Solver.resolve ns
                     $ elementCount
                     $ goDown 1 $ goDown 2 addr

    arraySize :: SymbolicFormulaSet BSet.Unbound
    arraySize = BSet.changeBound $ toValue arraySize'

-- * Array Index

data ArrayAccess = ArrayAccess SymbolicFormula
                 | InvalidArrayAccess
  deriving (Eq,Ord,Show,Generic,NFData)

convertArrayIndex :: Ref.ReferenceSet SimpleFieldIndex -> BSet.UnboundSet ArrayAccess
convertArrayIndex = BSet.changeBound
                  . BSet.map (maybe InvalidArrayAccess ArrayAccess)
                  . BSet.map (toDataPath >=> toPath >=> toFormula)
  where
    toDataPath :: Ref.Reference i -> Maybe (DP.DataPath i)
    toDataPath (Ref.Reference _ dp) = Just dp
    toDataPath _                    = Nothing

    toPath :: DP.DataPath i -> Maybe [i]
    toPath dp | DP.isInvalid dp = Nothing
    toPath dp                   = Just $ DP.getPath dp

    toFormula :: [SimpleFieldIndex] -> Maybe SymbolicFormula
    toFormula []          = Just Ar.zero
    toFormula (Index i:_) = Just $ Ar.mkConst $ fromIntegral i
    toFormula _           = Nothing

-- * Element Count

data ElementCountAnalysis = ElementCountAnalysis
  deriving (Typeable)

elementCountAnalysis :: DataFlowAnalysis ElementCountAnalysis (ValueTree.Tree SimpleFieldIndex (SymbolicFormulaSet BSet.Bound10))
elementCountAnalysis = (mkDataFlowAnalysis ElementCountAnalysis "EC" elementCount)

elementCount :: NodeAddress -> Solver.TypedVar (ValueTree.Tree SimpleFieldIndex (SymbolicFormulaSet BSet.Bound10))
elementCount addr = dataflowValue addr elementCountAnalysis [refNull, noChange, creation, scalar]
  where
    refNull = OperatorHandler cov dep val
      where
        cov a = isBuiltin a "ref_null"
        dep _ = []
        val _ = toComposed $ BSet.singleton $ Ar.zero

    noChange = OperatorHandler cov dep val
      where
        cov a = any (isBuiltin a) ["ref_cast", "ref_reinterpret" , "ref_narrow", "ref_expand"]
        dep _ = [Solver.toVar baseRefVar]
        val a = Solver.get a baseRefVar

        baseRefVar   = elementCount $ goDown 1 $ goDown 2 addr

    creation = OperatorHandler cov dep val
      where
        cov a = any (isBuiltin a) ["ref_decl", "ref_new"] && isRefArray
        dep _ = [Solver.toVar arraySize]
        val a = Solver.get a $ arraySize

        arraySize = arithmeticValue $ goesDown [0,2,0,2,1,0] addr

    scalar = OperatorHandler cov dep val
      where
        cov a = any (isBuiltin a) ["ref_decl", "ref_new"] && not isRefArray
        dep _ = []
        val _ = toComposed $ BSet.singleton $ Ar.one

    isRefArray = maybeToBool $ isArrayType <$> (getReferencedType $ goDown 0 addr)

-- * Utility

maybeToBool :: Maybe Bool -> Bool
maybeToBool = Data.Foldable.or

goesDown :: [Int] -> NodeAddress -> NodeAddress
goesDown l = foldr1 (.) $ goDown <$> reverse l

tracePrefix :: Show a => String -> a -> a
tracePrefix prefix obj = traceShow (prefix ++ ": " ++ show obj) obj
