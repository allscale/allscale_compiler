
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Allscale.Analysis.OutOfBounds (
    outOfBounds,
    elementCount,
    OutOfBoundsResult(..),
) where

import Debug.Trace

import Control.DeepSeq
import Data.Foldable (or)
import Data.Typeable
import Data.Maybe
import GHC.Generics (Generic)

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import qualified Insieme.Query as Q
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.Arithmetic (arithmeticValue, SymbolicFormulaSet(..))
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.SymbolicFormula (SymbolicFormula)
import Insieme.Analysis.Framework.Dataflow
import Insieme.Analysis.Framework.PropertySpace.ComposedValue (toComposed, toValue, getElement, isValue)
import Insieme.Analysis.Framework.Utils.OperatorHandler
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Analysis.Framework.PropertySpace.ValueTree as ValueTree
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Solver as Solver

-- * Out Of Bounds

data OutOfBoundsResult = MayBeOutOfBounds
                       | IsNotOutOfBounds
                       | IsOutOfBounds
  deriving (Eq,Ord,Enum,Show,Generic,NFData)

outOfBounds :: Solver.SolverState -> NodeAddress -> (OutOfBoundsResult,Solver.SolverState)
outOfBounds init addr = (result,ns')
  where
    result = case () of
        _ | BSet.isUniverse arrayIndex                 -> MayBeOutOfBounds
          | BSet.null arrayIndex                       -> MayBeOutOfBounds
          | all isOutOfBound $ BSet.toList arrayIndex  -> IsOutOfBounds
          | any mayOutOfBound $ BSet.toList arrayIndex -> MayBeOutOfBounds
          | otherwise                                  -> IsNotOutOfBounds


    checkBounds :: Bool -> ArrayAccess -> Bool
    checkBounds unknown (ArrayAccess d i) = case getElement d arraySize of
        v | isValue v -> case sizes of
                BSet.Universe -> unknown
                _             -> any tooSmall $ BSet.toList sizes
              where
                sizes = unSFS $ toValue v
                tooSmall s = Ar.numCompare i s /= Ar.NumLT

        _ -> unknown

    checkBounds _       NotAnArrayAccess   = False
    checkBounds unknown UnknownIndexAccess = unknown

    isOutOfBound = checkBounds False
    mayOutOfBound = checkBounds True

    (targetRef',ns) = Solver.resolve init
                     $ Ref.referenceValue
                     $ I.goDown 1 $ I.goDown 2 addr

    arrayIndex :: BSet.UnboundSet ArrayAccess
    arrayIndex = convertArrayIndex $ toValue targetRef'

    (arraySize,ns') = Solver.resolve ns
                     $ elementCount
                     $ I.goDown 1 $ I.goDown 2 addr


-- * Array Index

data ArrayAccess = ArrayAccess (DP.DataPath SimpleFieldIndex) SymbolicFormula
                 | NotAnArrayAccess
                 | UnknownIndexAccess
  deriving (Eq,Ord,Show,Generic,NFData)

convertArrayIndex :: Ref.ReferenceSet SimpleFieldIndex -> BSet.UnboundSet ArrayAccess
convertArrayIndex = BSet.changeBound
                  . BSet.map (maybe NotAnArrayAccess toAccess)
                  . BSet.map toDataPath
                  . Ref.unRS
  where
    toDataPath :: Ref.Reference i -> Maybe (DP.DataPath i)
    toDataPath (Ref.Reference _ dp) = Just dp
    toDataPath _                    = Nothing

    toAccess :: DP.DataPath SimpleFieldIndex -> ArrayAccess
    toAccess  DP.Root                                = NotAnArrayAccess
    toAccess (DP.DataPath dp DP.Down (ArrayIndex i)) = ArrayAccess dp $ Ar.mkConst $ fromIntegral i
    toAccess (DP.DataPath _  DP.Down UnknownIndex)   = UnknownIndexAccess
    toAccess  _                                      = NotAnArrayAccess


-- * Element Count

data ElementCountAnalysis = ElementCountAnalysis
  deriving (Typeable)


type ArraySize = ValueTree.Tree SimpleFieldIndex (SymbolicFormulaSet BSet.Bound10)

elementCount :: NodeAddress -> Solver.TypedVar ArraySize
elementCount addr = case Q.getNodeType addr of

    I.Literal -> Solver.mkVariable id [] $ toComposed $ SymbolicFormulaSet $ BSet.singleton size
      where
        size = if isRefArray then s else Ar.one
          where
            s = Ar.mkConst $ fromIntegral $ Q.getArraySize $ fromJust $ Q.getReferencedType addr


    _ -> dataflowValue addr analysis [refNull, noChange, creation, scalar]

  where

    analysis = (mkDataFlowAnalysis ElementCountAnalysis "EC" elementCount) {
        entryPointParameterHandler = const $ Solver.mkVariable id [] Solver.bot
    }
    id = mkVarIdentifier analysis addr

    refNull = OperatorHandler cov dep val
      where
        cov a = Q.isBuiltin a "ref_null"
        dep _ _ = []
        val _ _ = toComposed $ SymbolicFormulaSet $ BSet.singleton $ Ar.zero

    noChange = OperatorHandler cov dep val
      where
        cov a = any (Q.isBuiltin a) ["ref_cast", "ref_reinterpret" , "ref_narrow", "ref_expand"]
        dep _ _ = [Solver.toVar baseRefVar]
        val _ a = Solver.get a baseRefVar

        baseRefVar = elementCount $ I.goDown 1 $ I.goDown 2 addr

    creation = OperatorHandler cov dep val
      where
        cov a = any (Q.isBuiltin a) ["ref_decl", "ref_new"] && isRefArray
        dep _ _ = [Solver.toVar arraySize]
        val _ a = Solver.get a $ arraySize

        arraySize = arithmeticValue $ goesDown [0,2,0,2,1,0] addr

    scalar = OperatorHandler cov dep val
      where
        cov a = any (Q.isBuiltin a) ["ref_decl", "ref_new"] && not isRefArray
        dep _ _ = []
        val _ _ = toComposed $ SymbolicFormulaSet $ BSet.singleton $ Ar.one

    isRefArray = maybeToBool $ Q.isArrayType <$> (Q.getReferencedType $ I.goDown 0 addr)

-- * Utility

maybeToBool :: Maybe Bool -> Bool
maybeToBool = Data.Foldable.or

goesDown :: [Int] -> NodeAddress -> NodeAddress
goesDown l = foldr1 (.) $ I.goDown <$> reverse l

tracePrefix :: Show a => String -> a -> a
tracePrefix prefix obj = traceShow (prefix ++ ": " ++ show obj) obj
