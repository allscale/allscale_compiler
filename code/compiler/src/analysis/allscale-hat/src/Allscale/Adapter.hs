{-# LANGUAGE ForeignFunctionInterface #-}

module Allscale.Adapter where

import Foreign
import Foreign.C.Types

import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire.NodeAddress as Addr

import qualified Allscale.Analysis.OutOfBounds as OOB

-- * Context

foreign import ccall "hat_update_context"
  updateContext :: Ctx.CContext -> StablePtr Ctx.Context -> IO ()

-- * Analyses

foreign export ccall "hat_out_of_bounds"
  outOfBounds :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt

outOfBounds :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt
outOfBounds ctx_hs expr_hs = do
    ctx  <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (result,ns) = OOB.outOfBounds (Ctx.getSolverState ctx) expr
    let ctx_c = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    return $ fromIntegral $ fromEnum result
