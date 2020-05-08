module Futhark.CodeGen.ImpGen.Multicore.Base
 ( toParam
 , compileKBody
 , extractAllocations
 , compileThreadResult
 , MulticoreGen
 , getNumThreads
 , getNumThreads'
 , decideScheduling
 , sUnpauseProfiling
 )
 where

import Data.List
import Data.Bifunctor
import Prelude hiding (quot, rem)
import Futhark.Error
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Representation.MCMem

type MulticoreGen = ImpM MCMem () Imp.Multicore


toParam :: VName -> TypeBase shape u -> Imp.Param
toParam name (Prim pt)   = Imp.ScalarParam name pt
toParam name (Mem space) = Imp.MemParam name space
toParam _     Array{}    = error "Cannot make Array into Imp.Param"


compileKBody :: KernelBody MCMem
             -> ([(SubExp, [Imp.Exp])] -> ImpM MCMem () Imp.Multicore ())
             -> ImpM MCMem () Imp.Multicore ()
compileKBody kbody red_cont =
  compileStms (freeIn $ kernelBodyResult kbody) (kernelBodyStms kbody) $ do
    let red_res = kernelBodyResult kbody
    red_cont $ zip (map kernelResultSubExp red_res) $ repeat []



compileThreadResult :: SegSpace
                    -> PatElem MCMem -> KernelResult
                    -> MulticoreGen ()
compileThreadResult space pe (Returns _ what) = do
  let is = map (Imp.vi32 . fst) $ unSegSpace space
  copyDWIMFix (patElemName pe) is what []

compileThreadResult _ _ ConcatReturns{} =
  compilerBugS "compileThreadResult: ConcatReturn nunhandled."
compileThreadResult _ _ WriteReturns{} =
  compilerBugS "compileThreadResult: WriteReturns nunhandled."

compileThreadResult _ _ TileReturns{} =
  compilerBugS "compileThreadResult: TileReturns unhandled."


getNumThreads' :: VName -> MulticoreGen ()
getNumThreads' dest =
  emit $ Imp.Op $ Imp.MulticoreCall [dest] "futhark_context_get_num_threads"

getNumThreads :: MulticoreGen VName
getNumThreads = do
  v <- dPrim "num_threads" (IntType Int32)
  getNumThreads' v
  return v


isLoadBalanced :: Imp.Code -> Bool
isLoadBalanced (a Imp.:>>: b)    = isLoadBalanced a && isLoadBalanced b
isLoadBalanced (Imp.For _ _ _ a) = isLoadBalanced a
isLoadBalanced (Imp.If _ a b)    = isLoadBalanced a && isLoadBalanced b
isLoadBalanced (Imp.Comment _ a) = isLoadBalanced a
isLoadBalanced Imp.While{}       = False
isLoadBalanced _                 = True



decideScheduling :: Imp.Code -> Imp.Scheduling
decideScheduling code  =
  if isLoadBalanced code then
    Imp.Static
  else
    Imp.Dynamic 10

sUnpauseProfiling :: MulticoreGen ()
sUnpauseProfiling =
  emit $ Imp.Op $ Imp.MulticoreCall [] "futhark_context_unpause_profiling"

-- | Try to extract invariant allocations.  If we assume that the
-- given 'Code' is the body of a 'SegOp', then it is always safe to
-- move the allocations to the prebody.
extractAllocations :: Imp.Code -> (Imp.Code, Imp.Code)
extractAllocations segop_code = f segop_code
  where declared = Imp.declaredIn segop_code
        f (Imp.DeclareMem name space) =
          -- Hoisting declarations out is always safe.
          (Imp.DeclareMem name space, mempty)
        f (Imp.Allocate name size space)
          | not $ freeIn size `namesIntersect` declared =
              (Imp.Allocate name size space, mempty)
        f (x Imp.:>>: y) = f x <> f y
        f (Imp.While cond body) =
          second (Imp.While cond) (f body)
        f (Imp.For i it bound body) =
          second (Imp.For i it bound) (f body)
        f (Imp.Comment s code) =
          second (Imp.Comment s) (f code)
        f Imp.Free{} =
          mempty
        f (Imp.If cond tcode fcode) =
          let (ta, tcode') = f tcode
              (fa, fcode') = f fcode
          in (ta <> fa, Imp.If cond tcode' fcode')
        f (Imp.Op (Imp.ParLoop sched ntask i e
                   (Imp.MulticoreFunc free prebody body n))) =
          let (body_allocs, body') = extractAllocations body
              (free_allocs, here_allocs) = f body_allocs
              free' = filter (not .
                              (`nameIn` Imp.declaredIn body_allocs) .
                              Imp.paramName) free
          in (free_allocs,
              here_allocs <>
              Imp.Op (Imp.ParLoop sched ntask i e $
                      Imp.MulticoreFunc free' prebody body' n))
        f code =
          (mempty, code)