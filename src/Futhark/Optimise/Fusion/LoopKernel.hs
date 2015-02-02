module Futhark.Optimise.Fusion.LoopKernel
  ( FusedKer(..)
  , newKernel
  , inputs
  , setInputs
  , arrInputs
  , kernelType
  , transformOutput
  , attemptFusion
  , SOAC
  , SOACNest
  , MapNest
  )
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import qualified Data.HashSet as HS

import Data.Maybe
import Data.Monoid
import Data.List

import Futhark.Representation.Basic
import Futhark.Renamer (renameLambda)
import Futhark.MonadFreshNames
import qualified Futhark.Analysis.HORepresentation.SOAC as SOAC
import qualified Futhark.Analysis.HORepresentation.SOACNest as Nest
import qualified Futhark.Analysis.HORepresentation.MapNest as MapNest
import Futhark.Optimise.Fusion.TryFusion
import Futhark.Optimise.Fusion.Composing
import Futhark.Tools

type SOAC = SOAC.SOAC Basic
type SOACNest = Nest.SOACNest Basic
type MapNest = MapNest.MapNest Basic

-- XXX: This function is very gross.
transformOutput :: MonadFreshNames m =>
                   SOAC.ArrayTransforms -> [VName] -> SOAC
                -> m (Maybe (Binder Basic ()))
transformOutput ts names soac = do
  let (ctxnames,valnames) = splitAt (shapeContextSize soact) names
      descend ts' validents =
        case SOAC.viewf ts' of
          SOAC.EmptyF ->
            forM_ (zip valnames validents) $ \(k, valident) ->
            letBindNames [k] $
            PrimOp $ SubExp $ Var valident
          t SOAC.:< ts'' -> do
            let es = map (applyTransform t) validents
                mkPat ident = Pattern [Bindee ident ()]
            newIds <- forM (zip valnames $ concatMap primOpType es) $ \(k, opt) ->
              newIdent (baseString k) opt
            zipWithM_ letBind (map mkPat newIds) $ map PrimOp es
            descend ts'' newIds
  valnames' <- mapM (newVName . baseString) valnames
  case instantiateIdents (ctxnames<>valnames') soact of
    Nothing -> return Nothing
    Just (ctxidents,validents) -> return $ Just $ do
      e <- SOAC.toExp soac
      letBind_ (basicPattern $ ctxidents<>validents) e
      descend ts validents
  where soact = SOAC.typeOf soac

applyTransform :: SOAC.ArrayTransform -> Ident -> PrimOp
applyTransform (SOAC.Rearrange cs perm) v =
  Rearrange cs perm v
applyTransform (SOAC.Reshape cs shape) v =
  Reshape cs shape v
applyTransform (SOAC.ReshapeOuter cs shape) v =
  let shapes = reshapeOuter shape 1 v
  in Reshape cs shapes v
applyTransform (SOAC.ReshapeInner cs shape) v =
  let shapes = reshapeInner shape 1 v
  in Reshape cs shapes v
applyTransform (SOAC.Replicate n) v =
  Replicate n (Var v)

inputToOutput :: SOAC.Input -> Maybe (SOAC.ArrayTransform, SOAC.Input)
inputToOutput (SOAC.Input ts ia) =
  case SOAC.viewf ts of
    t SOAC.:< ts' -> Just (t, SOAC.Input ts' ia)
    SOAC.EmptyF   -> Nothing

data FusedKer = FusedKer {
    fsoac      :: SOAC
  -- ^ the SOAC expression, e.g., mapT( f(a,b), x, y )

  , inplace    :: Names
  -- ^ every kernel maintains a set of variables
  -- that alias vars used in in-place updates,
  -- such that fusion is prevented to move
  -- a use of an

  , fusedVars :: [Ident]
  -- ^ whether at least a fusion has been performed.

  , outputTransform :: SOAC.ArrayTransforms
  }
                deriving (Show)

newKernel :: SOAC -> FusedKer
newKernel soac =
  FusedKer { fsoac = soac
           , inplace = HS.empty
           , fusedVars = []
           , outputTransform = SOAC.noTransforms
           }

arrInputs :: FusedKer -> HS.HashSet Ident
arrInputs = HS.fromList . mapMaybe SOAC.inputArray . inputs

inputs :: FusedKer -> [SOAC.Input]
inputs = SOAC.inputs . fsoac

setInputs :: [SOAC.Input] -> FusedKer -> FusedKer
setInputs inps ker = ker { fsoac = inps `SOAC.setInputs` fsoac ker }

kernelType :: FusedKer -> [ExtType]
kernelType = SOAC.typeOf . fsoac

tryOptimizeSOAC :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
tryOptimizeSOAC outIds soac ker = do
  (soac', ots) <- optimizeSOAC Nothing soac
  let ker' = map (SOAC.addTransforms ots) (inputs ker) `setInputs` ker
      outIds' = fixOutputTypes outIds soac'
      ker'' = fixInputTypes outIds' ker'
  applyFusionRules outIds' soac' ker''

tryOptimizeKernel :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
tryOptimizeKernel outIds soac ker = do
  ker' <- optimizeKernel (Just outIds) ker
  applyFusionRules outIds soac ker'

tryExposeInputs :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
tryExposeInputs outIds soac ker = do
  (ker', ots) <- exposeInputs outIds ker
  if SOAC.nullTransforms ots
  then fuseSOACwithKer outIds soac ker'
  else do
    (soac', ots') <- pullOutputTransforms soac ots
    let outIds' = fixOutputTypes outIds soac'
        ker'' = fixInputTypes outIds' ker'
    if SOAC.nullTransforms ots'
    then applyFusionRules outIds' soac' ker''
    else fail "tryExposeInputs could not pull SOAC transforms"

fixOutputTypes :: [Ident] -> SOAC -> [Ident]
fixOutputTypes outIds soac =
  zipWith fixInputType outIds $ SOAC.typeOf soac
  where fixInputType outId t =
          let outId_t = identType outId
          in outId { identType = outId_t `setArrayShape`
                                 Shape (map fixDim $ extShapeDims $ arrayShape t)
                   }
        -- FIXME: filter sucks; get rid of it.
        fixDim (Ext {})   = error "existential size - how did we optimise a filter?"
        fixDim (Free dim) = dim

fixInputTypes :: [Ident] -> FusedKer -> FusedKer
fixInputTypes outIds ker =
  ker { fsoac = fixInputTypes' $ fsoac ker }
  where fixInputTypes' soac =
          map fixInputType (SOAC.inputs soac) `SOAC.setInputs` soac
        fixInputType (SOAC.Input ts (SOAC.Var v))
          | Just v' <- find ((==identName v) . identName) outIds =
            SOAC.Input ts $ SOAC.Var v'
        fixInputType inp = inp

applyFusionRules :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
applyFusionRules outIds soac ker =
  tryOptimizeSOAC outIds soac ker <|>
  tryOptimizeKernel outIds soac ker <|>
  tryExposeInputs outIds soac ker <|>
  fuseSOACwithKer outIds soac ker

attemptFusion :: MonadFreshNames m =>
                 [Ident] -> SOAC -> FusedKer -> m (Maybe FusedKer)
attemptFusion outIds soac ker =
  liftM removeUnusedParamsFromKer <$>
  tryFusion (applyFusionRules outIds soac ker)

removeUnusedParamsFromKer :: FusedKer -> FusedKer
removeUnusedParamsFromKer ker =
  case soac of
    SOAC.Map {}     -> ker { fsoac = soac' }
    SOAC.Redomap {} -> ker { fsoac = soac' }
    _                -> ker
  where soac = fsoac ker
        l = SOAC.lambda soac
        inps = SOAC.inputs soac
        (l', inps') = removeUnusedParams l inps
        soac' = l' `SOAC.setLambda`
                (inps' `SOAC.setInputs` soac)

removeUnusedParams :: Lambda -> [SOAC.Input] -> (Lambda, [SOAC.Input])
removeUnusedParams l inps =
  (l { lambdaParams = accParams ++ ps' }, inps')
  where allParams = lambdaParams l
        (accParams, arrParams) =
          splitAt (length allParams - length inps) allParams
        pInps = zip arrParams inps
        (ps', inps') = case (unzip $ filter (used . fst) pInps, pInps) of
                         (([], []), (p,inp):_) -> ([p], [inp])
                         ((ps_, inps_), _)     -> (ps_, inps_)
        used p = identName p `HS.member` freeVars
        freeVars = freeNamesInBody $ lambdaBody l

-- | Check that the consumer uses at least one output of the producer
-- unmodified.
mapFusionOK :: [Ident] -> FusedKer -> Bool
mapFusionOK outIds ker = any (`elem` inputs ker) (map SOAC.varInput outIds)

-- | Check that the input-array set of consumer is equal to the
-- output-array set of producer.  That is, a filter-producer can only
-- be fused with a filter or reduce-consumer if the consumer accepts
-- input from no other source, and consumes everything by the
-- producer.
filterFusionOK :: [Ident] -> FusedKer -> Bool
filterFusionOK outIds ker =
  case mapM SOAC.isVarInput $ inputs ker of
    Nothing       -> False
    Just inputIds -> all (`elem` outIds) inputIds &&
                     all (`elem` inputIds) outIds

-- | Check that the input-array set of consumer is contained in the
-- output-array set of producer.  That is, a filter-producer can only
-- be fused if the consumer accepts input from no other source.
filterFoldFusionOK :: [Ident] -> FusedKer -> Bool
filterFoldFusionOK outIds ker =
  case mapM SOAC.isVarInput $ inputs ker of
    Nothing       -> False
    Just inputIds -> all (`elem` outIds) inputIds

mapOrFilter :: SOAC -> Bool
mapOrFilter (SOAC.Filter {}) = True
mapOrFilter (SOAC.Map {})    = True
mapOrFilter _                 = False

fuseSOACwithKer :: [Ident] -> SOAC -> FusedKer -> TryFusion FusedKer
fuseSOACwithKer outIds soac1 ker = do
  -- We are fusing soac1 into soac2, i.e, the output of soac1 is going
  -- into soac2.
  let soac2 = fsoac ker
      cs1      = SOAC.certificates soac1
      cs2      = SOAC.certificates soac2
      inp1_arr = SOAC.inputs soac1
      inp2_arr = SOAC.inputs soac2
      lam1     = SOAC.lambda soac1
      lam2     = SOAC.lambda soac2
      success res_soac = do
        let fusedVars_new = fusedVars ker++outIds
        -- Avoid name duplication, because the producer lambda is not
        -- removed from the program until much later.
        uniq_lam <- renameLambda $ SOAC.lambda res_soac
        return $ ker { fsoac = uniq_lam `SOAC.setLambda` res_soac
                     , fusedVars = fusedVars_new
                     }
  outPairs <- forM outIds $ \outId -> do
                outId' <- newIdent' (++"_elem") outId
                return (outId,
                        outId' { identType = rowType $ identType outId' })
  case (soac2, soac1) of
    -- The Fusions that are semantically map fusions:
    (SOAC.Map {}, SOAC.Map    {})
      | mapFusionOK outIds ker -> do
      let (res_lam, new_inp) = fuseMaps lam1 inp1_arr outPairs lam2 inp2_arr
      success $ SOAC.Map (cs1++cs2) res_lam new_inp

    (SOAC.Redomap _ lam21 _ ne _, SOAC.Map {})
      | mapFusionOK outIds ker -> do
      let (res_lam, new_inp) = fuseMaps lam1 inp1_arr outPairs lam2 inp2_arr
      success $ SOAC.Redomap (cs1++cs2) lam21 res_lam ne new_inp

    -- The Fusions that are semantically filter fusions:
    (SOAC.Redomap _ lam21 _ nes _, SOAC.Filter {})
      | filterFoldFusionOK outIds ker-> do
      names <- replicateM (length $ lambdaReturnType lam21) $ newVName "check"
      let (res_lam, new_inp) = fuseFilterIntoFold lam1 inp1_arr outPairs lam2 inp2_arr names
      success $ SOAC.Redomap (cs1++cs2) lam21 res_lam nes new_inp

    (SOAC.Filter {}, SOAC.Filter {})
      | filterFusionOK outIds ker -> do
      name <- newVName "check"
      let (res_lam, new_inp) = fuseFilters lam1 inp1_arr outPairs lam2 inp2_arr name
      success $ SOAC.Filter (cs1++cs2) res_lam new_inp

    -- Nothing else worked, so mkLets try rewriting to redomap if
    -- possible.
    (SOAC.Reduce _ lam args, _) | mapOrFilter soac1 -> do
       let (ne, arrs) = unzip args
           soac2' = SOAC.Redomap (cs1++cs2) lam lam ne arrs
           ker'   = ker { fsoac = soac2'
                        }
       fuseSOACwithKer outIds soac1 ker'

    _ -> fail "Cannot fuse"

-- Here follows optimizations and transforms to expose fusability.

optimizeKernel :: Maybe [Ident] -> FusedKer -> TryFusion FusedKer
optimizeKernel inp ker = do
  (resNest, resTrans) <- optimizeSOACNest inp startNest startTrans
  soac <- Nest.toSOAC resNest
  return $ ker { fsoac = soac
               , outputTransform = resTrans
               }
  where startNest = Nest.fromSOAC $ fsoac ker
        startTrans = outputTransform ker

optimizeSOAC :: Maybe [Ident] -> SOAC -> TryFusion (SOAC, SOAC.ArrayTransforms)
optimizeSOAC inp soac = do
  (nest, ots) <- optimizeSOACNest inp (Nest.fromSOAC soac) SOAC.noTransforms
  soac' <- Nest.toSOAC nest
  return (soac', ots)

optimizeSOACNest :: Maybe [Ident] -> SOACNest -> SOAC.ArrayTransforms
                 -> TryFusion (SOACNest, SOAC.ArrayTransforms)
optimizeSOACNest inp soac os = do
  res <- foldM comb (False, soac, os) $ reverse optimizations
  case res of
    (False, _, _)      -> fail "No optimisation applied"
    (True, soac', os') -> return (soac', os')
  where comb (changed, soac', os') f = do
          (soac'', os'') <- f inp soac' os
          return (True, soac'', os'')
          <|> return (changed, soac', os')

type Optimization = Maybe [Ident] -> SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)

optimizations :: [Optimization]
optimizations = [iswim]

iswim :: Maybe [Ident] -> SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)
iswim _ nest ots
  | Nest.Scan cs1 (Nest.NewNest lvl nn) es <- Nest.operation nest,
    Nest.Map cs2 mb <- nn,
    Just es' <- mapM SOAC.inputFromSubExp es,
    Nest.Nesting paramIds mapArrs bndIds retTypes <- lvl,
    mapM (liftM identName . isVarInput) mapArrs == Just paramIds = do
    let newInputs = es' ++ map (SOAC.transposeInput 0 1) (Nest.inputs nest)
        inputTypes = map SOAC.inputType newInputs
        (accsizes, arrsizes) =
          splitAt (length es) $ map rowType inputTypes
        innerAccParams = zipWith Ident (take (length es) paramIds) accsizes
        innerArrParams = zipWith Ident (drop (length es) paramIds) arrsizes
    let innerScan = Nest.Scan cs2 mb (map Var innerAccParams)
        scanNest = Nest.Nesting {
                     Nest.nestingInputs = map SOAC.varInput innerArrParams
                   , Nest.nestingReturnType = zipWith setOuterSize retTypes $
                                              map (arraySize 0) arrsizes
                   , Nest.nestingResult = bndIds
                   , Nest.nestingParamNames = paramIds
                   }
        perm = case retTypes of []  -> []
                                t:_ -> transposeIndex 0 1 [0..arrayRank t]
        nest' = Nest.SOACNest
                newInputs
                (Nest.Map cs1 (Nest.NewNest scanNest innerScan))
    return (nest',
            ots SOAC.|> SOAC.Rearrange cs2 perm)
iswim _ _ _ = fail "ISWIM does not apply"

-- Now for fiddling with transpositions...

commonTransforms :: [Ident] -> [SOAC.Input]
                 -> (SOAC.ArrayTransforms, [SOAC.Input])
commonTransforms interesting inps = commonTransforms' inps'
  where inps' = [ (maybe False (`elem` interesting) $ SOAC.inputArray inp, inp)
                    | inp <- inps ]

commonTransforms' :: [(Bool, SOAC.Input)] -> (SOAC.ArrayTransforms, [SOAC.Input])
commonTransforms' inps =
  case foldM inspect (Nothing, []) inps of
    Just (Just mot, inps') -> first (mot SOAC.<|) $ commonTransforms' $ reverse inps'
    _                      -> (SOAC.noTransforms, map snd inps)
  where inspect (mot, prev) (True, inp) =
          case (mot, inputToOutput inp) of
           (Nothing,  Just (ot, inp'))  -> Just (Just ot, (True, inp') : prev)
           (Just ot1, Just (ot2, inp'))
             | ot1 == ot2 -> Just (Just ot2, (True, inp') : prev)
           _              -> Nothing
        inspect (mot, prev) inp = Just (mot,inp:prev)

mapDepth :: MapNest -> Int
mapDepth (MapNest.MapNest _ body levels _) =
  -- XXX: The restriction to pure nests is conservative, but we cannot
  -- know whether an arbitrary postbody is dependent on the exact size
  -- of the nesting result.
  min resDims (length levels) + 1
  where resDims = minDim $ case levels of
                    [] -> case body of Nest.Fun lam ->
                                         lambdaReturnType lam
                                       Nest.NewNest nest _ ->
                                         Nest.nestingReturnType nest
                    nest:_ -> MapNest.nestingReturnType nest
        minDim [] = 0
        minDim (t:ts) = foldl min (arrayRank t) $ map arrayRank ts

pullRearrange :: SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pullRearrange nest ots = do
  nest' <- liftMaybeNeedNames $ MapNest.fromSOACNest nest
  SOAC.Rearrange cs perm SOAC.:< ots' <- return $ SOAC.viewf ots
  if permuteReach perm <= mapDepth nest' then
    let -- Expand perm to cover the full extent of the input dimensionality
        perm' inp = perm ++ [length perm..SOAC.inputRank inp-1]
        addPerm inp = SOAC.addTransform (SOAC.Rearrange cs $ perm' inp) inp
        inputs' = map addPerm $ MapNest.inputs nest'
    in return (MapNest.toSOACNest $ inputs' `MapNest.setInputs` nest',
               ots')
  else fail "Cannot pull transpose"

pushRearrange :: [Ident] -> SOACNest -> SOAC.ArrayTransforms
              -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pushRearrange inpIds nest ots = do
  nest' <- liftMaybeNeedNames $ MapNest.fromSOACNest nest
  (perm, inputs') <- liftMaybe $ fixupInputs inpIds $ MapNest.inputs nest'
  if permuteReach perm <= mapDepth nest' then
    let invertRearrange = SOAC.Rearrange [] $ permuteInverse perm
    in return (MapNest.toSOACNest $
               inputs' `MapNest.setInputs`
               rearrangeReturnTypes nest' perm,
               ots SOAC.|> invertRearrange)
  else fail "Cannot push transpose"

rearrangeReturnTypes :: MapNest -> [Int] -> MapNest
rearrangeReturnTypes nest@(MapNest.MapNest cs body nestings inps) perm =
  MapNest.MapNest cs body
  (zipWith setReturnType nestings $
   drop 1 $ iterate (map rowType) ts)
  inps
  where rearrange t = setArrayDims t $
                      permuteShape (perm ++ [length perm..arrayRank t-1]) $
                      arrayDims t
        origts = MapNest.typeOf nest
        ts =  map rearrange origts
        setReturnType nesting t' =
          nesting { MapNest.nestingReturnType = t' }

fixupInputs :: [Ident] -> [SOAC.Input] -> Maybe ([Int], [SOAC.Input])
fixupInputs inpIds inps =
  case mapMaybe inputRearrange $ filter exposable inps of
    perm:_ -> do inps' <- mapM (fixupInput (permuteReach perm) perm) inps
                 return (perm, inps')
    _    -> Nothing
  where exposable = maybe False (`elem` inpIds) . SOAC.inputArray

        inputRearrange (SOAC.Input ts _)
          | _ SOAC.:> SOAC.Rearrange _ perm <- SOAC.viewl ts = Just perm
        inputRearrange _                                     = Nothing

        fixupInput d perm inp
          | SOAC.inputRank inp >= d =
              Just $ SOAC.addTransform (SOAC.Rearrange [] $ permuteInverse perm) inp
          | otherwise = Nothing

pullReshape :: SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)
pullReshape nest ots
  | SOAC.Reshape cs shape SOAC.:< ots' <- SOAC.viewf ots,
    op@Nest.Map {} <- Nest.operation nest,
    all basicType $ Nest.returnType op = do
  let inputs' = map (SOAC.addTransform $ SOAC.ReshapeOuter cs shape) $
                Nest.inputs nest
      inputTypes = map SOAC.inputType inputs'
      outernest inner outershape = do
        let addDims t = arrayOf t (Shape outershape) $ uniqueness t
            retTypes = map addDims $ Nest.returnType op

        ps <- forM (zip (Nest.params op) inputTypes) $
              \(p, inpt) -> do
                let t = rowType (stripArray (length outershape-1) inpt)
                        `setUniqueness` uniqueness (identType p)
                newIdent "pullReshape_param" t

        bnds <- forM retTypes $ \_ ->
                  newNameFromString "pullReshape_bnd"

        let nesting = Nest.Nesting {
                        Nest.nestingParamNames = map identName ps
                      , Nest.nestingInputs = map SOAC.varInput ps
                      , Nest.nestingResult = bnds
                      , Nest.nestingReturnType = retTypes
                      }
        return $ Nest.Map [] (Nest.NewNest nesting inner)
  -- Only have the certificates on the outermost loop nest.  This
  -- only has the significance of making the generated code look
  -- very slightly neater.
  op' <- foldM outernest ([] `Nest.setCombCertificates` op) $
         drop 1 $ reverse $ drop 1 $ tails shape
  let nest'   = Nest.SOACNest {
                  Nest.inputs    = inputs'
                , Nest.operation =
                  Nest.combCertificates op `Nest.setCombCertificates` op'
                }
  return (nest', ots')
pullReshape _ _ = fail "Cannot pull reshape"

-- Tie it all together in exposeInputs (for making inputs to a
-- consumer available) and pullOutputTransforms (for moving
-- output-transforms of a producer to its inputs instead).

exposeInputs :: [Ident] -> FusedKer
             -> TryFusion (FusedKer, SOAC.ArrayTransforms)
exposeInputs inpIds ker =
  (exposeInputs' =<< pushRearrange') <|>
  (exposeInputs' =<< pullRearrange') <|>
  exposeInputs' ker
  where nest = Nest.fromSOAC $ fsoac ker
        ot = outputTransform ker

        pushRearrange' = do
          (nest', ot') <- pushRearrange inpIds nest ot
          soac         <- Nest.toSOAC nest'
          return ker { fsoac = soac, outputTransform = ot' }

        pullRearrange' = do
          (nest',ot') <- pullRearrange nest ot
          unless (SOAC.nullTransforms ot') $
            fail "pullRearrange was not enough"
          soac        <- Nest.toSOAC nest'
          return ker { fsoac = soac,
                       outputTransform = SOAC.noTransforms
                     }

        exposeInputs' ker' =
          case commonTransforms inpIds $ inputs ker' of
            (ot', inps') | all exposed inps' ->
              return (ker' { fsoac = inps' `SOAC.setInputs` fsoac ker'}, ot')
            _ -> fail "Cannot expose"

        exposed (SOAC.Input ts (SOAC.Var _))
          | SOAC.nullTransforms ts = True
        exposed inp = maybe True (`notElem` inpIds) $ SOAC.inputArray inp

outputTransformPullers :: [SOACNest -> SOAC.ArrayTransforms -> TryFusion (SOACNest, SOAC.ArrayTransforms)]
outputTransformPullers = [pullRearrange, pullReshape]

pullOutputTransforms :: SOAC -> SOAC.ArrayTransforms -> TryFusion (SOAC, SOAC.ArrayTransforms)
pullOutputTransforms soac origOts = do
  (nest, ots') <- attemptAll (Nest.fromSOAC soac) origOts
  soac' <- Nest.toSOAC nest
  return (soac', ots')
  where attemptAll nest ots = attempt nest ots outputTransformPullers
        attempt _ _ [] = fail "Cannot pull anything"
        attempt nest ots (p:ps) = do
          (nest',ots') <- p nest ots
          if SOAC.nullTransforms ots' then return (nest', SOAC.noTransforms)
          else attemptAll nest' ots' <|> return (nest', ots')
          <|> attempt nest ots ps
