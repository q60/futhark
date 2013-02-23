{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.EnablingOptDriver ( 
                                    enablingOpts
                                  , copyCtProp
                                  , CallGraph
                                  , buildCG
                                  , aggInlineDriver
                                  , deadFunElim
                                  , EnablingOptError(..)
                            )
  where
 
 
--import Data.Either

import L0.AbSyn

import L0.EnablingOpts.InliningDeadFun
import L0.EnablingOpts.CopyCtPropFold
import L0.EnablingOpts.EnablingOptErrors


--------------------------------------------------------------
---- Enabling Optimization Driver
--------------------------------------------------------------

enablingOpts :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
enablingOpts prog = do

    proginline <- aggInlineDriver   prog

    progdfelim <- deadFunElim       proginline

    (success, progcp) <- copyCtProp progdfelim

    if(success) 
    then enablingOpts progcp
    else return progcp

