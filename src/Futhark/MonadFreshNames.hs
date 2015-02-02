{-# LANGUAGE FlexibleInstances #-}
-- | This module provides a monadic facility similar (and built on top
-- of) "Futhark.FreshNames".  The removes the need for a (small) amount of
-- boilerplate, at the cost of using some GHC extensions.  The idea is
-- that if your compiler pass runs in a monad that is an instance of
-- 'MonadFreshNames', you can automatically use the name generation
-- functions exported by this module.
module Futhark.MonadFreshNames
  ( MonadFreshNames (..)
  , modifyNameSource
  , newName
  , newNameFromString
  , newID
  , newIDFromString
  , newVName
  , newIdent
  , newIdent'
  , newIdents
  , newNameSourceForProg
  , module Futhark.FreshNames
  ) where

import Control.Applicative
import qualified Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes (progNames)
import qualified Futhark.FreshNames as FreshNames
import Futhark.FreshNames hiding (newName, newID, newVName)

-- | A monad that stores a name source.  The following is a good
-- instance for a monad in which the only state is a @NameSource vn@:
--
-- @
--  instance MonadFreshNames vn MyMonad where
--    getNameSource = get
--    putNameSource = put
-- @
class (Applicative m, Monad m) => MonadFreshNames m where
  getNameSource :: m VNameSource
  putNameSource :: VNameSource -> m ()

instance (Applicative im, Monad im) => MonadFreshNames (Control.Monad.State.Lazy.StateT VNameSource im) where
  getNameSource = Control.Monad.State.Lazy.get
  putNameSource = Control.Monad.State.Lazy.put

instance (Applicative im, Monad im) => MonadFreshNames (Control.Monad.State.Strict.StateT VNameSource im) where
  getNameSource = Control.Monad.State.Strict.get
  putNameSource = Control.Monad.State.Strict.put

-- | Run a computation needing a fresh name source and returning a new
-- one, using 'getNameSource' and 'putNameSource' before and after the
-- computation.
modifyNameSource :: MonadFreshNames m => (VNameSource -> (a, VNameSource)) -> m a
modifyNameSource m = do src <- getNameSource
                        let (x,src') = m src
                        putNameSource src'
                        return x

-- | Produce a fresh name, using the given name as a template.
newName :: MonadFreshNames m => VName -> m VName
newName = modifyNameSource . flip FreshNames.newName

-- | As @newName@, but takes a 'String' for the name template.
newNameFromString :: MonadFreshNames m => String -> m VName
newNameFromString s = newName $ varName s Nothing

-- | Produce a fresh 'ID', using the given base name as a template.
newID :: MonadFreshNames m => Name -> m VName
newID s = newName $ ID (s, 0)

-- | As 'newID', but takes a 'String' for the name template.
newIDFromString :: MonadFreshNames m => String -> m VName
newIDFromString s = newID $ varName s Nothing

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: MonadFreshNames m => String -> m VName
newVName = newID . nameFromString

-- | Produce a fresh 'Ident', using the given name as a template.
newIdent :: MonadFreshNames m =>
            String -> TypeBase shape -> m (IdentBase shape)
newIdent s t = do
  s' <- newID $ varName s Nothing
  return $ Ident s' t

-- | Produce a fresh 'Ident', using the given 'Ident' as a template,
-- but possibly modifying the name.
newIdent' :: MonadFreshNames m =>
             (String -> String)
          -> IdentBase shape -> m (IdentBase shape)
newIdent' f ident =
  newIdent (f $ nameToString $ baseName $ identName ident)
           (identType ident)

-- | Produce several 'Ident's, using the given name as a template,
-- based on a list of types.
newIdents :: MonadFreshNames m =>
             String -> [TypeBase shape] -> m [IdentBase shape]
newIdents = mapM . newIdent

-- | Create a new 'NameSource' that will never produce any of the
-- names used as variables in the given program.
newNameSourceForProg :: Prog lore -> VNameSource
newNameSourceForProg = newNameSource . progNames
