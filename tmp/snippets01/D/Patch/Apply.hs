--  Copyright (C) 2002-2005 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.


{-# LANGUAGE CPP, MultiParamTypeClasses #-}

#include "gadts.h"

-- |
-- Module      : Darcs.Patch.Apply
-- Copyright   : 2002-2005 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Patch.Apply
    (
      Apply(..)
    , applyToFilePaths
    , applyToTree
    , applyToState
    , effectOnFilePaths
    ) where

import Prelude hiding ( catch, pi )

import Control.Applicative ( (<$>) )
import Control.Arrow ( (***) )

import Storage.Hashed.Tree( Tree )
import Storage.Hashed.Monad( virtualTreeMonad )

import Darcs.Patch.ApplyMonad ( ApplyMonad(..), withFileNames, ApplyMonadTrans(..) )
import Darcs.Patch.FileName( fn2fp, fp2fn )
import Darcs.Witnesses.Ordered ( FL(..), RL(..) )


class Apply p where
    type ApplyState p :: (* -> *) -> *
    apply :: ApplyMonad m (ApplyState p) => p C(x y) -> m ()

instance Apply p => Apply (FL p) where
    type ApplyState (FL p) = ApplyState p
    apply NilFL = return ()
    apply (p:>:ps) = apply p >> apply ps

instance Apply p => Apply (RL p) where
    type ApplyState (RL p) = ApplyState p
    apply NilRL = return ()
    apply (p:<:ps) = apply ps >> apply p


effectOnFilePaths :: (Apply p, ApplyState p ~ Tree)
                  => p C(x y)
                  -> [FilePath]
                  -> [FilePath]
effectOnFilePaths p fps = fps' where
    (_, fps', _) = applyToFilePaths p Nothing fps


applyToFilePaths :: (Apply p, ApplyState p ~ Tree)
                 => p C(x y)
                 -> Maybe [(FilePath, FilePath)]
                 -> [FilePath]
                 -> ([FilePath], [FilePath], [(FilePath, FilePath)])
applyToFilePaths pa ofpos fs = toFPs $ withFileNames ofnos fns (apply pa) where
        fns = map fp2fn fs
        ofnos = map (fp2fn *** fp2fn) <$> ofpos
        toFPs (affected, new, renames) =
            (map fn2fp affected, map fn2fp new, map (fn2fp *** fn2fp) renames)


-- | Apply a patch to a 'Tree', yielding a new 'Tree'.
applyToTree :: (Apply p, Functor m, Monad m, ApplyState p ~ Tree)
            => p C(x y)
            -> Tree m
            -> m (Tree m)
applyToTree patch t = snd <$> virtualTreeMonad (apply patch) t


applyToState :: forall p m C(x y). (Apply p, ApplyMonadTrans m (ApplyState p))
             => p C(x y)
             -> (ApplyState p) m
             -> m ((ApplyState p) m)
applyToState patch t = snd <$> runApplyMonad (apply patch) t
