-- Copyright (C) 2002-2005,2007 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Repository.ApplyPatches ( applyPatches ) where

import Darcs.Patch.ApplyMonad( ApplyMonad )
import Darcs.MonadProgress ( MonadProgress, ProgressAction(..), runProgressActions)
import Darcs.Patch ( Patchy, apply )
import Darcs.IO () -- for ApplyMonad IO
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info )
import Darcs.Patch.Info ( humanFriendly )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Witnesses.Ordered ( FL(..), mapFL )
import Printer ( text, ($$) )

applyPatches :: (MonadProgress m, ApplyMonad m (ApplyState p), Patchy p) => FL (PatchInfoAnd p) C(x y) -> m ()
applyPatches ps = runProgressActions "Applying patch" (mapFL doApply ps)
   where doApply hp =
            ProgressAction { paAction = apply (hopefully hp)
                           , paMessage = humanFriendly (info hp)
                           , paOnError = text "Unapplicable patch:" $$ humanFriendly (info hp) }
