-- Copyright (C) 2003 David Roundy
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

{-# LANGUAGE CPP, EmptyDataDecls #-}

#include "gadts.h"

module Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, Origin,
                         progressPatchSet, tags, appendPSFL,
                         newset2RL, newset2FL ) where

import Progress ( progress )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Witnesses.Ordered ( FL, RL(..), (+<+), reverseFL,
                                 reverseRL, mapRL_RL, concatRL, mapRL )
import Darcs.Witnesses.Sealed ( Sealed )

data Origin

type SealedPatchSet p C(start) = Sealed ((PatchSet p) C(start))

-- A PatchSet is a list of patches since the last tag, and a list of tagged
-- patch lists that form a repo's history.
data PatchSet p C(start y) where
    PatchSet :: RL (PatchInfoAnd p) C(x y) -> RL (Tagged p) C(start x) -> PatchSet p C(start y)

-- A Tagged is a Tag, the hash of the 'previous' inventory (if it exists) and
-- the list of patches since that previous inventory.
data Tagged p C(x z) where
    Tagged :: PatchInfoAnd p C(y z) -> Maybe String
           -> RL (PatchInfoAnd p) C(x y) -> Tagged p C(x z)

-- |newset2RL takes a PatchSet and returns an equivalent, linear RL of patches.
newset2RL :: PatchSet p C(start x) -> RL (PatchInfoAnd p) C(start x)
newset2RL (PatchSet ps ts) = ps +<+ concatRL (mapRL_RL ts2rl ts)
    where ts2rl :: Tagged p C(y z) -> RL (PatchInfoAnd p) C(y z)
          ts2rl (Tagged t _ ps2) = t :<: ps2

-- |newset2FL takes a PatchSet and returns an equivalent, linear FL of patches.
newset2FL :: PatchSet p C(start x) -> FL (PatchInfoAnd p) C(start x)
newset2FL = reverseRL . newset2RL

-- |appendPSFL takes a PatchSet and a FL of patches that 'follow' the PatchSet,
-- and concatenates the patches into the PatchSet.
appendPSFL :: PatchSet p C(start x) -> FL (PatchInfoAnd p) C(x y)
           -> PatchSet p C(start y)
appendPSFL (PatchSet ps ts) newps = PatchSet (reverseFL newps +<+ ps) ts

-- |Runs a progress action for each tag and patch in a given PatchSet, using
-- the passed progress message. Does not alter the PatchSet.
progressPatchSet :: String -> PatchSet p C(start x) -> PatchSet p C(start x)
progressPatchSet k (PatchSet ps0 ts0) = PatchSet (mapRL_RL prog ps0) $ mapRL_RL pts ts0
    where prog = progress k
          pts :: Tagged p C(y z) -> Tagged p C(y z)
          pts (Tagged t h ps) = Tagged (prog t) h (mapRL_RL prog ps)

-- |tags returns the PatchInfos corresponding to the tags of a given PatchSet.
tags :: PatchSet p C(start x) -> [PatchInfo]
tags (PatchSet _ ts) = mapRL f ts
    where f :: Tagged p C(y z) -> PatchInfo
          f (Tagged t _ _) = info t
