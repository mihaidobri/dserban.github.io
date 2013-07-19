{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V3.Details
    ()
    where

import Darcs.Patch.Prim.Class ( PrimDetails(..) )
import Darcs.Patch.Prim.V3.Core ( Prim(..) )

#include "gadts.h"

-- TODO
instance PrimDetails Prim where
  summarizePrim _ = []
