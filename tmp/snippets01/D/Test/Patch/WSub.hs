{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Darcs.Test.Patch.WSub where

{-
The Examples.Set2Unwitnessed module builds a lot of test cases by pattern matching
on the results of merge/commute in where clauses. This would
be very painful to switch to using witnesses properly, because
we'd have to make them use case in series.

So instead we give up on witnesses for this module, but instead
of preprocessor hacks which make incompatible code with the rest
of darcs, we build a fresh set of witnesses constructors (FL etc)
which aren't actually GADTs or existentials. So the pattern matching
works as before, but we need to translate back and forth a lot.

We call the normal darcs constructors the 'W' variants.
-}

import qualified Darcs.Test.Patch.Arbitrary.Generic as W
     ( getPairs, getTriples )

import qualified Darcs.Patch as W ( commute )
import qualified Darcs.Patch.Merge as W ( merge, mergeFL )
import qualified Darcs.Patch.Prim as W ( join )

import qualified Darcs.Witnesses.Ordered as W
import Darcs.Witnesses.Sealed
import Darcs.Witnesses.Eq
import Darcs.Witnesses.Show
import Darcs.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart, unsafeCoercePEnd )

import Darcs.Patch.Merge ( Merge )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.Patchy ( Commute, Invert(..) )

#include "gadts.h"

infixr 5 :>:
infixr 5 +>+
infixr 1 :>
infix 1 :/\:
infix 1 :\/:

data FL p C(x y) where
   NilFL :: FL p C(x y)
   (:>:) :: p C(x y) -> FL p C(x y) -> FL p C(x y)

(+>+) :: FL p C(x y) -> FL p C(x y) -> FL p C(x y)
NilFL +>+ ps = ps
(p :>: ps) +>+ qs = p :>: (ps +>+ qs)

data (p :> q) C(x y) where
   (:>) :: p C(x y) -> q C(x y) -> (p :> q) C(x y)

data (p :\/: q) C(x y) where
   (:\/:) :: p C(x y) -> q C(x y) -> (p :\/: q) C(x y)

data (p :/\: q) C(x y) where
   (:/\:) :: p C(x y) -> q C(x y) -> (p :/\: q) C(x y)

class WSub wp p | p -> wp, wp -> p where
   fromW :: wp C(x y) -> p C(x y)
   toW :: p C(x y) -> wp C(x y)

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:\/: wp2) (p1 :\/: p2) where
   fromW (x W.:\/: y) = unsafeCoerceP (fromW x) :\/: unsafeCoerceP (fromW y)
   toW (x :\/: y) = unsafeCoerceP (toW x) W.:\/: unsafeCoerceP (toW y)

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:/\: wp2) (p1 :/\: p2) where
   fromW (x W.:/\: y) = unsafeCoerceP (fromW x) :/\: unsafeCoerceP (fromW y)
   toW (x :/\: y) = unsafeCoerceP (toW x) W.:/\: unsafeCoerceP (toW y)

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:> wp2) (p1 :> p2) where
   fromW (x W.:> y) = unsafeCoercePEnd (fromW x) :> unsafeCoercePStart (fromW y)
   toW (x :> y) = unsafeCoercePEnd (toW x) W.:> unsafeCoercePStart (toW y)

instance WSub wp p => WSub (W.FL wp) (FL p) where
   fromW W.NilFL = unsafeCoerceP NilFL
   fromW (x W.:>: xs) = unsafeCoercePEnd (fromW x) :>: unsafeCoercePStart (fromW xs)

   toW NilFL = unsafeCoerceP W.NilFL
   toW (x :>: xs) = unsafeCoercePEnd (toW x) W.:>: unsafeCoercePStart (toW xs)

instance WSub prim prim => WSub (RealPatch prim) (RealPatch prim) where
   fromW = id
   toW = id

instance WSub Prim Prim where
   fromW = id
   toW = id

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show ((p :> q) C(x y)) where
   show = show . toW

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show2 (p :> q) where
   showDict2 = ShowDictClass

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show ((p :\/: q) C(x y)) where
   show = show . toW

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show2 (p :\/: q) where
   showDict2 = ShowDictClass

instance (WSub wp p, Show2 wp) => Show (FL p C(x y)) where
   show = show . toW

instance (WSub wp p, Show2 wp) => Show2 (FL p) where
   showDict2 = ShowDictClass

instance (WSub wp p, Commute wp, MyEq wp) => MyEq (FL p) where
   unsafeCompare x y = unsafeCompare (toW x) (toW y)

instance (WSub wp p, Commute wp, Invert wp) => Invert (FL p) where
   invert = fromW . invert . toW

instance (WSub wp p, Commute wp) => Commute (FL p) where
   commute (xs W.:> ys) = do ys' W.:> xs' <- W.commute (toW xs W.:> toW ys)
                             return (fromW ys' W.:> fromW xs')

mergeFL :: (WSub wp p, Merge wp) => (p :\/: FL p) C(x y) -> (FL p :/\: p) C(x y)
mergeFL = fromW . W.mergeFL . toW

merge :: (WSub wp p, Merge wp) => (p :\/: p) C(x y) -> (p :/\: p) C(x y)
merge = fromW . W.merge . toW

commute :: (WSub wp p, Commute wp) => (p :> p) C(x y) -> Maybe ((p :> p) C(x y))
commute = fmap fromW . W.commute . toW

getPairs :: FL (RealPatch Prim) C(x y) -> [Sealed2 (RealPatch Prim :> RealPatch Prim)]
getPairs = map (mapSeal2 fromW) . W.getPairs . toW

getTriples :: FL (RealPatch Prim) C(x y) -> [Sealed2 (RealPatch Prim :> RealPatch Prim :> RealPatch Prim)]
getTriples = map (mapSeal2 fromW) . W.getTriples . toW

join :: (Prim :> Prim) C(x y) -> Maybe (FL Prim C(x y))
join = fmap fromW . W.join . toW

