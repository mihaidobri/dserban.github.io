module Darcs.Test.Patch.Properties.GenericUnwitnessed where

import qualified Darcs.Test.Patch.Properties.Generic as W
     ( permutivity, partialPermutivity
     , mergeConsistent, mergeArgumentsConsistent, mergeEitherWay
     , mergeCommute, patchAndInverseCommute, joinCommute, commuteInverses
     , recommute
     , show_read )
import Darcs.Test.Patch.Arbitrary.Generic ( Tree )
import Darcs.Test.Patch.RepoModel( RepoModel, RepoState )
import Darcs.Test.Patch.WithState( WithStartState )

import qualified Darcs.Test.Patch.Properties.Real as W ( propConsistentTreeFlattenings )
import Darcs.Test.Patch.WSub
import Darcs.Test.Util.TestResult

import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Patchy ( showPatch )
import Darcs.Witnesses.Show
import Darcs.Witnesses.Eq
import Darcs.Witnesses.Sealed( Sealed )
import Darcs.Patch.Merge ( Merge )
import Darcs.Patch ( Patchy )
import Printer ( Doc, redText, ($$) )
import qualified Storage.Hashed.Tree as HST ( Tree )

#include "gadts.h"

permutivity :: (Patchy wp, WSub wp p) => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
            -> (p :> p :> p) C(a b) -> TestResult
permutivity f = W.permutivity (fmap toW . f . fromW) . toW

partialPermutivity :: (Patchy wp, WSub wp p) => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                    -> (p :> p :> p) C(a b) -> TestResult
partialPermutivity f = W.partialPermutivity (fmap toW . f . fromW) . toW

mergeEitherWay :: (Patchy wp, Merge wp, WSub wp p) => (p :\/: p) C(x y) -> TestResult
mergeEitherWay = W.mergeEitherWay . toW

commuteInverses :: (Patchy wp, WSub wp p) => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                 -> (p :> p) C(a b) -> TestResult
commuteInverses f = W.commuteInverses (fmap toW . f . fromW) . toW

recommute :: (Patchy wp, WSub wp p) => (FORALL(x y) ((p :> p) C(x y) -> Maybe ((p :> p) C(x y))))
          -> (p :> p) C(a b) -> TestResult
recommute f = W.recommute (fmap toW . f . fromW) . toW

mergeCommute :: (Patchy wp, Merge wp, WSub wp p) => (p :\/: p) C(x y) -> TestResult
mergeCommute = W.mergeCommute . toW

mergeConsistent :: (Patchy wp, Merge wp, WSub wp p) =>
                           (FORALL(x y) p C(x y) -> Maybe Doc)
                        -> (p :\/: p) C(a b) -> TestResult
mergeConsistent f = W.mergeConsistent (f . fromW) . toW

mergeArgumentsConsistent :: (Patchy wp, WSub wp p) =>
                              (FORALL(x y) p C(x y) -> Maybe Doc)
                           -> (p :\/: p) C(a b) -> TestResult
mergeArgumentsConsistent f = W.mergeArgumentsConsistent (f . fromW) . toW

show_read :: (Patchy p, Show2 p) => p C(x y) -> TestResult
show_read = W.show_read

patchAndInverseCommute :: (Patchy wp, WSub wp p) =>
                             (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                          -> (p :> p) C(a b) -> TestResult
patchAndInverseCommute f = W.patchAndInverseCommute (fmap toW . f . fromW) . toW


joinCommute :: (FORALL(x y) (Prim :> Prim) C(x y) -> Maybe (FL Prim C(x y)))
             -> (Prim :> Prim :> Prim) C(a b) -> TestResult
joinCommute f = W.joinCommute (fmap toW . f . fromW) . toW

consistentTreeFlattenings :: (RepoState model ~ HST.Tree, RepoModel model)
                          => Sealed (WithStartState model (Tree Prim)) -> TestResult
consistentTreeFlattenings = (\x -> if W.propConsistentTreeFlattenings x
                                      then succeeded
                                      else failed $ redText "oops")

commuteFails :: (MyEq p, Patchy p)
             => ((p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
             -> (p :> p) C(x y)
             -> TestResult
commuteFails c (x :> y) = case c (x :> y) of
                            Nothing -> succeeded
                            Just (y' :> x') ->
                              failed $ redText "x" $$ showPatch x $$
                                       redText ":> y" $$ showPatch y $$
                                       redText "y'" $$ showPatch y' $$
                                       redText ":> x'" $$ showPatch x'
