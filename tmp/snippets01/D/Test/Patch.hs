{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 700
{-# LANGUAGE ImpredicativeTypes #-}
#endif
--  Copyright (C) 2002-2005,2007 David Roundy
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

module Darcs.Test.Patch ( testSuite ) where

import Data.Maybe( isNothing )
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck.Arbitrary( Arbitrary )
import Test.QuickCheck( Testable )
import Test.HUnit ( assertBool )

import Darcs.Test.Util.TestResult ( TestResult, isOk, fromMaybe )
import Darcs.Test.Patch.Utils ( testConditional )

import Darcs.Witnesses.Ordered
import Darcs.Witnesses.Sealed
import Darcs.Witnesses.Eq ( unsafeCompare )
import Darcs.Witnesses.Show
import Darcs.Patch.Prim( PrimPatch, join, FromPrim, PrimOf, PrimPatchBase )
import Darcs.Patch.Prim.Class( PrimCanonize )
import qualified Darcs.Patch.Prim.V1 as V1 ( Prim )
import qualified Darcs.Patch.Prim.V3 as V3 ( Prim )
import qualified Darcs.Patch.V1 as V1
import Darcs.Patch.V2.Real ( isConsistent, isForward, RealPatch )
import Darcs.Patch.RepoPatch( RepoPatch )
import Darcs.Patch.Patchy ( Commute(..), Patchy )
import Darcs.Patch.Merge( Merge )
import Darcs.Patch.Apply( ApplyState )

import Darcs.Test.Patch.Arbitrary.Generic
import qualified Darcs.Test.Patch.Arbitrary.PrimV1 as P1
import qualified Darcs.Test.Patch.Arbitrary.PrimV3 as P3
import Darcs.Test.Patch.Arbitrary.Real
import Darcs.Test.Patch.Arbitrary.PatchV1 ()
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.V3Model( V3Model )
import Darcs.Test.Patch.V1Model( V1Model )
import Darcs.Test.Patch.WithState( WithState, wsPatch, WithStartState )

import qualified Darcs.Test.Patch.Info

import qualified Darcs.Test.Patch.Examples.Set1 as Ex
import qualified Darcs.Test.Patch.Examples.Set2Unwitnessed as ExU

import Darcs.Test.Patch.Properties.Check( Check(..), checkAPatch )
import qualified Darcs.Test.Patch.Properties.V1Set1 as Prop
import qualified Darcs.Test.Patch.Properties.V1Set2 as Prop
import qualified Darcs.Test.Patch.Properties.Generic as Prop
import qualified Darcs.Test.Patch.Properties.Real as Prop
import qualified Darcs.Test.Patch.Properties.GenericUnwitnessed as PropU

import qualified Darcs.Test.Patch.WSub as WSub

#include "gadts.h"

type instance ModelOf (FL prim) = ModelOf prim

#if __GLASGOW_HASKELL__ >= 700
type TestGenerator thing gen = (FORALL(t ctx) ((FORALL(xx yy) thing xx yy -> t) -> (gen ctx -> t)))
type TestCondition thing = (FORALL (yy zz) thing C(yy zz) -> Bool)
type TestCheck thing t = (FORALL (yy zz) thing C(yy zz) -> t)

-- arbitraryThing :: (FORALL(xx yy) thing C(xx yy) -> t) -> (thing C(a b) -> t)
arbitraryThing :: x -> TestGenerator thing (thing x)
arbitraryThing _ f p = f p
#endif

-- | Run a test function on a set of data, using HUnit. The test function should
--   return @Nothing@ upon success and a @Just x@ upon failure.
testCases :: Show a => String               -- ^ The test name
             -> (a -> TestResult)  -- ^ The test function
             -> [a]                -- ^ The test data
             -> Test
testCases name test datas = testCase name (assertBool assertName res)
    where assertName = "Boolean assertion for \"" ++ name ++ "\""
          res        = and $ map (isOk . test) datas

unit_V1P1:: [Test]
unit_V1P1 =
  [ testCases "known commutes" Prop.checkCommute Ex.knownCommutes
  , testCases "known non-commutes" Prop.checkCantCommute Ex.knownCantCommutes
  , testCases "known merges" Prop.checkMerge Ex.knownMerges
  , testCases "known merges (equiv)" Prop.checkMergeEquiv Ex.knownMergeEquivs
  , testCases "known canons" Prop.checkCanon Ex.knownCanons
  , testCases "merge swaps" Prop.checkMergeSwap Ex.mergePairs2
  , testCases "the patch validation works" Prop.tTestCheck Ex.validPatches
  , testCases "commute/recommute" (Prop.recommute commute) Ex.commutePairs
  , testCases "merge properties: merge either way valid" Prop.tMergeEitherWayValid Ex.mergePairs
  , testCases "merge properties: merge swap" Prop.mergeEitherWay Ex.mergePairs
  , testCases "primitive patch IO functions" (Prop.tShowRead eqFLUnsafe) Ex.primitiveTestPatches
  , testCases "IO functions (test patches)" (Prop.tShowRead eqFLUnsafe) Ex.testPatches
  , testCases "IO functions (named test patches)" (Prop.tShowRead unsafeCompare) Ex.testPatchesNamed
  , testCases "primitive commute/recommute" (Prop.recommute commute) Ex.primitiveCommutePairs
  ]

unit_V2P1 :: [Test]
unit_V2P1 =
  [ testCases "join commute" (PropU.joinCommute WSub.join) ExU.primPermutables
  , testCases "prim recommute" (PropU.recommute WSub.commute) ExU.commutables
  , testCases "prim patch and inverse commute" (PropU.patchAndInverseCommute WSub.commute) ExU.commutables
  , testCases "prim inverses commute" (PropU.commuteInverses WSub.commute) ExU.commutables
  , testCases "FL prim recommute" (PropU.recommute WSub.commute) ExU.commutablesFL
  , testCases "FL prim patch and inverse commute" (PropU.patchAndInverseCommute WSub.commute) ExU.commutablesFL
  , testCases "FL prim inverses commute" (PropU.commuteInverses WSub.commute) $ ExU.commutablesFL
  , testCases "fails" (PropU.commuteFails WSub.commute) ([] :: [(V1.Prim WSub.:> V1.Prim) C(x y)])
  , testCases "read and show work on Prim" PropU.show_read ExU.primPatches
  , testCases "read and show work on RealPatch" PropU.show_read ExU.realPatches
  , testCases "example flattenings work" PropU.consistentTreeFlattenings ExU.realPatchLoopExamples
  , testCases "real merge input consistent" (PropU.mergeArgumentsConsistent isConsistent) ExU.realMergeables
  , testCases "real merge input is forward" (PropU.mergeArgumentsConsistent isForward) ExU.realMergeables
  , testCases "real merge output is forward" (PropU.mergeConsistent isForward) ExU.realMergeables
  , testCases "real merge output consistent" (PropU.mergeConsistent isConsistent) ExU.realMergeables
  , testCases "real merge either way" PropU.mergeEitherWay ExU.realMergeables
  , testCases "real merge and commute" PropU.mergeCommute ExU.realMergeables

  , testCases "real recommute" (PropU.recommute WSub.commute) ExU.realCommutables
  , testCases "real inverses commute" (PropU.commuteInverses WSub.commute) ExU.realCommutables
  , testCases "real permutivity" (PropU.permutivity WSub.commute) ExU.realNonduplicateTriples
  , testCases "real partial permutivity" (PropU.partialPermutivity WSub.commute) ExU.realNonduplicateTriples
  ]

instance PrimPatch prim => Check (RealPatch prim) where
  checkPatch p = return $ isNothing $ isConsistent p

instance Check V3.Prim where
  checkPatch _ = return True -- XXX

commuteReals :: PrimPatch prim => (RealPatch prim :> RealPatch prim) C(x y) -> Maybe ((RealPatch prim :> RealPatch prim) C(x y))
commuteReals = commute

qc_prim :: forall prim C(x y a) model.
           (PrimPatch prim, ArbitraryPrim prim, Show2 prim
           , model ~ ModelOf prim, RepoModel model
           , RepoState model ~ ApplyState (PrimOf prim)
           , Show1 (ModelOf prim)
           , Check prim, PrimPatchBase prim, PrimOf prim ~ prim
           , FromPrim prim
           , Show1 (prim C(a))
           , Show1 ((prim :> prim) C(a))
           , Show1 (WithState model prim C(a))
           , Arbitrary (Sealed ((prim :> prim) C(a)))
           , Arbitrary (Sealed ((prim :> prim :> prim) C(a)))
           , Arbitrary (Sealed (prim C(a)))
           , Arbitrary (Sealed (FL prim C(a)))
           , Arbitrary (Sealed ((FL prim :> FL prim) C(a)))
           , Arbitrary (Sealed (WithState model prim C(a)))
           , Arbitrary (Sealed (WithState model (FL prim) C(a)))
           , Arbitrary (Sealed2 (WithState model (prim :> prim)))
           , Arbitrary (Sealed ((WithState model (prim :> prim)) C(a)))
           , Arbitrary (Sealed ((WithState model (FL prim :> FL prim)) C(a)))
           ) => prim C(x y) -> [Test]
qc_prim _ =
  -- The following fails because of setpref patches...
  -- testProperty "prim inverse doesn't commute" (inverseDoesntCommute :: Prim -> Maybe Doc)
  [ testProperty "prim join effect preserving... "
    (unseal2 $ Prop.joinEffectPreserving join :: Sealed2 (WithState model (prim :> prim)) -> TestResult)
  ]
#if __GLASGOW_HASKELL__ >= 700
    ++ concat
  [ pair_properties            (undefined :: prim C(x y))    "arbitrary"    arbitraryThing'
  , pair_properties            (undefined :: FL prim C(x y)) "arbitrary FL" arbitraryThing'
  , coalesce_properties        (undefined :: prim C(x y))    "arbitrary"    arbitraryThing'
  , nonreal_commute_properties (undefined :: prim C(x y))    "arbitrary"    arbitraryThing'
  , nonreal_commute_properties (undefined :: FL prim C(x y)) "arbitrary FL" arbitraryThing'
  , patch_properties           (undefined :: prim C(x a))    "arbitrary"    arbitraryThing'
  , patch_properties           (undefined :: FL prim C(x a)) "arbitrary FL" arbitraryThing'
  , patch_repo_properties      (undefined :: prim C(x a))    "arbitrary"    arbitraryThing'
  , patch_repo_properties      (undefined :: FL prim C(x a)) "arbitrary FL" arbitraryThing'
  , pair_repo_properties       (undefined :: prim C(x a))    "arbitrary"    arbitraryThing'
  , pair_repo_properties       (undefined :: FL prim C(x a)) "arbitrary FL" arbitraryThing'
  ]
      where arbitraryThing' = arbitraryThing (undefined :: a) -- bind the witness for generator
#endif

qc_V2P1 :: [Test]
qc_V2P1 =
  [ testProperty "tree flattenings are consistent... "
    Prop.propConsistentTreeFlattenings
  , testProperty "with quickcheck that real patches are consistent... "
    (unseal $ P1.patchFromTree $ fromMaybe . isConsistent)
  -- permutivity ----------------------------------------------------------------------------
  , testConditional "permutivity"
    (unseal $ P1.commuteTripleFromTree notDuplicatestriple)
    (unseal $ P1.commuteTripleFromTree $ Prop.permutivity commuteReals)
  , testConditional "partial permutivity"
    (unseal $ P1.commuteTripleFromTree notDuplicatestriple)
    (unseal $ P1.commuteTripleFromTree $ Prop.partialPermutivity commuteReals)
  , testConditional "nontrivial permutivity"
    (unseal $ P1.commuteTripleFromTree (\t -> nontrivialTriple t && notDuplicatestriple t))
    (unseal $ P1.commuteTripleFromTree $ (Prop.permutivity commuteReals))
  ]

qc_V2 :: forall prim C(xx yy a). (PrimPatch prim, Show1 (ModelOf prim), RepoModel (ModelOf prim),
                                  Check (RealPatch prim), ArbitraryPrim prim, Show2 prim,
                                  RepoState (ModelOf prim) ~ ApplyState prim)
      => prim C(xx yy) -> [Test]
qc_V2 _ =
  [ testProperty "readPatch and showPatch work on RealPatch... "
    (unseal $ patchFromTree $ (Prop.show_read :: RealPatch prim C(x y) -> TestResult))
  , testProperty "readPatch and showPatch work on FL RealPatch... "
    (unseal2 $ (Prop.show_read :: FL (RealPatch prim) C(x y) -> TestResult))
  , testProperty "we can do merges using QuickCheck"
    (isNothing . (Prop.propIsMergeable ::
                     Sealed (WithStartState (ModelOf prim) (Tree prim))
                     -> Maybe (Tree (RealPatch prim) C(x))))
  ]
#if __GLASGOW_HASKELL__ >= 700
  ++ concat
  [ merge_properties   (undefined :: RealPatch prim C(x y)) "tree" mergePairFromTree
  , merge_properties   (undefined :: RealPatch prim C(x y)) "twfp" mergePairFromTWFP
  , pair_properties    (undefined :: RealPatch prim C(x y)) "tree" commutePairFromTree
  , pair_properties    (undefined :: RealPatch prim C(x y)) "twfp" commutePairFromTWFP
  , patch_properties   (undefined :: RealPatch prim C(x y)) "tree" patchFromTree
  -- , patch_repo_properties (undefined :: RealPatch prim C(x y)) "tree" arbitraryThing'
  ]
    where arbitraryThing' = arbitraryThing (undefined :: a)
#endif

#if __GLASGOW_HASKELL__ >= 700
properties :: forall thing gen. (Show1 gen, Arbitrary (Sealed gen)) =>
              TestGenerator thing gen
           -- -> forall xx yy. thing xx yy
           -> String -> String
           -> forall t. Testable t => [(String, TestCondition thing, TestCheck thing t)]
           -> [Test]
properties gen prefix genname tests =
  [ cond name condition check | (name, condition, check) <- tests ]
  where cond :: forall testable. Testable testable
             => String -> TestCondition thing -> TestCheck thing testable -> Test
        cond t c p =
          testConditional (prefix ++ " (" ++ genname ++ "): " ++ t) (unseal $ gen c) (unseal $ gen p)

type PropList what gen = String -> TestGenerator what gen -> [Test]

pair_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen), Patchy p)
                   => p x y -> PropList (p :> p) gen
pair_properties _ genname gen =
  properties gen "commute" genname
  [ ("recommute"              , const True       , Prop.recommute commute             )
  , ("nontrivial recommute"   , nontrivialCommute, Prop.recommute commute             )
  , ("inverses commute"       , const True       , Prop.commuteInverses commute       )
  , ("nontrivial inverses"    , nontrivialCommute, Prop.commuteInverses commute       )
  , ("inverse composition"    , const True       , Prop.inverseComposition            )
  ]

coalesce_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen), Patchy p, PrimPatch p)
                    => p x y -> PropList (p :> p :> p) gen
coalesce_properties _ genname gen =
  properties gen "commute" genname
  [ ("join commutes with commute", const True, Prop.joinCommute join) ]

-- The following properties do not hold for "Real" patches (conflictors and
-- duplicates, specifically) .
nonreal_commute_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen), Patchy p)
                           => p x y -> PropList (p :> p) gen
nonreal_commute_properties _ genname gen =
  properties gen "commute" genname
  [ ("patch & inverse commute", const True       , Prop.patchAndInverseCommute commute)
  , ("patch & inverse commute", nontrivialCommute, Prop.patchAndInverseCommute commute)
  ]

patch_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen), Patchy p)
                 => p x y -> PropList p gen
patch_properties _ genname gen =
  properties gen "patch" genname
  [ ("inverse . inverse is id"  , const True       , Prop.invertSymmetry)
  ]

patch_repo_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen), Patchy p,
                                            RepoModel (ModelOf (PrimOf p)),
                                            RepoState (ModelOf (PrimOf p)) ~ ApplyState p)
                      => p x y -> PropList (WithState (ModelOf (PrimOf p)) p) gen
patch_repo_properties _ genname gen =
  properties gen "patch/repo" genname
  [ ("invert rollback"          , const True       , Prop.invertRollback)
  ]

pair_repo_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen), Patchy p,
                                            RepoModel (ModelOf p),
                                            RepoState (ModelOf p) ~ ApplyState p)
                      => p x y -> PropList (WithState (ModelOf p) (p :> p)) gen
pair_repo_properties _ genname gen =
  properties gen "patch/repo" genname
  [ ("commute is effect preserving" , const True       , Prop.effectPreserving commute )
  ]

merge_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen)
                                      , Patchy p, Merge p, Show2 p, Check p)
                 => p x y -> PropList (p :\/: p) gen
merge_properties _ genname gen =
  properties gen "merge" genname
  [ ("merge either way"           , const True     , Prop.mergeEitherWay      )
  , ("merge either way valid"     , const True     , Prop.tMergeEitherWayValid)
  , ("nontrivial merge either way", nontrivialMerge, Prop.mergeEitherWay      )
  , ("merge commute"              , const True     , Prop.mergeCommute        )
  ]
#endif

qc_V1P1 :: [Test]
qc_V1P1 =
  [
    testProperty "show and read work right" (unseal Prop.propReadShow)
  ]
  ++ Prop.checkSubcommutes Prop.subcommutesInverse "patch and inverse both commute"
  ++ Prop.checkSubcommutes Prop.subcommutesNontrivialInverse "nontrivial commutes are correct"
  ++ Prop.checkSubcommutes Prop.subcommutesFailure "inverses fail"
  ++
  [ testProperty "commuting by patch and its inverse is ok" Prop.propCommuteInverse
  -- , testProperty "conflict resolution is valid... " Prop.propResolveConflictsValid
  , testProperty "a patch followed by its inverse is identity"
    Prop.propPatchAndInverseIsIdentity
  , testProperty "'simple smart merge'" Prop.propSimpleSmartMergeGoodEnough
  , testProperty "commutes are equivalent" Prop.propCommuteEquivalency
  , testProperty "merges are valid" Prop.propMergeValid
  , testProperty "inverses being valid" Prop.propInverseValid
  , testProperty "other inverse being valid" Prop.propOtherInverseValid
  -- The patch generator isn't smart enough to generate correct test cases for
  -- the following: (which will be obsoleted soon, anyhow)
  -- , testProperty "the order dependence of unravel... " Prop.propUnravelOrderIndependent
  -- , testProperty "the unravelling of three merges... " Prop.propUnravelThreeMerge
  -- , testProperty "the unravelling of a merge of a sequence... " Prop.propUnravelSeqMerge
  , testProperty "the order of commutes" Prop.propCommuteEitherOrder
  , testProperty "commute either way" Prop.propCommuteEitherWay
  , testProperty "the double commute" Prop.propCommuteTwice
  , testProperty "merges commute and are well behaved"
    Prop.propMergeIsCommutableAndCorrect
  , testProperty "merges can be swapped" Prop.propMergeIsSwapable
  , testProperty "again that merges can be swapped (I'm paranoid) " Prop.propMergeIsSwapable

  ] -- the following properties are disabled, because they routinely lead to
    -- exponential cases, making the tests run for ever and ever; nevertheless,
    -- we would expect them to hold
 {- ++ merge_properties (undefined :: V1.Patch Prim C(x y)) "tree" mergePairFromTree
    ++ merge_properties (undefined :: V1.Patch Prim C(x y)) "twfp" mergePairFromTWFP
    ++ commute_properties (undefined :: V1.Patch Prim C(x y)) "tree" commutePairFromTree
    ++ commute_properties (undefined :: V1.Patch Prim C(x y)) "twfp" commutePairFromTWFP -}

-- | This is the big list of tests that will be run using testrunner.
testSuite :: [Test]
testSuite = [ testGroup "Darcs.Patch.Prim.V1" $ qc_prim (undefined :: V1.Prim C(x y))
            , testGroup "Darcs.Patch.V1 (using Prim.V1)" $ unit_V1P1 ++ qc_V1P1
            , testGroup "Darcs.Patch.V2 (using Prim.V1)" $ unit_V2P1 ++ qc_V2 (undefined :: V1.Prim C(x y)) ++ qc_V2P1
            , testGroup "Darcs.Patch.Prim.V3" $ qc_prim (undefined :: V3.Prim C(x y))
            , testGroup "Darcs.Patch.V2 (using Prim.V3)" $ qc_V2 (undefined :: V3.Prim C(x y))
            , Darcs.Test.Patch.Info.testSuite
            ]
