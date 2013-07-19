{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Test.Patch.Arbitrary.PrimV1 where

import qualified Darcs.Test.Patch.Arbitrary.Generic as T
     ( commuteTripleFromTree, commutePairFromTree, commutePairFromTWFP
     , mergePairFromTree, mergePairFromTWFP
     , patchFromTree )
import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.RepoModel

import Control.Monad ( liftM )
import Test.QuickCheck
import Darcs.Test.Patch.WithState
import Darcs.Witnesses.Sealed
import Darcs.Witnesses.Eq
import Darcs.Witnesses.Unsafe
import Darcs.Witnesses.Ordered
-- import Darcs.Witnesses.Show
import Darcs.Patch.Prim.V1 ()
import Darcs.Patch.Prim.V1.Core ( FilePatchType( Hunk, TokReplace ), Prim( FP ), isIdentity )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.FileHunk( IsHunk( isHunk ), FileHunk(..) )

import Darcs.Test.Patch.V1Model
import Darcs.Test.Util.QuickCheck ( alpha, notIn, maybeOf )

import Darcs.Commands.Replace ( defaultToks )
import Darcs.Patch.Prim

import Control.Applicative ( (<$>) )
import qualified Data.ByteString.Char8 as BC
import Data.Maybe ( isJust )

#include "gadts.h"
#include "impossible.h"

patchFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) p C(y z) -> t) -> WithStartState V1Model (Tree Prim) C(x) -> t
patchFromTree = T.patchFromTree

mergePairFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :\/: p) C(y z) -> t) -> WithStartState V1Model (Tree Prim) C(x) -> t
mergePairFromTree = T.mergePairFromTree

mergePairFromTWFP :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :\/: p) C(y z) -> t) -> WithStartState V1Model (TreeWithFlattenPos Prim) C(x) -> t
mergePairFromTWFP = T.mergePairFromTWFP

commutePairFromTWFP :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :> p) C(y z) -> t) -> WithStartState V1Model (TreeWithFlattenPos Prim) C(x) -> t
commutePairFromTWFP = T.commutePairFromTWFP

commutePairFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :> p) C(y z) -> t) -> WithStartState V1Model (Tree Prim) C(x) -> t
commutePairFromTree = T.commutePairFromTree

commuteTripleFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :> p :> p) C(y z) -> t) -> WithStartState V1Model (Tree Prim) C(x) -> t
commuteTripleFromTree = T.commuteTripleFromTree

nonEmptyHunk :: (IsHunk p) => p C(x y) -> Bool
nonEmptyHunk p
  | Just (FileHunk _ _ [] []) <- isHunk p = False
  | otherwise                             = True

nonEmptyHunksPair :: (IsHunk p) => (p :> p) C(x y) -> Bool
nonEmptyHunksPair (p1 :> p2) = nonEmptyHunk p1 && nonEmptyHunk p2

nonEmptyHunksTriple :: (IsHunk p) => (p :> p :> p) C(x y) -> Bool
nonEmptyHunksTriple (p1 :> p2 :> p3) = nonEmptyHunk p1 && nonEmptyHunk p2 && nonEmptyHunk p3

nonEmptyHunksFLPair :: (IsHunk p) => (FL p :> FL p) C(x y) -> Bool
nonEmptyHunksFLPair (ps :> qs) = allFL nonEmptyHunk ps && allFL nonEmptyHunk qs

type instance ModelOf Prim = V1Model
instance ArbitraryPrim Prim

instance NullPatch Prim where
  nullPatch (FP _ fp) = nullPatch fp
  nullPatch p | IsEq <- isIdentity p = IsEq
  nullPatch _ = NotEq

instance NullPatch FilePatchType where
  nullPatch (Hunk _ [] []) = unsafeCoerceP IsEq -- is this safe?
  nullPatch _ = NotEq

instance Arbitrary (Sealed2 (FL (WithState V1Model Prim))) where
  arbitrary = do repo <- ourSmallRepo
                 liftM (unseal (seal2 . wesPatch)) $ arbitraryState repo

-- instance Show1 (TreeWithFlattenPos Prim) where
--   showDict1 = ShowDictClass

-- WithState and propFail are handy for debugging arbitrary code
propFail :: Int -> Tree Prim C(x) -> Bool
propFail n xs = sizeTree xs < n

----------------------------------------------------------------------
-- * QuickCheck generators

----------------------------------------------------------------------
-- ** FilePatchType generators

aHunk :: FORALL(x y) Content -> Gen (FilePatchType C(x y))
aHunk content
 = sized $ \n ->
     do pos <- choose (1, contentLen+1)
        let prefixLen = pos-1
            restLen   = contentLen-prefixLen
        oldLen <- frequency
                      [ (75, choose (0, min restLen n))
                        -- produces small hunks common in real editing
                      , (25, choose (0, min 10 restLen))
                      ]
        -- newLen choice aims to cover all possibilities, that is,
        -- remove less/the same/more than added and empty the file.
        newLen <- frequency
                      [ ( 54
                        , choose (1,min 1 n)
                        )
                      , ( if oldLen /= 0 then 42 else 0
                        , choose (1,min 1 oldLen)
                        )
                      , ( if oldLen /= 0 then 2 else 0
                        , return oldLen
                        )
                      , ( if oldLen /= 0 then 2 else 0
                        , return 0
                        )
                      ]
        new <- vectorOf newLen aLine
        let old = take oldLen $ drop prefixLen $ content
        return $ Hunk pos old new
  where
      contentLen = length content

aTokReplace :: FORALL(x y) Content -> Gen (FilePatchType C(x y))
aTokReplace []
  = do w <- vectorOf 1 alpha
       w' <- vectorOf 1 alpha
       return $ TokReplace defaultToks w w'
aTokReplace content
  = do let fileWords = concatMap BC.words content
       wB <- elements fileWords
       w' <- alphaBS `notIn` fileWords
       return $ TokReplace defaultToks (BC.unpack wB) (BC.unpack w')
  where
      alphaBS = do x <- alpha; return $ BC.pack [x]

----------------------------------------------------------------------
-- ** Prim generators

aHunkP :: FORALL(x y) (AnchoredPath,File) -> Gen (Prim C(x y))
aHunkP (path,file)
  = do Hunk pos old new <- aHunk content
       return $ hunk (ap2fp path) pos old new
  where
      content = fileContent file

aTokReplaceP :: FORALL (x y) (AnchoredPath,File) -> Gen (Prim C(x y))
aTokReplaceP (path,file)
  = do TokReplace tokchars old new <- aTokReplace content
       return $ tokreplace (ap2fp path) tokchars old new
  where
      content = fileContent file

anAddFileP :: FORALL (x y) (AnchoredPath,Dir) -> Gen (Prim C(x y))
anAddFileP (path,dir)
  = do newFilename <- aFilename `notIn` existing
       let newPath = path `appendPath` newFilename
       return $ addfile (ap2fp newPath)
  where
      existing = map fst $ filterFiles $ dirContent dir

aRmFileP :: FORALL (x y) AnchoredPath   -- ^ Path of an empty file
                          -> Prim C(x y)
aRmFileP path = rmfile (ap2fp path)

anAddDirP :: FORALL (x y) (AnchoredPath,Dir) -> Gen (Prim C(x y))
anAddDirP (path,dir)
  = do newDirname <- aDirname `notIn` existing
       let newPath = path `appendPath` newDirname
       return $ adddir (ap2fp newPath)
  where
      existing = map fst $ filterDirs $ dirContent dir

aRmDirP :: FORALL (x y) AnchoredPath    -- ^ Path of an empty directory
                        -> Prim C(x y)
aRmDirP path = rmdir (ap2fp path)

aMoveP :: FORALL (x y) Gen Name -> AnchoredPath -> (AnchoredPath,Dir) -> Gen (Prim C(x y))
aMoveP nameGen oldPath (dirPath,dir)
  = do newName <- nameGen `notIn` existing
       let newPath = dirPath `appendPath` newName
       return $ move (ap2fp oldPath) (ap2fp newPath)
  where
      existing = map fst $ dirContent dir

-- | Generates any type of 'Prim' patch, except binary and setpref patches.
aPrim :: FORALL(x y) V1Model C(x) -> Gen (WithEndState V1Model (Prim C(x)) C(y))
aPrim repo
  = do mbFile <- maybeOf repoFiles
       mbEmptyFile <- maybeOf $ filter (isEmpty . snd) repoFiles
       dir  <- elements (rootDir:repoDirs)
       mbOldDir <- maybeOf repoDirs
       mbEmptyDir <- maybeOf $ filter (isEmpty . snd) repoDirs
       patch <- frequency
                  [ ( if isJust mbFile then 12 else 0
                    , aHunkP $ fromJust mbFile
                    )
                  , ( if isJust mbFile then 6 else 0
                    , aTokReplaceP $ fromJust mbFile
                    )
                  , ( 2
                    , anAddFileP dir
                    )
                  , ( if isJust mbEmptyFile then 12 else 0
                    , return $ aRmFileP $ fst $ fromJust mbEmptyFile
                    )
                  , ( 2
                    , anAddDirP dir
                    )
                  , ( if isJust mbEmptyDir then 10 else 0
                    , return $ aRmDirP $ fst $ fromJust mbEmptyDir
                    )
                  , ( if isJust mbFile then 3 else 0
                    , aMoveP aFilename (fst $ fromJust mbFile) dir
                    )
                  , let oldPath = fst $ fromJust mbOldDir in
                    ( if isJust mbOldDir
                         && not (oldPath `isPrefix` fst dir)
                        then 4 else 0
                    , aMoveP aDirname oldPath dir
                    )
                  ]
       let repo' = unFail $ repoApply repo patch
       return $ WithEndState patch repo'
  where
      repoItems = list repo
      repoFiles = filterFiles repoItems
      repoDirs  = filterDirs repoItems
      rootDir   = (anchoredRoot,root repo)

{- [COVERAGE OF aPrim]

  PLEASE,
  if you change something that may affect the coverage of aPrim then
      a) recalculate it, or if that is not possible;
      b) indicate the need to do it.

  Patch type
  ----------
  42% hunk
  22% tokreplace
  14% move
   6% rmdir
   6% addfile
   6% adddir
   4% rmfile
-}

----------------------------------------------------------------------
-- *** Pairs of primitive patches

-- Try to generate commutable pairs of hunks
hunkPairP :: FORALL(x y) (AnchoredPath,File) -> Gen ((Prim :> Prim) C(x y))
hunkPairP (path,file)
  = do h1@(Hunk l1 old1 new1) <- aHunk content
       (delta, content') <- selectChunk h1 content
       Hunk l2' old2 new2 <- aHunk content'
       let l2 = l2'+delta
       return (hunk fpPath l1 old1 new1 :> hunk fpPath l2 old2 new2)
  where
      content = fileContent file
      fpPath = ap2fp path
      selectChunk (Hunk l old new) content_
        = elements [prefix, suffix]
        where
            start = l - 1
            prefix = (0, take start content_)
            suffix = (start + length new, drop (start + length old) content_)
      selectChunk _ _ = impossible

aPrimPair :: FORALL(x y) V1Model C(x) -> Gen (WithEndState V1Model ((Prim :> Prim) C(x)) C(y))
aPrimPair repo
  = do mbFile <- maybeOf repoFiles
       frequency
          [ ( if isJust mbFile then 1 else 0
            , do p1 :> p2 <- hunkPairP $ fromJust mbFile
                 let repo'  = unFail $ repoApply repo p1
                     repo'' = unFail $ repoApply repo' p2
                 return $ WithEndState (p1 :> p2) repo''
            )
          , ( 1
            , do Sealed wesP <- arbitraryState repo
                 return $ unsafeCoerceP1 wesP
            )
          ]
  where
      repoItems = list repo
      repoFiles = filterFiles repoItems

{- [COVERAGE OF aPrimPair]

  PLEASE,
  if you change something that may affect the coverage of aPrimPair then
      a) recalculate it, or if that is not possible;
      b) indicate the need to do it.

  Rate of ommutable pairs
  -----------------------
  67% commutable

  Commutable coverage (for 1000 tests)
  -------------------
  21% hunks-B
  20% hunks-A
  14% file:>dir
  12% file:>move
   8% trivial-FP
   8% hunk:>tok
   4% hunks-D
   3% tok:>tok
   2% hunks-C
   1% move:>move
   1% dir:>move
   1% dir:>dir
   0% emptyhunk:>file
-}

----------------------------------------------------------------------
-- Arbitrary instances

ourSmallRepo :: Gen (V1Model C(x))
ourSmallRepo = aSmallRepo

instance ArbitraryState V1Model Prim where
  arbitraryState s = seal <$> aPrim s


instance Arbitrary (Sealed2 Prim) where
  arbitrary = makeS2Gen ourSmallRepo

instance Arbitrary (Sealed2 (Prim :> Prim)) where
  arbitrary = do repo <- ourSmallRepo
                 WithEndState pp _ <- aPrimPair repo
                 return $ seal2 pp

instance Arbitrary (Sealed ((Prim :> Prim) C(a))) where
  arbitrary = do repo <- ourSmallRepo
                 WithEndState pp _ <- aPrimPair repo
                 return $ seal pp

instance Arbitrary (Sealed2 (Prim :> Prim :> Prim)) where
  arbitrary = makeS2Gen ourSmallRepo

instance Arbitrary (Sealed ((Prim :> Prim :> Prim) a)) where
  arbitrary = makeSGen ourSmallRepo

instance Arbitrary (Sealed2 (FL Prim)) where
  arbitrary = makeS2Gen ourSmallRepo

instance Arbitrary (Sealed ((FL Prim) C(a))) where
  arbitrary = makeSGen ourSmallRepo

instance Arbitrary (Sealed2 (FL Prim :> FL Prim)) where
  arbitrary = makeS2Gen ourSmallRepo

instance Arbitrary (Sealed ((FL Prim :> FL Prim) C(a))) where
  arbitrary = makeSGen ourSmallRepo

instance Arbitrary (Sealed2 (WithState V1Model Prim)) where
  arbitrary = makeWS2Gen ourSmallRepo

instance Arbitrary (Sealed (WithState V1Model Prim C(a))) where
  arbitrary = makeWSGen ourSmallRepo

instance Arbitrary (Sealed (WithState V1Model (FL Prim) C(a))) where
  arbitrary = makeWSGen ourSmallRepo

instance Arbitrary (Sealed2 (WithState V1Model (Prim :> Prim))) where
  arbitrary = do repo <- ourSmallRepo
                 WithEndState pp repo' <- aPrimPair repo
                 return $ seal2 $ WithState repo pp repo'

instance Arbitrary (Sealed (WithState V1Model (Prim :> Prim) a)) where
  arbitrary = do repo <- ourSmallRepo
                 WithEndState pp repo' <- aPrimPair repo
                 return $ seal $ WithState repo pp repo'


instance Arbitrary (Sealed2 (WithState V1Model (FL Prim))) where
  arbitrary = makeWS2Gen ourSmallRepo

instance Arbitrary (Sealed2 (WithState V1Model (FL Prim :> FL Prim))) where
  arbitrary = makeWS2Gen ourSmallRepo

instance Arbitrary (Sealed (WithState V1Model (FL Prim :> FL Prim) a)) where
  arbitrary = makeWSGen ourSmallRepo
