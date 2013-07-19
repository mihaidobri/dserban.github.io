--  Copyright (C) 2003 David Roundy, 2010-2011 Petr Rockai
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
{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -cpp #-}

module Darcs.Commands.Annotate ( annotate ) where

import Control.Monad ( when )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag(..), workingRepoDir,
                         summary, unified, machineReadable,
                        creatorhash,
                        fixSubPaths,
                        listRegisteredFiles,
                        matchOne,
                      )
import Darcs.Flags ( isUnified )
import Storage.Hashed.Plain( readPlainTree )
import Darcs.Repository.State ( readRecorded )
import Darcs.Repository ( Repository, amInHashedRepository, withRepository, RepoJob(..), readRepo )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Patch ( RepoPatch, Named, patch2patchinfo, invertRL )
import Darcs.Patch.Apply( ApplyState )
import qualified Darcs.Patch ( summary )
import qualified Data.ByteString.Char8 as BC ( pack, concat, intercalate )
import Data.ByteString.Lazy ( toChunks )
import Darcs.PrintPatch ( printPatch, contextualPrintPatch )
import Darcs.Patch.ApplyMonad( withFileNames )
import Darcs.Patch.FileName( fp2fn )
import System.FilePath( (</>) )
import Darcs.RepoPath( toFilePath )
import Darcs.Patch.Info ( humanFriendly, showPatchInfo )
import Darcs.Match ( matchPatch, haveNonrangeMatch, getFirstMatch, getOnePatchset, getNonrangeMatchS )
import Darcs.Lock ( withTempDir )
import Darcs.Witnesses.Sealed ( Sealed2(..), Sealed(..), seal )
import qualified Darcs.Annotate as A
import Printer ( putDocLn, Doc )

import Storage.Hashed.Tree( Tree, TreeItem(..), readBlob, list, expand )
import Storage.Hashed.Monad( findM, virtualTreeIO )
import Storage.Hashed.AnchoredPath( floatPath, anchorPath )
#include "gadts.h"
#include "impossible.h"

annotateDescription :: String
annotateDescription = "Display which patch last modified something."

annotateHelp :: String
annotateHelp =
 "The `darcs annotate' command provides two unrelated operations.  When\n" ++
 "called on a file, it will find the patch that last modified each line\n" ++
 "in that file.  When called on a patch (e.g. using --patch), it will\n" ++
 "print the internal representation of that patch.\n" ++
 "\n" ++
 "The --summary option will result in a summarized patch annotation,\n" ++
 "similar to `darcs whatsnew'.  It has no effect on file annotations.\n" ++
 "\n" ++
 "By default, output is in a human-readable format.  The --machine-readable\n" ++
 "option can be used to generate output for machine postprocessing.\n"

annotate :: DarcsCommand
annotate = DarcsCommand {commandProgramName = "darcs",
                         commandName = "annotate",
                         commandHelp = annotateHelp,
                         commandDescription = annotateDescription,
                         commandExtraArgs = -1,
                         commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                         commandCommand = annotateCmd,
                         commandPrereq = amInHashedRepository,
                         commandGetArgPossibilities = listRegisteredFiles,
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [],
                         commandBasicOptions = [summary,unified,
                                                 machineReadable,
                                                 matchOne, creatorhash,
                                                 workingRepoDir]}

annotateCmd :: [DarcsFlag] -> [String] -> IO ()
annotateCmd opts files = withRepository opts (RepoJob (annotate' opts files))

annotate' :: (RepoPatch p, ApplyState p ~ Tree)
          => [DarcsFlag] -> [String] -> Repository p C(r u r) -> IO ()

annotate' opts [] repository = do
  when (not $ haveNonrangeMatch opts) $
      fail $ "Annotate requires either a patch pattern or a " ++
               "file or directory argument."
  Sealed2 p <- matchPatch opts `fmap` readRepo repository
  if Summary `elem` opts
     then do putDocLn $ showpi $ patch2patchinfo p
             putDocLn $ show_summary p
     else if isUnified opts
          then withTempDir "context" $ \_ ->
               do getFirstMatch repository opts
                  c <- readPlainTree "."
                  contextualPrintPatch c p
          else printPatch p
    where showpi | MachineReadable `elem` opts = showPatchInfo
                 | otherwise                   = humanFriendly
          show_summary :: RepoPatch p => Named p C(x y) -> Doc
          show_summary = Darcs.Patch.summary

annotate' opts [""] repository = annotate' opts [] repository
annotate' opts args@[_] repository = do
  r <- readRepo repository
  (origpath:_) <- fixSubPaths opts args
  recorded <- readRecorded repository

  (Sealed patches, initial, path) <-
    if haveNonrangeMatch opts
       then do Sealed x <- getOnePatchset repository opts
               let fn = [fp2fn $ toFilePath origpath]
                   nonRangeMatch = getNonrangeMatchS opts r
                   (_, [path], _) = withFileNames Nothing fn nonRangeMatch
               initial <- snd `fmap` virtualTreeIO (getNonrangeMatchS opts r) recorded
               return $ (seal $ newset2RL x, initial, toFilePath path)
       else return $ (seal $ newset2RL r, recorded, toFilePath origpath)

  found <- findM initial (floatPath $ toFilePath path)
  -- TODO need to decide about the --machine flag
  let fmt = if MachineReadable `elem` opts then A.machineFormat else A.format
  case found of
    Nothing -> fail $ "No such file or directory: " ++ toFilePath path
    Just (SubTree s) -> do
      s' <- expand s
      let subs = map (fp2fn . (path </>) . anchorPath "" . fst) $ list s'
          showPath (n, File _) = BC.pack (path </> n)
          showPath (n, _) = BC.concat [BC.pack (path </> n), "/"]
      putStrLn $ fmt (BC.intercalate "\n" $ map showPath $
                        map (\(x,y) -> (anchorPath "" x, y)) $ list s') $
        A.annotateDirectory (invertRL patches) (fp2fn $ "./" ++ path) subs
    Just (File b) -> do con <- BC.concat `fmap` toChunks `fmap` readBlob b
                        putStrLn $ fmt con $ A.annotate (invertRL patches) (fp2fn $ "./" ++ path) con
    Just (Stub _ _) -> impossible

annotate' _ _ _ = fail "annotate accepts at most one argument"
