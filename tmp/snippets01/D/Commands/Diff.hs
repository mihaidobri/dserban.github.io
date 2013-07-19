--  Copyright (C) 2003-2004 David Roundy
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

{-# LANGUAGE CPP #-}

module Darcs.Commands.Diff ( diffCommand ) where

import System.FilePath.Posix ( takeFileName )
import Workaround ( getCurrentDirectory )
import Darcs.Utils ( askEnter, withCurrentDirectory )
import Control.Monad ( when )
import Data.List ( (\\) )

import Storage.Hashed.Plain( writePlainTree )

import Darcs.External( diffProgram )
import CommandLine ( parseCmd )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments
    ( DarcsFlag(AfterPatch, DiffCmd, DiffFlags, LastN)
    , diffCmdFlag
    , diffflags
    , fixSubPaths
    , matchRange
    , pauseForGui
    , storeInMemory
    , unidiff
    , workingRepoDir
    )
import Darcs.Flags ( isNotUnified, wantGuiPause )
import Darcs.Patch.PatchInfoAnd ( info )
import Darcs.RepoPath ( AbsolutePath, SubPath, toFilePath, sp2fn )
import Darcs.Match ( getPartialFirstMatch, getPartialSecondMatch,
                     firstMatch, secondMatch,
                     matchFirstPatchset, matchSecondPatchset )
import Darcs.Repository ( withRepository, RepoJob(..), readRepo,
                          amInHashedRepository,
                          createPristineDirectoryTree,
                          createPartialsPristineDirectoryTree )
import Darcs.Patch.Set ( PatchSet, newset2RL )
import Darcs.Repository.State ( readUnrecorded )
import Darcs.Patch ( RepoPatch )
import Darcs.Witnesses.Ordered ( mapRL )
import Darcs.Patch.Info ( PatchInfo, humanFriendly )
import Darcs.External ( execPipeIgnoreError )
import Darcs.Lock ( withTempDir )
import Darcs.Witnesses.Sealed ( unseal )
import Printer ( Doc, putDoc, vcat, empty, ($$) )
#include "impossible.h"

#include "gadts.h"

diffDescription :: String
diffDescription = "Create a diff between two versions of the repository."

diffHelp :: String
diffHelp =
 "The `darcs diff' command compares two versions of the working tree of\n" ++
 "the current repository.  Without options, the pristine (recorded) and\n" ++
 "unrecorded working trees are compared.  This is lower-level than\n" ++
 "the `darcs whatsnew' command, since it outputs a line-by-line diff,\n" ++
 "and it is also slower.  As with `darcs whatsnew', if you specify\n" ++
 "files or directories, changes to other files are not listed.\n" ++
 "The command always uses an external diff utility.\n" ++
 "\n" ++
 "With the --patch option, the comparison will be made between working\n" ++
 "trees with and without that patch.  Patches `after' the selected patch\n" ++
 "are not present in either of the compared working trees.  The\n" ++
 "--from-patch and --to-patch options allow the set of patches in the\n" ++
 "`old' and `new' working trees to be specified separately.\n" ++
 "\n" ++
 "The associated tag and match options are also understood, e.g. `darcs\n" ++
 "diff --from-tag 1.0 --to-tag 1.1'.  All these options assume an\n" ++
 "ordering of the patch set, so results may be affected by operations\n" ++
 "such as `darcs optimize --reorder'.\n" ++
 "\n" ++
 "diff(1) is called with the arguments -rN.  The --unified option causes\n" ++
 "-u to be passed to diff(1).  An additional argument can be passed\n" ++
 "using --diff-opts, such as --diff-opts=-ud or --diff-opts=-wU9.\n" ++
 "\n" ++
 "The --diff-command option can be used to specify an alternative\n" ++
 "utility, such as meld (GNOME) or opendiff (OS X).  Arguments may be\n" ++
 "included, separated by whitespace.  The value is not interpreted by a\n" ++
 "shell, so shell constructs cannot be used.  The arguments %1 and %2\n" ++
 "MUST be included, these are substituted for the two working trees\n" ++
 "being compared.  If this option is used, --diff-opts is ignored.\n"

diffCommand :: DarcsCommand
diffCommand = DarcsCommand {commandProgramName = "darcs",
                             commandName = "diff",
                             commandHelp = diffHelp,
                             commandDescription = diffDescription,
                             commandExtraArgs = -1,
                             commandExtraArgHelp
                                 = ["[FILE or DIRECTORY]..."],
                             commandCommand = diffCmd,
                             commandPrereq = amInHashedRepository,
                             commandGetArgPossibilities = return [],
                             commandArgdefaults = nodefaults,
                             commandAdvancedOptions =
                                [ pauseForGui
                                ],
                             commandBasicOptions =
                                [ matchRange
                                , diffCmdFlag
                                , diffflags
                                , unidiff
                                , workingRepoDir
                                , storeInMemory
                                ]
                           }

getDiffOpts :: [DarcsFlag] -> [String]
getDiffOpts opts | isNotUnified opts = get_nonU_diff_opts opts
                 | otherwise         = "-u" : get_nonU_diff_opts opts
    where get_nonU_diff_opts (DiffFlags f:fs) = f : get_nonU_diff_opts fs
          get_nonU_diff_opts (_:fs) = get_nonU_diff_opts fs
          get_nonU_diff_opts [] = []

-- | Returns the command we should use for diff as a tuple (command, arguments).
-- This will either be whatever the user specified via --diff-command  or the
-- default 'diffProgram'.  Note that this potentially involves parsing the
-- user's diff-command, hence the possibility for failure with an exception.
getDiffCmdAndArgs :: String -> [DarcsFlag] -> String -> String
                      -> Either String (String, [String])
getDiffCmdAndArgs cmd opts f1 f2 = helper opts where
  helper (DiffCmd c:_) =
    case parseCmd [ ('1', f1) , ('2', f2) ] c of
    Left err        -> Left $ show err
    Right ([],_)    -> bug $ "parseCmd should never return empty list"
    Right ((h:t),_) -> Right (h,t)
  helper [] = -- if no command specified, use 'diff'
    Right (cmd, ("-rN":getDiffOpts opts++[f1,f2]))
  helper (_:t) = helper t

diffCmd :: [DarcsFlag] -> [String] -> IO ()
diffCmd opts args
  | not (null [i | LastN i <- opts]) &&
      not (null [p | AfterPatch p <- opts]) =
        fail $ "using --patch and --last at the same time with the 'diff'" ++
          " command doesn't make sense. Use --from-patch to create a diff" ++
          " from this patch to the present, or use just '--patch' to view" ++
          " this specific patch."
  | null args = doDiff opts Nothing
  | otherwise = doDiff opts . Just =<< fixSubPaths opts args

doDiff :: [DarcsFlag] -> Maybe [SubPath] ->  IO ()
doDiff opts sps = withRepository opts $ RepoJob $ \repository -> do
  let pathList = map sp2fn `fmap` sps
  formerdir <- getCurrentDirectory
  withTempDirs (takeFileName formerdir) $ \odir ndir -> do
    if firstMatch opts
      then withCurrentDirectory odir $ getPartialFirstMatch repository opts pathList
      else case pathList of
        Nothing -> createPristineDirectoryTree repository $ toFilePath odir
        Just pl -> createPartialsPristineDirectoryTree repository pl $ toFilePath odir
    if secondMatch opts
       then withCurrentDirectory ndir $ getPartialSecondMatch repository opts pathList
       else withCurrentDirectory formerdir $
               readUnrecorded repository sps >>= (flip writePlainTree (toFilePath ndir))
    thediff <- withCurrentDirectory (toFilePath odir ++ "/..") $
                   case pathList of
                   Nothing -> rundiff (takeFileName $ toFilePath odir) (takeFileName $ toFilePath ndir)
                   Just fs -> vcat `fmap`
                         mapM (\f -> rundiff
                               (takeFileName (toFilePath odir) ++ "/" ++ toFilePath f)
                               (takeFileName (toFilePath ndir) ++ "/" ++ toFilePath f)) fs
    morepatches <- readRepo repository
    putDoc $ changelog (getDiffInfo opts morepatches)
            $$ thediff
    where rundiff :: String -> String -> IO Doc
          rundiff f1 f2 = do
            cmd <- diffProgram
            case getDiffCmdAndArgs cmd opts f1 f2 of
             Left err -> fail err
             Right (d_cmd, d_args) ->
              let pausingForGui = wantGuiPause opts in
              do when pausingForGui $ putStrLn $
                   "Running command '" ++ unwords (d_cmd:d_args) ++ "'"
                 output <- execPipeIgnoreError d_cmd d_args empty
                 when pausingForGui $
                    askEnter "Hit return to move on..."
                 return output

          withTempDirs :: String -> (AbsolutePath -> AbsolutePath -> IO a) -> IO a
          withTempDirs x f = withTempDir ("old-" ++ x) $ \odir ->
            withTempDir ("new-" ++ x) $ \ndir -> f odir ndir

getDiffInfo :: RepoPatch p => [DarcsFlag] -> PatchSet p C(start x) -> [PatchInfo]
getDiffInfo opts ps =
    let infos = mapRL info . newset2RL
        handle (match_cond, do_match)
          | match_cond opts = unseal infos (do_match opts ps)
          | otherwise = infos ps
    in handle (secondMatch, matchSecondPatchset)
         \\ handle (firstMatch, matchFirstPatchset)

changelog :: [PatchInfo] -> Doc
changelog pis = vcat $ map humanFriendly pis

