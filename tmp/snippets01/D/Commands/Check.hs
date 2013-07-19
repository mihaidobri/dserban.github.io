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

module Darcs.Commands.Check ( check, repair ) where

import Prelude hiding ( catch )

import Control.Monad ( when, unless )
import Control.Applicative( (<$>) )
import Control.Exception ( catch, IOException )
import System.Exit ( ExitCode(..), exitWith )
import System.Directory( renameFile )

import Darcs.Commands ( DarcsCommand(..), nodefaults, putInfo )
import Darcs.Arguments ( DarcsFlag(Quiet),
                         test, umaskOption,
                        leaveTestDir, workingRepoDir, ignoretimes
                      )
import Darcs.Flags(willIgnoreTimes)
import Darcs.Repository.Repair( replayRepository, checkIndex,
                                replayRepositoryInTemp,
                                RepositoryConsistency(..) )
import Darcs.Repository ( Repository, amInHashedRepository, withRepository,
                          testRecorded, readRecorded, RepoJob(..),
                          withRepoLock, replacePristine, writePatchSet )
import Darcs.Patch ( RepoPatch, showPatch, PrimOf )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Witnesses.Ordered ( FL(..) )
import Darcs.Witnesses.Sealed ( Sealed(..), unFreeLeft )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Diff( treeDiff )
import Printer ( text, ($$), (<+>) )
import Storage.Hashed.Tree( Tree )

#include "gadts.h"

checkDescription :: String
checkDescription = "Check the repository for consistency."

checkHelp :: String
checkHelp =
 "This command verifies that the patches in the repository, when applied\n" ++
 "successively to an empty tree, result in the pristine tree.  If not,\n" ++
 "the differences are printed and Darcs exits unsucessfully (with a\n" ++
 "non-zero exit status).\n" ++
 "\n" ++
 "If a regression test is defined (see `darcs setpref') it will be run\n" ++
 "by `darcs check'.  Use the --no-test option to disable this.\n"

check :: DarcsCommand
check = DarcsCommand {commandProgramName = "darcs",
                      commandName = "check",
                      commandHelp = checkHelp,
                      commandDescription = checkDescription,
                      commandExtraArgs = 0,
                      commandExtraArgHelp = [],
                      commandCommand = checkCmd,
                      commandPrereq = amInHashedRepository,
                      commandGetArgPossibilities = return [],
                      commandArgdefaults = nodefaults,
                      commandAdvancedOptions = [],
                      commandBasicOptions = [ test,
                                              leaveTestDir,
                                              workingRepoDir,
                                               ignoretimes
                                             ]}

checkCmd :: [DarcsFlag] -> [String] -> IO ()
checkCmd opts _ = withRepository opts (RepoJob (check' opts))

check'
  :: forall p C(r u t) . (RepoPatch p, ApplyState p ~ Tree)
  => [DarcsFlag] -> Repository p C(r u t) -> IO ()
check' opts repository = do
    state <- replayRepositoryInTemp repository opts
    failed <-
      case state of
        RepositoryConsistent -> do
          putInfo opts $ text "The repository is consistent!"
          rc <- testRecorded repository
          when (rc /= ExitSuccess) $ exitWith rc
          return False
        BrokenPristine newpris -> do
          brokenPristine newpris
          return True
        BrokenPatches newpris _ -> do
          brokenPristine newpris
          putInfo opts $ text "Found broken patches."
          return True
    bad_index <- case willIgnoreTimes opts of
                   False -> not <$> checkIndex repository (Quiet `elem` opts)
                   True -> return False
    when bad_index $ putInfo opts $ text "Bad index."
    exitWith $ if failed || bad_index then ExitFailure 1 else ExitSuccess
   where
     brokenPristine newpris = do
         putInfo opts $ text "Looks like we have a difference..."
         mc' <- (fmap Just $ readRecorded repository) `catch` (\(_ :: IOException) -> return Nothing)
         case mc' of
           Nothing -> do putInfo opts $ text "cannot compute that difference, try repair"
                         putInfo opts $ text "" $$ text "Inconsistent repository"
                         return ()
           Just mc -> do
               ftf <- filetypeFunction
               Sealed (diff :: FL (PrimOf p) C(r r2)) <- unFreeLeft `fmap` treeDiff ftf newpris mc :: IO (Sealed (FL (PrimOf p) C(r)))
               putInfo opts $ case diff of
                        NilFL -> text "Nothing"
                        patch -> text "Difference: " <+> showPatch patch
               putInfo opts $ text ""
                     $$ text "Inconsistent repository!"


repairDescription :: String
repairDescription = "Repair a corrupted repository."

repairHelp :: String
repairHelp =
 "The `darcs repair' command attempts to fix corruption in the current\n" ++
 "repository.  Currently it can only repair damage to the pristine tree,\n" ++
 "which is where most corruption occurs.\n"

repair :: DarcsCommand
repair = DarcsCommand {commandProgramName = "darcs",
                       commandName = "repair",
                       commandHelp = repairHelp,
                       commandDescription = repairDescription,
                       commandExtraArgs = 0,
                       commandExtraArgHelp = [],
                       commandCommand = repairCmd,
                       commandPrereq = amInHashedRepository,
                       commandGetArgPossibilities = return [],
                       commandArgdefaults = nodefaults,
                       commandAdvancedOptions = [umaskOption],
                       commandBasicOptions = [workingRepoDir]}

repairCmd :: [DarcsFlag] -> [String] -> IO ()
repairCmd opts _ = withRepoLock opts $ RepoJob $ \repository -> do
  replayRepository repository opts $ \state ->
    case state of
      RepositoryConsistent ->
          putStrLn "The repository is already consistent, no changes made."
      BrokenPristine tree -> do
               putStrLn "Fixing pristine tree..."
               replacePristine repository tree
      BrokenPatches tree newps  -> do
               putStrLn "Writing out repaired patches..."
               _ <- writePatchSet newps opts
               putStrLn "Fixing pristine tree..."
               replacePristine repository tree
  index_ok <- checkIndex repository (Quiet `elem` opts)
  unless index_ok $ do renameFile "_darcs/index" "_darcs/index.bad"
                       putStrLn "Bad index discarded."

