--  Copyright (C) 2002-2003,2005 David Roundy
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

module Darcs.Commands.MarkConflicts ( markconflicts ) where

import Prelude hiding ( catch )

import System.Exit ( ExitCode(..), exitWith )
import Darcs.SignalHandler ( withSignalsBlocked )
import Control.Monad ( unless )
import Control.Exception ( catch, IOException )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag, ignoretimes, workingRepoDir, umaskOption )
import Darcs.Flags ( diffingOpts )
import Darcs.Repository ( withRepoLock, RepoJob(..), amInHashedRepository, addToPending,
                    applyToWorking,
                    readRepo, unrecordedChanges, Repository
                    )
import Darcs.Patch ( invert, PrimOf )
import Darcs.Witnesses.Ordered ( FL(..) )
import Darcs.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Resolution ( patchsetConflictResolutions )
import Darcs.Utils ( promptYorn )
#include "impossible.h"
#include "gadts.h"

markconflictsDescription :: String
markconflictsDescription =
 "Mark unresolved conflicts in working tree, for manual resolution."

markconflictsHelp :: String
markconflictsHelp =
 "Darcs requires human guidance to unify changes to the same part of a\n" ++
 "source file.  When a conflict first occurs, darcs will add the\n" ++
 "initial state and both choices to the working tree, delimited by the\n" ++
 "markers `v v v', `=====',  `* * *' and `^ ^ ^', as follows:\n" ++
 "\n" ++
 "v v v v v v v\n" ++
 "Initial state.\n" ++
 "=============\n" ++
 "First choice.\n" ++
 "*************\n" ++
 "Second choice.\n" ++
 "^ ^ ^ ^ ^ ^ ^\n" ++
 "\n" ++
 "However, you might revert or manually delete these markers without\n" ++
 "actually resolving the conflict.  In this case, `darcs mark-conflicts'\n" ++
 "is useful to show where any unresolved conflicts.  It is also useful\n" ++
 "if `darcs apply' is called with --apply-conflicts, where conflicts\n" ++
 "aren't marked initially.\n" ++
 "\n" ++
 "Any unrecorded changes to the working tree WILL be lost forever when\n" ++
 "you run this command!  You will be prompted for confirmation before\n" ++
 "this takes place.\n"

markconflicts :: DarcsCommand
markconflicts = DarcsCommand {commandProgramName = "darcs",
                              commandName = "mark-conflicts",
                              commandHelp = markconflictsHelp,
                              commandDescription = markconflictsDescription,
                              commandExtraArgs = 0,
                              commandExtraArgHelp = [],
                              commandCommand = markconflictsCmd,
                              commandPrereq = amInHashedRepository,
                              commandGetArgPossibilities = return [],
                              commandArgdefaults = nodefaults,
                              commandAdvancedOptions = [umaskOption],
                              commandBasicOptions = [ignoretimes,
                                                      workingRepoDir]}

markconflictsCmd :: [DarcsFlag] -> [String] -> IO ()
markconflictsCmd opts [] = withRepoLock opts $ RepoJob $ \(repository :: Repository p C(r u r)) -> do
  pend <- unrecordedChanges (diffingOpts opts) repository Nothing
  r <- readRepo repository
  Sealed res <- return $ patchsetConflictResolutions r
  (case res of NilFL -> do putStrLn "No conflicts to mark."
                           exitWith ExitSuccess
               _ -> return ()) :: IO ()
  let undoUnrec :: FL (PrimOf p) C(r u) -> IO (Repository p C(r r r))
      undoUnrec NilFL = return repository
      undoUnrec pend' =
              do putStrLn ("This will trash any unrecorded changes"++
                          " in the working directory.")
                 confirmed <- promptYorn "Are you sure? "
                 unless confirmed $ exitWith ExitSuccess
                 applyToWorking repository opts (invert pend') `catch` \(e :: IOException) ->
                    bug ("Can't undo pending changes!" ++ show e)
  repository' <- undoUnrec pend
  withSignalsBlocked $
    do addToPending repository' res
       _ <- applyToWorking repository' opts res `catch` \(e :: IOException) ->
           bug ("Problem marking conflicts in mark-conflicts!" ++ show e)
       return ()
  putStrLn "Finished marking conflicts."
markconflictsCmd _ _ = impossible

