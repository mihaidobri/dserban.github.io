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

module Darcs.Commands.Test ( test ) where

import Prelude hiding ( catch )

import System.Exit ( exitWith )

import Darcs.Commands ( DarcsCommand(..), nodefaults, putInfo )
import Darcs.Arguments ( DarcsFlag(Test)
                       , leaveTestDir
                       , workingRepoDir
                       )
import Darcs.Repository ( Repository, amInHashedRepository, withRepository,
                          testRecorded, RepoJob(..) )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Apply( ApplyState )
import Printer ( text )
import Storage.Hashed.Tree( Tree )

#include "gadts.h"

testDescription :: String
testDescription = "Run regression test."

testHelp :: String
testHelp =
 "If a regression test is defined (see `darcs setpref') it will be run.\n"

test :: DarcsCommand
test = DarcsCommand  {commandProgramName = "darcs",
                      commandName = "test",
                      commandHelp = testHelp,
                      commandDescription = testDescription,
                      commandExtraArgs = 0,
                      commandExtraArgHelp = [],
                      commandCommand = testCmd,
                      commandPrereq = amInHashedRepository,
                      commandGetArgPossibilities = return [],
                      commandArgdefaults = nodefaults,
                      commandAdvancedOptions = [],
                      commandBasicOptions = [ leaveTestDir,
                                              workingRepoDir
                                             ]}

testCmd :: [DarcsFlag] -> [String] -> IO ()
testCmd opts _ = withRepository (Test:opts) (RepoJob (test' opts))

test'
  :: forall p C(r u t) . (RepoPatch p, ApplyState p ~ Tree)
  => [DarcsFlag] -> Repository p C(r u t) -> IO ()
test' opts repository = do
    putInfo opts $ text "Running test on current repository state."
    rc <- testRecorded repository
    exitWith rc
