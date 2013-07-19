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

module Darcs.Test ( getTest,
                    runPosthook, runPrehook )
where
import Darcs.RepoPath ( AbsolutePath )
import Darcs.Utils ( withCurrentDirectory )
import System.Exit ( ExitCode(..) )
import System.Cmd ( system )
import Control.Monad ( when )

import Darcs.Arguments ( DarcsFlag( Quiet,
                                    AskPosthook, AskPrehook ),
                        getPosthookCmd, getPrehookCmd )
import Darcs.Repository.Prefs ( getPrefval )
import Darcs.Utils ( askUser )
import System.IO ( hPutStrLn, stderr )

getTest :: [DarcsFlag] -> IO (IO ExitCode)
getTest opts =
 let putInfo s = when (not $ Quiet `elem` opts) $ putStr s
 in do
 testline <- getPrefval "test"
 return $
   case testline of
   Nothing -> return ExitSuccess
   Just testcode -> do
     putInfo "Running test...\n"
     ec <- system testcode
     if ec == ExitSuccess
       then putInfo "Test ran successfully.\n"
       else putInfo "Test failed!\n"
     return ec

runPosthook :: [DarcsFlag] -> AbsolutePath -> IO ExitCode
runPosthook opts repodir =  do ph <- getPosthook opts
                               withCurrentDirectory repodir $ runHook opts "Posthook" ph

getPosthook :: [DarcsFlag] -> IO (Maybe String)
getPosthook opts = case getPosthookCmd opts of
                    Nothing -> return Nothing
                    Just command ->
                       if AskPosthook `elem` opts
                       then do putStr ("\nThe following command is set to execute.\n"++
                                                "Execute the following command now (yes or no)?\n"++
                                                command++"\n")
                               yorn <- askUser ""
                               case yorn of ('y':_) -> return $ Just command
                                            _ -> do putStrLn "Posthook cancelled..."
                                                    return Nothing
                       else return $ Just command

runPrehook :: [DarcsFlag] -> AbsolutePath -> IO ExitCode
runPrehook opts repodir =  do ph <- getPrehook opts
                              withCurrentDirectory repodir $ runHook opts "Prehook" ph

getPrehook :: [DarcsFlag] -> IO (Maybe String)
getPrehook opts = case getPrehookCmd opts of
                   Nothing -> return Nothing
                   Just command ->
                       if AskPrehook `elem` opts
                       then do putStr ("\nThe following command is set to execute.\n"++
                                                "Execute the following command now (yes or no)?\n"++
                                                command++"\n")
                               yorn <- askUser ""
                               case yorn of ('y':_) -> return $ Just command
                                            _ -> do putStrLn "Prehook cancelled..."
                                                    return Nothing
                       else return $ Just command

runHook :: [DarcsFlag] -> String -> Maybe String -> IO ExitCode
runHook _ _ Nothing = return ExitSuccess
runHook opts cname (Just command) =
    do ec <- system command
       when (Quiet `notElem` opts) $
         if ec == ExitSuccess
         then putStrLn $ cname++" ran successfully."
         else hPutStrLn stderr $ cname++" failed!"
       return ec

