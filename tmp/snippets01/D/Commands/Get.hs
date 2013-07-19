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

{-# LANGUAGE CPP #-}

module Darcs.Commands.Get ( get, clone ) where

import Prelude hiding ( catch )

import System.Directory ( setCurrentDirectory, doesDirectoryExist, doesFileExist,
                          createDirectory )
import Workaround ( getCurrentDirectory )
import Control.Exception ( catch, IOException )
import Control.Monad ( when )

import Darcs.Commands ( DarcsCommand(..), nodefaults, commandAlias, putInfo )
import Darcs.Flags( compression )
import Darcs.Arguments ( DarcsFlag( NewRepo, Lazy,
                                    UseFormat2,
                                    UseHashedInventory, UseNoWorkingDir,
                                    SetScriptsExecutable, OnePattern ),
                        getContext, useWorkingDir,
                        partial, reponame,
                        matchOneContext, setDefault, setScriptsExecutableOption,
                        networkOptions, makeScriptsExecutable, usePacks )
import Darcs.Repository ( Repository, withRepository, RepoJob(..), withRepoLock, identifyRepositoryFor, readRepo,
                          tentativelyRemovePatches, patchSetToRepository,
                          copyRepository, tentativelyAddToPending,
                          finalizeRepositoryChanges, setScriptsExecutable
                        , invalidateIndex, createRepository )
import Darcs.Repository.Format ( identifyRepoFormat, RepoFormat,
                                 RepoProperty ( Darcs2, HashedInventory ), formatHas )
import Darcs.Patch ( RepoPatch, apply, invert, effect, PrimOf )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Witnesses.Ordered ( lengthFL, mapFL_FL, (:>)(..) )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Patch.Depends ( findCommonWithThem, countUsThem )
import Darcs.Repository.Prefs ( setDefaultrepo )
import Darcs.Repository.Motd ( showMotd )
import Darcs.Match ( havePatchsetMatch, getOnePatchset )
import Progress ( debugMessage )
import Printer ( text, errorDoc, ($$) )
import Darcs.Lock ( writeBinFile )
import Darcs.RepoPath ( toFilePath, toPath, ioAbsoluteOrRemote)
import Darcs.Witnesses.Sealed ( Sealed(..) )
import Darcs.Global ( darcsdir )
import English ( englishNum, Noun(..) )
import Storage.Hashed.Tree( Tree )
#include "gadts.h"

getDescription :: String
getDescription = "Create a local copy of a repository."

getHelp :: String
getHelp =
 "Get creates a local copy of a repository.  The optional second\n" ++
 "argument specifies a destination directory for the new copy; if\n" ++
 "omitted, it is inferred from the source location.\n" ++
 "\n" ++
 "By default Darcs will copy every patch from the original repository.\n" ++
 "This means the copy is completely independent of the original; you can\n" ++
 "operate on the new repository even when the original is inaccessible.\n" ++
 "If you expect the original repository to remain accessible, you can\n" ++
 "use --lazy to avoid copying patches until they are needed (`copy on\n" ++
 "demand').  This is particularly useful when copying a remote\n" ++
 "repository with a long history that you don't care about.\n" ++
 "\n" ++
 "The --lazy option isn't as useful for local copies, because Darcs will\n" ++
 "automatically use `hard linking' where possible.  As well as saving\n" ++
 "time and space, you can move or delete the original repository without\n" ++
 "affecting a complete, hard-linked copy.  Hard linking requires that\n" ++
 "the copy be on the same filesystem and the original repository, and\n" ++
 "that the filesystem support hard linking.  This includes NTFS, HFS+\n" ++
 "and all general-purpose Unix filesystems (such as ext3, UFS and ZFS).\n" ++
 "FAT does not support hard links.\n" ++
 "\n" ++
 "Darcs get will not copy unrecorded changes to the source repository's\n" ++
 "working tree.\n" ++
 "\n" ++
 getHelpTag

get :: DarcsCommand
get = DarcsCommand {commandProgramName = "darcs",
                    commandName = "get",
                    commandHelp = getHelp,
                    commandDescription = getDescription,
                    commandExtraArgs = -1,
                    commandExtraArgHelp = ["<REPOSITORY>", "[<DIRECTORY>]"],
                    commandCommand = getCmd,
                    commandPrereq = contextExists,
                    commandGetArgPossibilities = return [],
                    commandArgdefaults = nodefaults,
                    commandAdvancedOptions = usePacks:networkOptions,
                    commandBasicOptions = [reponame,
                                            partial,
                                            matchOneContext,
                                            setDefault True,
                                            setScriptsExecutableOption,
                                            useWorkingDir]
                   }

clone :: DarcsCommand
clone = commandAlias "clone" Nothing get

getCmd :: [DarcsFlag] -> [String] -> IO ()
getCmd opts [inrepodir, outname] = getCmd (NewRepo outname:opts) [inrepodir]
getCmd opts [inrepodir] = do
  debugMessage "Starting work on get..."
  typed_repodir <- ioAbsoluteOrRemote inrepodir
  let repodir = toPath typed_repodir
  showMotd opts repodir
  rfsource <- identifyRepoFormat repodir
  debugMessage $ "Found the format of "++repodir++"..."
  mysimplename <- makeRepoName opts repodir
  createDirectory mysimplename
  setCurrentDirectory mysimplename
  let opts' = if formatHas Darcs2 rfsource
              then UseFormat2:opts
              else UseHashedInventory:filter (/= UseFormat2) opts
  createRepository opts'
  debugMessage "Finished initializing new directory."
  setDefaultrepo repodir opts

  writeBinFile (darcsdir++"/hashed_inventory") ""

  if not (null [p | OnePattern p <- opts]) -- --to-match given
     && Lazy `notElem` opts
    then withRepository opts $ RepoJob $ \repository -> do
      debugMessage "Using economical get --to-match handling"
      fromrepo <- identifyRepositoryFor  repository repodir
      Sealed patches_to_get <- getOnePatchset fromrepo opts
-- Warning:  A do-notation statement discarded a result of type Repository p ghc-prim
      _ <- patchSetToRepository fromrepo patches_to_get opts
      debugMessage "Finished converting selected patch set to new repository"
    else copyRepoAndGoToChosenVersion opts repodir rfsource
getCmd _ _ = fail "You must provide 'get' with either one or two arguments."

-- | called by getCmd
-- assumes that the target repo of the get is the current directory, and that an inventory in the
-- right format has already been created.
copyRepoAndGoToChosenVersion :: [DarcsFlag] -> String -> RepoFormat -> IO ()
copyRepoAndGoToChosenVersion opts repodir rfsource = do
  copyRepo
  withRepository opts ((RepoJob $ \repository -> goToChosenVersion repository opts) :: RepoJob ())
  putInfo opts $ text "Finished getting."
      where copyRepo =
                withRepository opts $ RepoJob $ \repository ->
                  if formatHas HashedInventory rfsource
                   then do
                                   debugMessage "Identifying and copying repository..."
                                   copyRepoHashed repository
                   else do
                                   putInfo opts $ text "***********************************************************************"
                                               $$ text "  _______   Sorry for the wait! The repository you are fetching is"
                                               $$ text " |       |  using the DEPRECATED 'old-fashioned' format. I'm getting a"
                                               $$ text " | O   O |  hashed copy instead, but this may take a while."
                                               $$ text " |  ___  |"
                                               $$ text " | /   \\ |  We recommend that the maintainer upgrade the remote copy"
                                               $$ text " |_______|  as well. See http://wiki.darcs.net/OF for more information."
                                               $$ text ""
                                               $$ text "***********************************************************************"
                                   copyRepoHashed repository
            copyRepoHashed repository =
              do identifyRepositoryFor repository repodir >>= flip copyRepository (UseNoWorkingDir `notElem` opts)
                 when (SetScriptsExecutable `elem` opts) setScriptsExecutable

makeRepoName :: [DarcsFlag] -> FilePath -> IO String
makeRepoName (NewRepo n:_) _ =
    do exists <- doesDirectoryExist n
       file_exists <- doesFileExist n
       if exists || file_exists
          then fail $ "Directory or file named '" ++ n ++ "' already exists."
          else return n
makeRepoName (_:as) d = makeRepoName as d
makeRepoName [] d =
  case dropWhile (=='.') $ reverse $
       takeWhile (\c -> c /= '/' && c /= ':') $
       dropWhile (=='/') $ reverse d of
  "" -> modifyRepoName "anonymous_repo"
  base -> modifyRepoName base

modifyRepoName :: String -> IO String
modifyRepoName name =
    if head name == '/'
    then mrn name (-1)
    else do cwd <- getCurrentDirectory
            mrn (cwd ++ "/" ++ name) (-1)
 where
  mrn :: String -> Int -> IO String
  mrn n i = do
    exists <- doesDirectoryExist thename
    file_exists <- doesFileExist thename
    if not exists && not file_exists
       then do when (i /= -1) $
                    putStrLn $ "Directory '"++ n ++
                               "' already exists, creating repository as '"++
                               thename ++"'"
               return thename
       else mrn n $ i+1
    where thename = if i == -1 then n else n++"_"++show i

getHelpTag :: String
getHelpTag =
 "It is often desirable to make a copy of a repository that excludes\n" ++
 "some patches.  For example, if releases are tagged then `darcs get\n" ++
 "--tag .' would make a copy of the repository as at the latest release.\n" ++
 "\n" ++
 "An untagged repository state can still be identified unambiguously by\n" ++
 "a context file, as generated by `darcs changes --context'.  Given the\n" ++
 "name of such a file, the --context option will create a repository\n" ++
 "that includes only the patches from that context.  When a user reports\n" ++
 "a bug in an unreleased version of your project, the recommended way to\n" ++
 "find out exactly what version they were running is to have them\n" ++
 "include a context file in the bug report.\n" ++
 "\n" ++
 "You can also make a copy of an untagged state using the --to-patch or\n" ++
 "--to-match options, which exclude patches `after' the first matching\n" ++
 "patch.  Because these options treat the set of patches as an ordered\n" ++
 "sequence, you may get different results after reordering with `darcs\n" ++
 "optimize', so tagging is preferred.\n"

contextExists :: [DarcsFlag] -> IO (Either String ())
contextExists opts =
   case getContext opts of
     Nothing -> return $ Right ()
     Just f  -> do exists <- doesFileExist $ toFilePath f
                   return $ if exists
                            then Right ()
                            else Left $ "Context file "++toFilePath f++" does not exist"

goToChosenVersion :: (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
                  => Repository p C(r u r)
                  -> [DarcsFlag] -> IO ()
goToChosenVersion repository opts =
    when (havePatchsetMatch opts) $ do
       debugMessage "Going to specified version..."
       patches <- readRepo repository
       Sealed context <- getOnePatchset repository opts
       when (snd (countUsThem patches context) > 0) $
            errorDoc $ text "Missing patches from context!" -- FIXME : - (
       _ :> us' <- return $ findCommonWithThem patches context
       let ps = mapFL_FL hopefully us'
       putInfo opts $ text $ "Unapplying " ++ show (lengthFL ps) ++ " " ++
                   englishNum (lengthFL ps) (Noun "patch") ""
       invalidateIndex repository
       withRepoLock opts $ RepoJob $ \_ ->
-- Warning:  A do-notation statement discarded a result of type Repository p r u z.
           do _ <- tentativelyRemovePatches repository (compression opts) us'
              tentativelyAddToPending repository opts $ invert $ effect us'
              finalizeRepositoryChanges repository
              apply (invert $ effect ps) `catch` \(e :: IOException) ->
                  fail ("Couldn't undo patch in working dir.\n" ++ show e)
              makeScriptsExecutable opts (invert $ effect ps)

