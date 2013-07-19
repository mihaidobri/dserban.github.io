{-# OPTIONS_GHC -fno-warn-dodgy-imports #-} -- needed for GHC 7.0/7.2
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Exec
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable
--
-- Various utility functions that do not belong anywhere else.

module Darcs.Utils
    (
      ortryrunning
    , nubsort
    , breakCommand
    , showHexLen
    , maybeGetEnv
    , formatPath
    -- * Monads
    , firstJustIO
    -- * User prompts
    , askEnter
    , askUser
    , askUserListItem
    , PromptConfig(..)
    , promptYorn
    , promptChar
    -- * Text
    , getViewer
    , editFile
    , runEditor
    , stripCr
    -- * Help
    , environmentHelpEditor
    , environmentHelpPager
    -- * Errors and exceptions
    , catchall
    , clarifyErrors
    , prettyException
    , prettyError
    , addToErrorLoc
    -- * Files and directories
    , getFileStatus
    , withCurrentDirectory
    , withUMask
    --  * Locales
    , setDarcsEncodings
    , getSystemEncoding
    , isUTF8Locale
    -- * Tree filtering.
    , filterFilePaths
    , filterPaths
    -- * Tree lookup.
    , treeHas
    , treeHasDir
    , treeHasFile
    , treeHasAnycase
    ) where


import Prelude hiding ( catch )

import Control.Exception.Extensible
             ( bracket, bracket_, catch, try,
               IOException, SomeException, Exception(fromException) )
import Control.Monad ( when, forM )
import Control.Monad.Error( MonadError )
import Control.Monad.State.Strict( gets )

import qualified Data.ByteString as B ( readFile )
import qualified Data.ByteString.Char8 as BSC

import Data.Char ( toUpper, toLower, isSpace )
import Data.List ( group, sort )
import Data.Maybe ( isJust )

import Foreign.C.String ( CString, withCString, peekCString )
import Foreign.C.Error ( throwErrno )
import Foreign.C.Types ( CInt(..) )

#ifdef FORCE_CHAR8_ENCODING
import GHC.IO.Encoding ( setFileSystemEncoding, setForeignEncoding, char8 )
#endif

import Storage.Hashed.AnchoredPath( AnchoredPath(..), Name(..), isPrefix, floatPath )
import Storage.Hashed.Monad( withDirectory, fileExists, directoryExists
                           , virtualTreeMonad, currentDirectory
                           , TreeMonad )
import qualified Storage.Hashed.Monad as HS ( exists, tree )

import Storage.Hashed.Tree( Tree, listImmediate, findTree )

import System.Console.Haskeline ( runInputT, defaultSettings, getInputLine,
                                  getInputChar, outputStr, outputStrLn )
import System.Directory ( doesFileExist )
import System.Environment ( getEnv )
import System.Exit ( ExitCode(..) )
import System.IO.Error ( annotateIOError, isUserError, ioeGetErrorString
                       , isDoesNotExistError, ioeGetFileName )
import System.Posix.Files( getSymbolicLinkStatus, FileStatus )

import Text.Regex

import Darcs.RepoPath ( FilePathLike, getCurrentDirectory, setCurrentDirectory, toFilePath )
import Darcs.SignalHandler ( catchNonSignal )

import Exec ( execInteractive )
import Numeric ( showHex )
import Progress ( withoutProgress )


showHexLen :: (Integral a, Show a)
           => Int
           -> a
           -> String
showHexLen n x = let s = showHex x ""
                 in replicate (n - length s) ' ' ++ s


addToErrorLoc :: IOException
              -> String
              -> IOException
addToErrorLoc ioe s = annotateIOError ioe s Nothing Nothing


catchall :: IO a
         -> IO a
         -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)


maybeGetEnv :: String
            -> IO (Maybe String)
maybeGetEnv s = fmap Just (getEnv s) `catchall` return Nothing -- err can only be isDoesNotExist


-- | The firstJustM returns the first Just entry in a list of monadic
-- operations. This is close to `listToMaybe `fmap` sequence`, but the sequence
-- operator evaluates all monadic members of the list before passing it along
-- (i.e. sequence is strict). The firstJustM is lazy in that list member monads
-- are only evaluated up to the point where the first Just entry is obtained.
firstJustM :: Monad m
           => [m (Maybe a)]
           -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (e:es) = e >>= (\v -> if isJust v then return v else firstJustM es)


-- | The firstJustIO is a slight modification to firstJustM: the entries in the
-- list must be IO monad operations and the firstJustIO will silently turn any
-- monad call that throws an exception into Nothing, basically causing it to be
-- ignored.
firstJustIO :: [IO (Maybe a)]
            -> IO (Maybe a)
firstJustIO = firstJustM . map (`catchall` return Nothing)


clarifyErrors :: IO a
              -> String
              -> IO a
clarifyErrors a e = a `catch` (\x -> fail $ unlines [prettyException x,e])


prettyException :: SomeException
                -> String
prettyException e | Just ioe <- fromException e, isUserError ioe = ioeGetErrorString ioe
prettyException e | Just ioe <- fromException e, isDoesNotExistError ioe =
  case ioeGetFileName ioe of
    Just f  -> f ++ " does not exist"
    Nothing -> show e
prettyException e = show e


prettyError :: IOError -> String
prettyError e | isUserError e = ioeGetErrorString e
              | otherwise = show e


-- | Given two shell commands as arguments, execute the former.  The
-- latter is then executed if the former failed because the executable
-- wasn't found (code 127), wasn't executable (code 126) or some other
-- exception occurred.  Other failures (such as the user holding ^C)
-- do not cause the second command to be tried.
ortryrunning :: IO ExitCode
             -> IO ExitCode
             -> IO ExitCode
a `ortryrunning` b = do
  ret <- try a
  case ret of
    (Right (ExitFailure 126)) -> b -- command not executable
    (Right (ExitFailure 127)) -> b -- command not found
#ifdef WIN32
    (Right (ExitFailure 9009)) -> b -- command not found by cmd.exe on Windows
#endif
    (Right x) -> return x          -- legitimate success/failure
    (Left (_ :: SomeException)) -> b  -- an exception


withCurrentDirectory :: FilePathLike p
                     => p
                     -> IO a
                     -> IO a
withCurrentDirectory name m =
    bracket
        (do cwd <- getCurrentDirectory
            when (toFilePath name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd `catchall` return ())
        (const m)


foreign import ccall unsafe "umask.h set_umask" set_umask
    :: CString -> IO CInt
foreign import ccall unsafe "umask.h reset_umask" reset_umask
    :: CInt -> IO CInt


withUMask :: String
          -> IO a
          -> IO a
withUMask umask job =
    do rc <- withCString umask set_umask
       when (rc < 0) (throwErrno "Couldn't set umask")
       bracket_
           (return ())
           (reset_umask rc)
           job


-- | Ask the user for a line of input.
askUser :: String    -- ^ The prompt to display
        -> IO String -- ^ The string the user entered.
askUser prompt = withoutProgress $ runInputT defaultSettings $
                    getInputLine prompt
                        >>= maybe (error "askUser: unexpected end of input") return

-- | Ask the user to press Enter
askEnter :: String  -- ^ The prompt to display
         -> IO ()
askEnter prompt = askUser prompt >> return ()

-- | @askUserListItem prompt xs@ enumerates @xs@ on the screen, allowing
--   the user to choose one of the items
askUserListItem :: String
                -> [String]
                -> IO String
askUserListItem prompt xs = withoutProgress $ runInputT defaultSettings $ do
    outputStr . unlines $ zipWith (\n x -> show n ++ ". " ++ x) [1::Int ..] xs
    loop
  where
    loop = do
      answer <- getInputLine prompt
                  >>= maybe (error "askUser: unexpected end of input") return
      case maybeRead answer of
        Just n | n > 0 && n <= length xs -> return (xs !! (n-1))
        _ -> outputStrLn "Invalid response, try again!" >> loop


maybeRead :: Read a
          => String
          -> Maybe a
maybeRead s = case reads s of
    [(x, rest)] | all isSpace rest -> Just x
    _         -> Nothing


stripCr :: String
        -> String
stripCr ""     = ""
stripCr "\r"   = ""
stripCr (c:cs) = c : stripCr cs


-- | Format a path for screen output, so that the user sees where the path
-- begins and ends. Could (should?) also warn about unprintable characters here.
formatPath :: String
           -> String
formatPath path = "\"" ++ quote path ++ "\""
    where quote "" = ""
          quote (c:cs) = if c `elem` ['\\', '"']
                         then '\\':c:quote cs
                         else c:quote cs


breakCommand :: String -> (String, [String])
breakCommand s = case words s of
                   (arg0:args) -> (arg0,args)
                   [] -> (s,[])


nubsort :: Ord a
        => [a]
        -> [a]
nubsort = map head . group . sort


-- | @editFile f@ lets the user edit a file which could but does not need to
-- already exist. This function returns the exit code from the text editor and a
-- flag indicating if the user made any changes.
editFile :: FilePathLike p
         => p
         -> IO (ExitCode, Bool)
editFile ff = do
    old_content <- file_content
    ec <- runEditor f
    new_content <- file_content
    return (ec, new_content /= old_content)
  where
    f = toFilePath ff
    file_content = do
      exists <- doesFileExist f
      if exists then do content <- B.readFile f
                        return $ Just content
                else return Nothing


runEditor :: FilePath
          -> IO ExitCode
runEditor f = do
    ed <- getEditor
    execInteractive ed f
         `ortryrunning` execInteractive "emacs" f
         `ortryrunning` execInteractive "emacs -nw" f
         `ortryrunning` execInteractive "nano" f
#ifdef WIN32
         `ortryrunning` execInteractive "edit" f
#endif


getEditor :: IO String
getEditor = getEnv "DARCS_EDITOR" `catchall`
             getEnv "DARCSEDITOR" `catchall`
             getEnv "VISUAL" `catchall`
             getEnv "EDITOR" `catchall` return "vi"


environmentHelpEditor :: ([String], [String])
environmentHelpEditor = (["DARCS_EDITOR", "DARCSEDITOR", "VISUAL", "EDITOR"],[
 "To edit a patch description of email comment, Darcs will invoke an",
 "external editor.  Your preferred editor can be set as any of the",
 "environment variables $DARCS_EDITOR, $DARCSEDITOR, $VISUAL or $EDITOR.",
 "If none of these are set, vi(1) is used.  If vi crashes or is not",
 "found in your PATH, emacs, emacs -nw, nano and (on Windows) edit are",
 "each tried in turn."])


getViewer :: IO String
getViewer = getEnv "DARCS_PAGER" `catchall`
             getEnv "PAGER" `catchall` return "less"

environmentHelpPager :: ([String], [String])
environmentHelpPager = (["DARCS_PAGER", "PAGER"],[
 "Darcs will sometimes invoke a pager if it deems output to be too long",
 "to fit onscreen.  Darcs will use the pager specified by $DARCS_PAGER",
 "or $PAGER.  If neither are set, `less' will be used."])

data PromptConfig = PromptConfig { pPrompt :: String
                                 , pBasicCharacters :: [Char]
                                 , pAdvancedCharacters :: [Char] -- ^ only shown on help
                                 , pDefault :: Maybe Char
                                 , pHelp    :: [Char]
                                 }


-- | Prompt the user for a yes or no
promptYorn :: [Char] -> IO Bool
promptYorn p = (== 'y') `fmap` promptChar (PromptConfig p "yn" [] Nothing [])


promptChar :: PromptConfig -> IO Char
promptChar (PromptConfig p basic_chs adv_chs md help_chs) =
  withoutProgress $ runInputT defaultSettings loopChar
 where
 chs = basic_chs ++ adv_chs
 loopChar = do
    let chars = setDefault (basic_chs ++ (if null adv_chs then "" else "..."))
        prompt = p ++ " [" ++ chars ++ "]" ++ helpStr
    a <- getInputChar prompt >>= maybe (error "promptChar: unexpected end of input")
                                    return
    case () of
     _ | a `elem` chs                   -> return a
       | a == ' '                       -> maybe tryAgain return md
       | a `elem` help_chs              -> return a
       | otherwise                      -> tryAgain
 helpStr = case help_chs of
           []                      -> ""
           (h:_) | null adv_chs    -> ", or " ++ (h:" for help: ")
                 | otherwise       -> ", or " ++ (h:" for more options: ")
 tryAgain = do outputStrLn "Invalid response, try again!"
               loopChar
 setDefault s = case md of Nothing -> s
                           Just d  -> map (setUpper d) s
 setUpper d c = if d == c then toUpper c else c


-- | Construct a filter from a list of AnchoredPaths, that will accept any path
-- that is either a parent or a child of any of the listed paths, and discard
-- everything else.
filterPaths :: [AnchoredPath]
            -> AnchoredPath
            -> t
            -> Bool
filterPaths files p _ = any (\x -> x `isPrefix` p || p `isPrefix` x) files


-- | Same as 'filterPath', but for ordinary 'FilePath's (as opposed to
-- AnchoredPath).
filterFilePaths :: [FilePath]
                -> AnchoredPath
                -> t
                -> Bool
filterFilePaths = filterPaths . map floatPath


getFileStatus :: FilePath
              -> IO (Maybe FileStatus)
getFileStatus f =
  Just `fmap` getSymbolicLinkStatus f `catchall` return Nothing


treeHasAnycase :: (MonadError e m, Functor m, Monad m)
               => Tree m
               -> FilePath
               -> m Bool
treeHasAnycase tree path =
    fst `fmap` virtualTreeMonad (existsAnycase $ floatPath path) tree


existsAnycase :: (MonadError e m, Functor m, Monad m)
              => AnchoredPath
              -> TreeMonad m Bool
existsAnycase (AnchoredPath []) = return True
existsAnycase (AnchoredPath (Name x:xs)) = do
  do wd <- currentDirectory
     Just tree <- gets (flip findTree wd . HS.tree)
     let subs = [ AnchoredPath [Name n] | (Name n, _) <- listImmediate tree,
                                          BSC.map toLower n == BSC.map toLower x ]
     or `fmap` forM subs (\path -> do
       file <- fileExists path
       if file then return True
               else withDirectory path (existsAnycase $ AnchoredPath xs))


treeHas :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHas tree path = fst `fmap` virtualTreeMonad (HS.exists $ floatPath path) tree

treeHasDir :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasDir tree path = fst `fmap` virtualTreeMonad (directoryExists $ floatPath path) tree

treeHasFile :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasFile tree path = fst `fmap` virtualTreeMonad (fileExists $ floatPath path) tree

-- | In some environments, darcs requires that certain global GHC library variables that
-- control the encoding used in internal translations are set to specific values.
--
-- @setDarcsEncoding@ enforces those settings, and should be called before the
-- first time any darcs operation is run, and again if anything else might have
-- set those encodings to different values.
--
-- Note that it isn't thread-safe and has a global effect on your program.
--
-- The current behaviour of this function is as follows, though this may
-- change in future:
--
-- Encodings are only set on GHC 7.4 and up, on any non-Windows platform.
--
-- Two encodings are set, both to @GHC.IO.Encoding.char8@:
-- @GHC.IO.Encoding.setFileSystemEncoding@ and @GHC.IO.Encoding.setForeignEncoding@.
--
setDarcsEncodings :: IO ()
setDarcsEncodings = do
#ifdef FORCE_CHAR8_ENCODING

-- This is needed for appropriate behaviour from getArgs and from general
-- filesystem calls (e.g. getDirectoryContents, readFile, ...)
    setFileSystemEncoding char8

-- This ensures that foreign calls made by hashed-storage to stat
-- filenames returned from getDirectoryContents are translated appropriately
    setForeignEncoding char8

#endif
    return ()

-- The following functions are copied from the encoding package (BSD3
-- licence, by Henning Günther).

-- | @getSystemEncoding@ fetches the current encoding from locale
foreign import ccall "system_encoding.h get_system_encoding"
     get_system_encoding :: IO CString


getSystemEncoding :: IO String
getSystemEncoding = do
    enc <- get_system_encoding
    peekCString enc


-- | @isUTF8@ checks if an encoding is UTF-8 (or ascii, since it is a
-- subset of UTF-8).
isUTF8Locale :: String -> Bool
isUTF8Locale codeName = case (normalizeEncoding codeName) of
    -- ASCII
    "ascii"              -> True
    "646"                -> True
    "ansi_x3_4_1968"     -> True
    "ansi_x3.4_1986"     -> True
    "cp367"              -> True
    "csascii"            -> True
    "ibm367"             -> True
    "iso646_us"          -> True
    "iso_646.irv_1991"   -> True
    "iso_ir_6"           -> True
    "us"                 -> True
    "us_ascii"           -> True
    -- UTF-8
    "utf_8"              -> True
    "u8"                 -> True
    "utf"                -> True
    "utf8"               -> True
    "utf8_ucs2"          -> True
    "utf8_ucs4"          -> True
    -- Everything else
    _                    -> False
  where
    normalizeEncoding s = map toLower $ subRegex sep s "_"
    sep = mkRegex "[^0-9A-Za-z]+"
