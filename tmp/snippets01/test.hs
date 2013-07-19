{-# LANGUAGE CPP, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main ( main ) where

import qualified Darcs.Test.Misc
import qualified Darcs.Test.Patch
import qualified Darcs.Test.Email

import Control.Monad (when, filterM)
import Data.List ( isPrefixOf, isSuffixOf, sort )
import qualified Data.ByteString.Char8 as B
import System.Console.CmdLib
import System.Directory ( doesFileExist )
import System.Environment.FindBin ( getProgPath )
import System.FilePath( takeDirectory, takeBaseName, isAbsolute )
import System.IO( hSetBinaryMode, hSetBuffering, BufferMode( NoBuffering ), stdin, stdout, stderr )
import Test.Framework.Providers.API
import Test.Framework
import Shellish hiding ( liftIO, run )
import qualified Shellish

doUnit :: IO [Test]
doUnit = return unitTests

-- | This is the big list of tests that will be run using testrunner.
unitTests :: [Test]
unitTests =
  [ Darcs.Test.Email.testSuite
  , Darcs.Test.Misc.testSuite
  ] ++ Darcs.Test.Patch.testSuite

-- ----------------------------------------------------------------------
-- shell tests
-- ----------------------------------------------------------------------

data Format = Hashed | Darcs2 deriving Show
data Running = Running deriving Show
data Result = Success | Skipped | Failed String

instance Show Result where
  show Success = "Success"
  show Skipped = "Skipped"
  show (Failed f) = unlines (map ("| " ++) $ lines f)

instance TestResultlike Running Result where
  testSucceeded Success = True
  testSucceeded Skipped = True
  testSucceeded _ = False

data ShellTest = ShellTest { format :: Format
                           , testfile :: FilePath
                           , testdir  :: Maybe FilePath -- ^ only if you want to set it explicitly
                           , _darcspath :: FilePath
                           }
                 deriving Typeable

runtest' :: ShellTest -> FilePath -> ShIO Result
runtest' (ShellTest fmt _ _ dp) srcdir =
  do wd <- pwd
     setenv "HOME" wd
     setenv "TESTDATA" (srcdir </> "tests" </> "data")
     setenv "TESTBIN" (srcdir </> "tests" </> "bin")
     setenv "DARCS_TESTING_PREFS_DIR" $ wd </> ".darcs"
     setenv "EMAIL" "tester"
     setenv "DARCS_DONT_COLOR" "1"
     setenv "DARCS_DONT_ESCAPE_ANYTHING" "1"
     getenv "PATH" >>= setenv "PATH" . ((takeDirectory dp ++ pathVarSeparator) ++)
     setenv "DARCS" dp
     mkdir ".darcs"
     writefile ".darcs/defaults" defaults
-- Warning:  A do-notation statement discarded a result of type String.
     _ <- Shellish.run "bash" [ "test" ]
     return Success
   `catch_sh` \e -> case e of
      RunFailed _ 200 _ -> return Skipped
      RunFailed _ _   _ -> Failed <$> B.unpack <$> lastOutput
  where defaults = unlines ["ALL " ++ fmtstr, "send no-edit-description", "ALL ignore-times"]
        fmtstr = case fmt of
                  Darcs2 -> "darcs-2"
                  Hashed -> "hashed"
#ifdef WIN32
        pathVarSeparator = ";"
#else
        pathVarSeparator = ":"
#endif

runtest :: ShellTest -> ShIO Result
runtest t =
 withTmp $ \dir -> do
  cp "tests/lib" dir
  cp ("tests" </> testfile t) (dir </> "test")
  srcdir <- pwd
  silently $ sub $ cd dir >> runtest' t srcdir
 where
  withTmp =
   case testdir t of
     Just dir -> \job -> do
       let d = (dir </> show (format t) </> takeBaseName (testfile t))
       mkdir_p d
       job d
     Nothing  -> withTmpDir

instance Testlike Running Result ShellTest where
  testTypeName _ = "Shell"
  runTest _ test = runImprovingIO $ do yieldImprovement Running
                                       liftIO (shellish $ runtest test)

shellTest :: FilePath -> Format -> Maybe FilePath -> String -> Test
shellTest dp fmt tdir file = Test (file ++ " (" ++ show fmt ++ ")") $ ShellTest fmt file tdir dp

findShell :: FilePath -> Maybe FilePath -> Bool -> ShIO [Test]
findShell dp tdir isFailing =
  do files <- sort <$> grep relevant <$> grep (".sh" `isSuffixOf`) <$> ls "tests"
     return [ shellTest dp fmt tdir file
            | fmt <- [ Darcs2, Hashed ]
            , file <- files ]
  where relevant = (if isFailing then id else not) . ("failing-" `isPrefixOf`)

findNetwork :: FilePath -> Maybe FilePath -> ShIO [Test]
findNetwork dp tdir =
  do files <- sort <$> grep (".sh" `isSuffixOf`) <$> ls "tests/network"
     return [ shellTest dp Darcs2 tdir ("network" </> file) | file <- files ]

-- ----------------------------------------------------------------------
-- harness
-- ----------------------------------------------------------------------

data Config = Config { failing :: Bool
                     , shell :: Bool
                     , network :: Bool
                     , unit :: Bool
                     , darcs :: String
                     , tests :: [String]
                     , testDir :: Maybe FilePath
                     , plain :: Bool
                     , hideSuccesses :: Bool
                     , threads :: Int }
            deriving (Data, Typeable, Eq)

instance Attributes Config where
  attributes _ = group "Options"
    [ failing %> Help "Run the failing (shell) tests."
    , shell %> Help "Run the passing, non-network shell tests." %+ Default True
    , network %> Help "Run the network shell tests."
    , unit %> Help "Run the unit tests." %+ Default True
    , tests %> Help "Pattern to limit the tests to run." %+ short 't'
    , testDir %> Help "Directory to run tests in" %+ Default (Nothing :: Maybe FilePath)
    , plain %> Help "Use plain-text output."
    , hideSuccesses %> Help "Hide successes."
    , threads %> Default (1 :: Int) %+ short 'j' ]

data DarcsTest = DarcsTest deriving Typeable
instance Command DarcsTest (Record Config) where
  run _ conf _ = do
    let args = [ "-j", show $ threads conf ] 
             ++ concat [ ["-t", x ] | x <- tests conf ]
             ++ [ "--plain" | True <- [plain conf] ]
             ++ [ "--hide-successes" | True <- [hideSuccesses conf] ]
             ++ [ "--maximum-unsuitable-generated-tests", "700" ]
    case testDir conf of
       Nothing -> return ()
       Just d  -> do e <- shellish (test_e d)
                     when e $ fail ("Directory " ++ d ++ " already exists. Cowardly exiting")
    darcsBin <-
        case darcs conf of
            "" -> do
                path <- getProgPath
                let candidates =
                      -- if darcs-test lives in foo/something, look for foo/darcs[.exe]
                      -- for example if we've done cabal install -ftest, there'll be a darcs-test and darcs in the cabal
                      -- installation folder
                      [path </> "darcs" ++ exeSuffix] ++
                      -- if darcs-test lives in foo/darcs-test/something, look for foo/darcs/darcs[.exe]
                      -- for example after cabal build we can run dist/build/darcs-test/darcs-test and it'll find
                      -- the darcs in dist/build/darcs/darcs
                      [takeDirectory path </> "darcs" </> "darcs" ++ exeSuffix | takeBaseName path == "darcs-test" ]
                availableCandidates <- filterM doesFileExist candidates
                case availableCandidates of
                     (darcsBin:_) -> do
                         putStrLn $ "Using darcs executable in " ++ darcsBin
                         return darcsBin
                     [] -> fail ("No darcs specified or found nearby. Perhaps --darcs `pwd`/dist/build/darcs/darcs" ++ exeSuffix ++ "?")
            v -> return v
    when (shell conf || network conf || failing conf) $ do
      when (not (isAbsolute $ darcsBin)) $
        fail ("Argument to --darcs should be an absolute path")
      when (not (exeSuffix `isSuffixOf` darcsBin)) $
        putStrLn $ "Warning: --darcs flag does not end with " ++ exeSuffix ++ " - some tests may fail (case does matter)"
    ftests <- shellish $ if failing conf then findShell darcsBin (testDir conf) True else return []
    stests <- shellish $ if shell conf then findShell darcsBin (testDir conf) False else return []
    utests <- if unit conf then doUnit else return []
    ntests <- shellish $ if network conf then findNetwork (darcs conf) (testDir conf) else return []
    defaultMainWithArgs (ftests ++ stests ++ utests ++ ntests) args
       where
          exeSuffix :: String
#ifdef WIN32
          exeSuffix = ".exe"
#else
          exeSuffix = ""
#endif

main :: IO ()
main = do hSetBinaryMode stdout True
          hSetBuffering stdout NoBuffering
          hSetBinaryMode stderr True
          hSetBinaryMode stdin True
          getArgs >>= execute DarcsTest
