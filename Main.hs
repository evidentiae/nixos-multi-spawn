{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Foldable (concatMap)
import Data.List (isPrefixOf)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as H
import GHC.Generics (Generic)
import System.Environment (getArgs)
import qualified System.Directory as D
import System.FilePath
import System.Process
import System.Process.Internals
import System.IO
import System.IO.TailFile
import System.Posix.Signals
import System.Posix.User
import System.Posix.Files
import System.Console.Concurrent
import System.Entropy
import System.Environment
import System.Systemd.Daemon
import qualified Codec.Binary.Base32 as B32
import Data.Digest.Pure.CRC32 (crc32)

systemdNspawnPath :: FilePath
systemdNspawnPath = "/run/current-system/sw/sbin/systemd-nspawn"

data Machine = Machine
  { nixosSystem :: FilePath
  , environment :: Maybe (H.HashMap String String)
  , inheritEnvVars :: Maybe [String]
  } deriving (Generic)

instance FromJSON Machine


data Config = Config
  { initBinary :: FilePath
  , machines :: H.HashMap String Machine
  , tailFiles :: [FilePath]
  } deriving (Generic)

instance FromJSON Config


printLog :: String -> IO ()
printLog = outputConcurrent . (++ "\n")


main :: IO ()
main = withConcurrentOutput $ do
  (uid,gid) <- checkUid
  [cfgFile,zone] <- getArgs
  jsonContents <- B.readFile cfgFile
  cfg <- case eitherDecode jsonContents of
           Right config -> pure config
           Left e -> error e

  runDir <- D.makeAbsolute "."
  runId <- getEntropy 8
  setupRunDir runDir

  tailers <- mapM (async . tailRelFile runDir) (tailFiles cfg)
  ps <- mapM (runMachine cfg uid runId runDir zone) (H.keys $ machines cfg)

  aTerm <- async waitForTermination
  aProc <- async ((mapM (async . waitForProcess) ps) >>= waitAny)

  _ <- notifyReady

  -- Wait for termination or at least one machine to stop
  waitEither_ aTerm aProc

  _ <- notifyStopping

  killer <- async (killRemainingProcesses ps)
  waitForAllProcesses killer ps

  -- Clean up
  _ <- cancel aTerm
  mapM_ cancel tailers
  fixupDir uid gid (runDir </> "logs")
  fixupDir uid gid (runDir </> "fs")


waitForTermination :: IO ()
waitForTermination = do
  v <- newEmptyMVar
  let handler = CatchOnce $ putMVar v ()
  _ <- installHandler sigINT handler Nothing
  _ <- installHandler sigTERM handler Nothing
  takeMVar v


killRemainingProcesses :: [ProcessHandle] -> IO ()
killRemainingProcesses ps = do
  -- First, try shutdown containers orderly (one SIGTERM to systemd-nspawn)
  mapM_ terminateProcess ps

  -- Give the containers some time to shut down
  threadDelay $ 5 * 1000 * 1000

  -- Order systemd-nspawn to halt containers (double SIGTERMs)
  mapM_ terminateProcess ps
  mapM_ terminateProcess ps

  -- Give the containers some time to shut down
  threadDelay $ 5 * 1000 * 1000

  -- Forcibly kill remaining systemd-nspawn instances
  -- TODO: This will leave grand child processes running, and they have
  -- to be killed manually. We should have some way to track grand children
  -- too so we can kill them here too
  -- A better way would probably be to KILL the systemd PIDs within the
  -- containers instead of killing systemd-nspawn
  forM_ ps $ \ph ->
    withProcessHandle ph $ \case
      ClosedHandle _  -> pure ()
      OpenHandle pid  -> signalProcess sigKILL pid
      OpenExtHandle{} -> error "Windows not supported"


waitForAllProcesses :: Async () -> [ProcessHandle] -> IO ()
waitForAllProcesses killer ps = do
  ecs <- mapM getProcessExitCode ps
  if all isJust ecs
    then cancel killer
    else waitForAllProcesses killer [ p | (p,Nothing) <- zip ps ecs ]


checkUid :: IO (UserID, GroupID)
checkUid = do
  ruid <- getRealUserID
  rgid <- getRealGroupID
  euid <- getEffectiveUserID
  case (ruid,euid) of
    (_,0) -> pure (ruid,rgid)
    _ -> error "Your effective uid must be root"


setupRunDir :: FilePath -> IO ()
setupRunDir p = do
  D.createDirectory $ p </> "fs"
  D.createDirectory $ p </> "logs"


fixupDir :: UserID -> GroupID -> FilePath -> IO ()
fixupDir uid gid f = do
  stat <- getSymbolicLinkStatus f
  if isSymbolicLink stat
    then do
      target <- readSymbolicLink f
      if isPrefixOf "/nix/store" target
        then rm f
        else fixup f
    else
      if isDirectory stat
        then do
          fixup f
          D.listDirectory f >>= mapM_ (fixupDir uid gid . (f </>))
        else
          if isRegularFile stat
            then fixup f
            else rm f
  where
    fixup p = setSymbolicLinkOwnerAndGroup p uid gid
    rm = D.removeFile


makeIdentifier :: BC.ByteString -> String
makeIdentifier s =
  map toLower $ take 8 $ filter (/= '=') $ B32.encode (octets $ crc32 s)
  where octets w32 = map (fromIntegral . shiftR w32) [24,16,8,0]


runMachine :: Config -> UserID -> BC.ByteString -> FilePath -> String -> String -> IO ProcessHandle
runMachine cfg uid runId runDir zone machine = do
  let root = runDir </> "fs" </> machine
  D.createDirectory root
  D.createDirectory $ root </> "usr"
  inH <- openFile "/dev/null" ReadMode
  outH <- openFile (runDir </> "logs" </> (machine++".out")) WriteMode
  errH <- openFile (runDir </> "logs" </> (machine++".err")) WriteMode
  let m = machines cfg H.! machine
  env' <- mapM (\k -> (k,) <$> getEnv k) (concat $ inheritEnvVars m)
  let mkEnv (k,v) = "--setenv="++k++"="++v
      args = concatMap (map mkEnv . (++) env' . H.toList) (environment m) ++
             [ "-D", root
             , "-M", makeIdentifier (runId `BC.append` BC.pack machine)
             , if uid == 0 then "-U" else "--private-users="++show uid
             , "--private-users-chown"
             , "--bind-ro=/nix/store"
             , "--tmpfs=/nix/var"
             , "--tmpfs=/var"
             , "--network-zone=" ++ zone
             , "--kill-signal=SIGRTMIN+3"
             , initBinary cfg
             , nixosSystem m ++ "/init"
             ]
      p = CreateProcess
            { cmdspec = RawCommand systemdNspawnPath args
            , cwd = Just root
            , env = Just []
            , std_in = UseHandle inH
            , std_out = UseHandle outH
            , std_err = UseHandle errH
            , close_fds = False
            , create_group = False
            , delegate_ctlc = False
            , detach_console = False
            , create_new_console = False
            , new_session = False
            , child_group = Nothing
            , child_user = Nothing
            }
  (_, _, _, ph) <- createProcess p
  pure ph


tailRelFile :: FilePath -> FilePath -> IO ()
tailRelFile runDir path = tailFile (runDir </> path) handler (pure "")
  where
    handler :: BC.ByteString -> BC.ByteString -> IO BC.ByteString
    handler prev buf = do
      let (f,l) = BC.breakEnd (== '\n') buf
          ls = BC.lines $ BC.concat [prev,f]
      mapM_ (printLog . BC.unpack) ls
      pure l
