import Control.Concurrent (threadDelay)
import Control.Monad (when, forM_)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes, fromMaybe)
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment (getEnv, setEnv)
import System.IO
import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W

main :: IO ()
main = do
  home <- getHomeDirectory
  logFile <- getLogFile
  _ <- writeFile logFile ""
  -- Kill processes started in prior session
  _ <- spawnPipe "pkill -9 -f 'xmobar|xscreensaver'"
  _ <- threadDelay 10000

  -- TODO: make this conditional on detection of nvidia-settings on PATH:
  _ <- spawnPipe "nvidia-settings --load-config-only"

  -- TODO: probably want to check the specific ssh-agent
  _ <- spawnPipe $ "if ! pgrep -u $USER ssh-agent > /dev/null; then ssh-agent > ~/.ssh/agent-env; fi"
  _ <- threadDelay 10000
  loadSshAgentEnv $ home <> "/.ssh/agent-env"
  sshAgentPid <- getEnv "SSH_AGENT_PID"
  appendLog $ "getEnv  SSH_AGENT_PID: " ++ sshAgentPid
  let xmobarBin = home <> "/.local/bin/xmobar"
  let xmobarRc = home <> "/.xmonad/xmobarrc"
  _ <- spawnPipe $ "xscreensaver"
  xmproc <- spawnPipe $ xmobarBin <> " " <> xmobarRc
  xmonad myConfig' {
    logHook = dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "green" "" . shorten 50
    }
  }

myModMask = mod4Mask -- Use Super instead of Alt

myConfig = docks $ def {
    manageHook = manageDocks <+> manageHook def
  , layoutHook = avoidStruts  $  layoutHook def
  , handleEventHook    = handleEventHook def
  , modMask = mod4Mask
  , terminal = "alacritty" -- "xfce4-terminal" -- TODO: add a static priority list
  , workspaces = myWorkspaces
  }
myConfig' = myConfig `additionalKeys` myKeys

myExtraWorkspaces = [
                    (xK_0, "10💼")
                  , (xK_1, "11🌐🌐")
                  , (xK_2, "12🌐📝")
                  , (xK_3, "13📝")
                  , (xK_4, "14📖")
                  , (xK_5, "15📖🖋️")
                  , (xK_6, "16☯")
                  , (xK_7, "17🎮")
                  , (xK_8, "18🖥️")
                  , (xK_9, "19[tmp]")
                  ]
myWorkspaces = [
               "1☺♫","2💬","3✉","4🌐🦀","5✏️🦀","6🌐🦀","7✏️🦀","8🌐𝕊","9✏️𝕊"
            ] ++ (map snd myExtraWorkspaces)

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [
    ((modMask myConfig, xK_b), bringMenu)
  , ((modMask myConfig, xK_g), gotoMenu)
  , ((modMask myConfig, xK_p), spawn  "$(yeganesh -x)")
  , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
  ] ++ [
    ((myModMask .|. controlMask, key), (windows $ W.greedyView ws))
      | (key,ws) <- myExtraWorkspaces
  ] ++ [
    ((myModMask .|. controlMask .|. shiftMask, key), (windows $ W.shift ws))
      | (key,ws) <- myExtraWorkspaces
  ]

-- Parses a line into a variable name and value, trimming a trailing semicolon if present
parseEnvVarLine :: String -> Maybe (String, String)
parseEnvVarLine line = case span (/= '=') line of
    (var, '=':value) -> Just (var, takeWhile (/= ';') value)
    _ -> Nothing

getLogFile :: IO String
getLogFile = do
  home <- getHomeDirectory 
  pure $ home <> "/xmonad_debug.log" 

-- Helper function to append a debug message to a log file
appendLog :: String -> IO ()
appendLog msg = do
  logFile <- getLogFile
  appendFile logFile (msg ++ "\n")

loadSshAgentEnv :: FilePath -> IO ()
loadSshAgentEnv filePath = do
    fileExists <- doesFileExist filePath
    appendLog $ "File " ++ filePath ++ " exits?: " ++ show fileExists
    when fileExists $ do
        withFile filePath ReadMode (\handle -> do
            lazyContent <- hGetContents handle
            let envVars = catMaybes . map parseEnvVarLine . lines $ lazyContent
            appendLog $ "Num vars: " ++ show (length envVars)
            forM_ envVars $ \(var, value) -> do
                appendLog $ "Setting " ++ var ++ " to " ++ value
                setEnv var value
            )    
            
-- loadSshAgentEnv :: FilePath -> IO ()
-- loadSshAgentEnv filePath = do
--     fileExists <- doesFileExist filePath
--     when fileExists $ do
--         content <- withFile filePath ReadMode hGetContents
--         let envVars = catMaybes . map parseEnvVarLine . lines $ content
--         forM_ envVars $ \(var, value) ->
--             setEnv var value
