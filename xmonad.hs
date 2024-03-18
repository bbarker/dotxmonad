import Control.Concurrent (threadDelay)
import System.Directory (getHomeDirectory)
import System.IO
import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = do
  -- Kill processes started in prior session
  _ <- spawnPipe "pkill -9 xmobar xscreensaver"
  _ <- threadDelay 10000

  -- TODO: make this conditional on detection of nvidia-settings on PATH:
  _ <- spawnPipe "nvidia-settings --load-config-only"

  home <- getHomeDirectory
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

myConfig = docks $ def {
    manageHook = manageDocks <+> manageHook def
  , layoutHook = avoidStruts  $  layoutHook def
   -- this must be in this order, docksEventHook must be last:
  , handleEventHook    = handleEventHook def -- <+> docksEventHook
  , modMask = mod4Mask -- Use Super instead of Alt
  , terminal = "alacritty" -- "xfce4-terminal" -- TODO: add a static priority list
  }

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [
    ((modMask myConfig, xK_b), bringMenu)
  , ((modMask myConfig, xK_g), gotoMenu)
  , ((modMask myConfig, xK_p), spawn  "$(yeganesh -x)")
  , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
  ]

myConfig' = myConfig `additionalKeys` myKeys
