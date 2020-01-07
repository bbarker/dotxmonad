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
  _ <- spawnPipe "pkill -9 xmobar"
  _ <- threadDelay 10000

  home <- getHomeDirectory
  let xmobarBin = home <> "/.local/bin/xmobar"
  let xmobarRc = home <> "/.xmonad/xmobarrc"
  xmproc <- spawnPipe $ xmobarBin <> " " <> xmobarRc
  xmonad myConfig' {
    logHook = dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "green" "" . shorten 50
    }
  }

myConfig = def {
    manageHook = manageDocks <+> manageHook def
  , layoutHook = avoidStruts  $  layoutHook def
   -- this must be in this order, docksEventHook must be last:
  -- , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook
  , modMask = mod4Mask -- Use Super instead of Alt
  }

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [
    ((modMask myConfig, xK_p), spawn  "$(yeganesh -x)")
  , ((modMask myConfig, xK_KP_Enter), spawn  "$(xfce4-terminal)")
  , ((modMask myConfig, xK_g), gotoMenu)
  , ((modMask myConfig, xK_b), bringMenu)
  ]

myConfig' = myConfig `additionalKeys` myKeys
