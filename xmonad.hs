import Control.Concurrent (threadDelay)
import System.Directory (getHomeDirectory)
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

