import XMonad
import XMonad.Util.EZConfig (additionalKeys)

main :: IO ()
main = xmonad myConfig'

myConfig = def {
    modMask = mod4Mask -- Use Super instead of Alt
  }

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [
   ((modMask myConfig, xK_p ), spawn  "$(yeganesh -x)")
-- , ((modMask myConfig, xK_enter ), spawn  "$(xfce4-terminal)")
 ]

myConfig' = myConfig `additionalKeys` myKeys
