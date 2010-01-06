import XMonad
import XMonad.Config.Gnome
import XMonad.ManageHook
import qualified Data.Map as M

myManageHook :: [ManageHook]
myManageHook =
    [ resource =? "Do" --> doIgnore
    , resource =? "Do.exe" --> doIgnore
    , resource =? "/usr/lib/gnome-do/Do.exe" --> doIgnore
    , resource =? "gnome-do" --> doIgnore
    , className =? "Gimp" --> doFloat ]

main = xmonad gnomeConfig
    { modMask = mod4Mask 
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    }
