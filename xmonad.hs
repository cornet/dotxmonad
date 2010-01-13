import XMonad
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Config.Desktop
import XMonad.Layout.Grid
import qualified Data.Map as M

myManageHook :: [ManageHook]
myManageHook =
    [ resource =? "Do" --> doIgnore
    , resource =? "Do.exe" --> doIgnore
    , resource =? "/usr/lib/gnome-do/Do.exe" --> doIgnore
    , resource =? "gnome-do" --> doIgnore
    , className =? "Gimp-2.6" --> doFloat ]

myLayoutHook = desktopLayoutModifiers $ tiled ||| Mirror tiled ||| Full ||| Grid
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

main = xmonad gnomeConfig
    { modMask = mod4Mask 
    , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
    , layoutHook = myLayoutHook
    }
