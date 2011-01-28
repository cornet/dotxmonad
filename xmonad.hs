import XMonad
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Config.Desktop
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LayoutHints
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.WindowGo
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Window
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Scratchpad
import System.IO
import DBus
import DBus.Connection
import DBus.Message
import Control.OldException
import Control.Monad
import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- This retry is really awkward, but sometimes DBus won't let us get our
-- name unless we retry a couple times.
getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
                                                getWellKnownName dbus)
 where
  tryGetName = do
    namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
    addArgs namereq [String "org.xmonad.Log", Word32 5]
    sendWithReplyAndBlock dbus namereq 0
    return ()


--
-- My Manage Hook
--
-- myManageHook :: [ManageHook]
myManageHook = composeAll (
    [ resource  =? "Do"                       --> doIgnore
    , resource  =? "Do.exe"                   --> doIgnore
    , resource  =? "/usr/lib/gnome-do/Do.exe" --> doIgnore
    , resource  =? "gnome-do"                 --> doIgnore
    , resource  =? "Dialog"                   --> doFloat
    , title     =? "Firefox Preferences"      --> doFloat
    , title     =? "Namoroka Preferences"     --> doFloat
    , title     =? "Thunderbird Preferences"  --> doFloat
    , title     =? "Shredder Preferences"     --> doFloat
    , className =? "Gimp-2.6"                 --> doFloat
    , className =? "Stickynotes_applet"       --> doFloat
    , manageHook gnomeConfig
    , scratchpadManageHook (W.RationalRect 0.1 0.1 0.75 0.75)
    ])


--
-- My Layout Hook
--
myLayoutHook = desktopLayoutModifiers $ toggleLayouts Full $ layoutHintsToCenter(tiled) ||| layoutHintsToCenter(Mirror tiled) ||| layoutHintsToCenter(Full) ||| layoutHintsToCenter(Grid)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

--
-- My Keybindings
--
myKeys conf@(XConfig {XMonad.modMask = modm}) =
    [ ((modm              , xK_F2 ), shellPrompt  defaultXPConfig)
    , ((modm              , xK_c  ), runOrRaise "google-chome" (className =? "Google-chrome"))
    , ((modm .|. shiftMask, xK_g  ), windowPromptGoto  defaultXPConfig)
    , ((modm .|. shiftMask, xK_b  ), windowPromptBring defaultXPConfig)
    , ((modm              , xK_s  ), scratchpadSpawnActionCustom "gnome-terminal --disable-factory --name scratchpad" )
    , ((modm .|. shiftMask, xK_s  ), SM.submap $ searchEngineMap $ S.selectSearch)
    , ((modm .|. shiftMask, xK_f  ), sendMessage ToggleLayout)
    ]

--
-- Search Engines
--
searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google)
       , ((0, xK_m), method S.maps)
       , ((0, xK_w), method S.wikipedia)
       ]

-- Merge myKeys with default config
newKeys x  = M.union (keys gnomeConfig x) (M.fromList (myKeys x))

--
-- Main Configuration
--
main :: IO ()
main = withConnection Session $ \ dbus -> do
     putStrLn "Getting well-known name."
     getWellKnownName dbus
     putStrLn "Got name, starting XMonad."
     xmonad $ withUrgencyHook NoUrgencyHook gnomeConfig
	    { modMask = mod4Mask 
            , keys = newKeys
	    , manageHook = myManageHook
	    , layoutHook = myLayoutHook
            , logHook    = dynamicLogWithPP $ defaultPP {
                             ppOutput   = \ str -> do
                               let str'  = "<span font=\"Droid Sans Mono 8\">" ++ str ++ 
                                           "</span>"
                                   str'' = sanitize str'
                               msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" 
                                          "Update"
                               addArgs msg [String str'']
                               -- If the send fails, ignore it.
                               send dbus msg 0 `catchDyn`
                                 (\ (DBus.Error _name _msg) ->
                                   return 0)
                               return ()
                  , ppTitle    = pangoColor "#ffffff" . shorten 25
                  , ppCurrent  = pangoColor "#ffffff" . wrap "[" "]"
                  , ppVisible  = pangoColor "#663366" . wrap "(" ")"
                  , ppHidden   = wrap "" ""
                  , ppUrgent   = pangoColor "red"
	    }
      }

--
-- Colouring and saitizing for xmonad-log-applet
--
pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

sanitize :: String -> String
sanitize [] = []
sanitize (x:rest) | fromEnum x > 127 = "&#" ++ show (fromEnum x) ++ "; " ++
                                       sanitize rest
                  | otherwise        = x : sanitize rest

