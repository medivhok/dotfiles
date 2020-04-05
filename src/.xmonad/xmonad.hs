-- The xmonad configuration of Derek Taylor (DistroTube)
-- http://www.youtube.com/c/DistroTube
-- http://www.gitlab.com/dwt1/

------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import Data.Maybe (isJust)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, spawnPipe)

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work

    -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..))
import XMonad.Actions.GridSelect

    -- Layouts modifiers
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.SimpleFloat
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomReset, ZoomMessage(ZoomFullToggle))

    -- Prompts
import XMonad.Prompt (Direction1D(..))

------------------------------------------------------------------------
---CONFIG
------------------------------------------------------------------------
myFont          = "xft:Mononoki Nerd Font:regular:pixelsize=12"
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "alacritty"      -- Sets default terminal
myTextEditor    = "emacs"     -- Sets default text editor
myBorderWidth   = 0         -- Sets border width for windows
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main ::IO()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc <- spawnPipe "xmobar"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh desktopConfig
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc x
                        , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#d0d0d0" "" . shorten 80     -- Title of active window in xmobar
                        , ppSep =  "<fc=#9AEDFE> : </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = "#292d3e"
        , focusedBorderColor = "#bbc5ff"
        } `additionalKeysP`         myKeys

------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
myStartupHook = do
  -- spawnOnce "xmobar &"
  --spawnOnce "nitrogen --restore &"
  --spawnOnce "compton --config /home/dt/.config/compton/compton.conf &"
  setWMName "LG3D"
  --spawnOnce "exec /usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --transparent true --alpha 0 --tint 0x292d3e --height 19 &"


------------------------------------------------------------------------
---GRID SELECT
------------------------------------------------------------------------

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x61,0x57,0x72) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg

-- gridSelect menu layout
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig

------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
myKeys =
    --- Xmonad
  [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
  , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
  , ("M-S-q", io exitSuccess)                  -- Quits xmonad

    --- Windows
  , ("M-S-c", kill1)                           -- Kill the currently focused client
  , ("M-S-a", killAll)                         -- Kill all the windows on current workspace

    --- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
  , ("M-S-<Delete>", sinkAll)                  -- Push ALL floating windows back to tile.

    --- Grid Select
  , (("M-S-t"), spawnSelected'
      [ ("Audacity", "audacity")
      , ("Deadbeef", "deadbeef")
      , ("Emacs", "emacs")
      , ("Firefox", "firefox")
      , ("Geany", "geany")
      , ("Geary", "geary")
      , ("Gimp", "gimp")
      , ("Kdenlive", "kdenlive")
      , ("LibreOffice Impress", "loimpress")
      , ("LibreOffice Writer", "lowriter")
      , ("OBS", "obs")
      , ("PCManFM", "pcmanfm")
      , ("Simple Terminal", "st")
      , ("Steam", "steam")
      , ("Surf Browser",    "surf suckless.org")
      , ("Xonotic", "xonotic-glx")
      ])

  , ("M-S-g", goToSelected $ mygridConfig myColorizer)
  , ("M-S-b", bringSelected $ mygridConfig myColorizer)

    --- Windows navigation
  , ("M-m", windows W.focusMaster)             -- Move focus to the master window
  , ("M-j", windows W.focusDown)               -- Move focus to the next window
  , ("M-k", windows W.focusUp)                 -- Move focus to the prev window
  , ("M-S-m", windows W.swapMaster)            -- Swap the focused window and the master window
  , ("M-S-j", windows W.swapDown)              -- Swap the focused window with the next window
  , ("M-S-k", windows W.swapUp)                -- Swap the focused window with the prev window
  , ("M-<Backspace>", promote)                 -- Moves focused window to master, all others maintain order
  , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
  , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
  , ("M-S-s", windows copyToAll)
  , ("M-C-s", killAllOtherCopies)

  , ("M-C-M1-<Up>", sendMessage Arrange)
  , ("M-C-M1-<Down>", sendMessage DeArrange)
  , ("M-<Up>", sendMessage (MoveUp 10))             --  Move focused window to up
  , ("M-<Down>", sendMessage (MoveDown 10))         --  Move focused window to down
  , ("M-<Right>", sendMessage (MoveRight 10))       --  Move focused window to right
  , ("M-<Left>", sendMessage (MoveLeft 10))         --  Move focused window to left
  , ("M-S-<Up>", sendMessage (IncreaseUp 10))       --  Increase size of focused window up
  , ("M-S-<Down>", sendMessage (IncreaseDown 10))   --  Increase size of focused window down
  , ("M-S-<Right>", sendMessage (IncreaseRight 10)) --  Increase size of focused window right
  , ("M-S-<Left>", sendMessage (IncreaseLeft 10))   --  Increase size of focused window left
  , ("M-C-<Up>", sendMessage (DecreaseUp 10))       --  Decrease size of focused window up
  , ("M-C-<Down>", sendMessage (DecreaseDown 10))   --  Decrease size of focused window down
  , ("M-C-<Right>", sendMessage (DecreaseRight 10)) --  Decrease size of focused window right
  , ("M-C-<Left>", sendMessage (DecreaseLeft 10))   --  Decrease size of focused window left

   --- Layouts
  , ("M-<Space>", sendMessage NextLayout)                              -- Switch to next layout
  , ("M-S-<Space>", sendMessage ToggleStruts)                          -- Toggles struts
  , ("M-S-n", sendMessage $ Toggle NOBORDERS)                          -- Toggles noborder
  , ("M-S-=", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
  , ("M-S-f", sendMessage (T.Toggle "float"))
  , ("M-S-x", sendMessage $ Toggle REFLECTX)
  , ("M-S-y", sendMessage $ Toggle REFLECTY)
  , ("M-S-m", sendMessage $ Toggle MIRROR)
  , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in the master pane
  , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in the master pane
  , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows that can be shown
  , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows that can be shown

  , ("M-C-h", sendMessage Shrink)
  , ("M-C-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)
  , ("M-S-;", sendMessage zoomReset)
  , ("M-;", sendMessage ZoomFullToggle)

    --- Workspaces
  , ("M-<KP_Add>", moveTo Next nonNSP)                                -- Go to next workspace
  , ("M-<KP_Subtract>", moveTo Prev nonNSP)                           -- Go to previous workspace
  , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next workspace
  , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to previous workspace

    --- Scratchpads
  , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-c", namedScratchpadAction myScratchPads "cmus")

  -- Applications
  -- Rofi
  , ("M-p", spawn "rofi -show run")
  , ("M-d", spawn "dia --integrated")
  -- Open Terminal
  , ("M-<Return>", spawn myTerminal)

  -- My Applications (Super+Alt+Key)
  , ("M-M1-a", spawn (myTerminal ++ " -e ncpamixer"))
  , ("M-M1-b", spawn ("surf www.youtube.com/c/DistroTube/"))
  , ("M-M1-c", spawn (myTerminal ++ " -e cmus"))
  , ("M-M1-e", spawn (myTerminal ++ " -e neomutt"))
  , ("M-M1-f", spawn (myTerminal ++ " -e sh ./.config/vifm/scripts/vifmrun"))
  , ("M-M1-i", spawn (myTerminal ++ " -e irssi"))
  , ("M-M1-j", spawn (myTerminal ++ " -e joplin"))
  , ("M-M1-l", spawn (myTerminal ++ " -e lynx -cfg=~/.lynx/lynx.cfg -lss=~/.lynx/lynx.lss gopher://distro.tube"))
  , ("M-M1-m", spawn (myTerminal ++ " -e toot curses"))
  , ("M-M1-n", spawn (myTerminal ++ " -e newsboat"))
  , ("M-M1-p", spawn (myTerminal ++ " -e pianobar"))
  , ("M-M1-r", spawn (myTerminal ++ " -e rtv"))
  , ("M-M1-w", spawn (myTerminal ++ " -e wopr report.xml"))
  , ("M-M1-y", spawn (myTerminal ++ " -e youtube-viewer"))

  -- Monitor brightness
  , ("<XF86MonBrightnessUp>", spawn ("xbacklight -inc 10"))
  , ("<XF86MonBrightnessDown>", spawn ("xbacklight -dec 10"))

  -- Multimedia Keys
  , ("<XF86AudioPlay>", spawn "cmus toggle")
  , ("<XF86AudioPrev>", spawn "cmus prev")
  , ("<XF86AudioNext>", spawn "cmus next")
  -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.

  , ("<XF86AudioMute>", spawn "pamixer -m")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -u -d 5")
  , ("<XF86AudioRaiseVolume>", spawn "pamixer -u -i 5")
  , ("<XF86HomePage>", spawn "firefox")
  , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
  , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
  , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
  , ("<XF86Eject>", spawn "toggleeject")
  , ("<Print>", spawn "scrotd 0")
  ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
          nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
               $ ["code", "shell", "web", "office", "graphics", "mus", "torrent", "sys", "misc"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "Chromium"    --> doShift "<action=xdotool key super+2>web</action>"
  , (className =? "Chromium" <&&> resource =? "Dialog") --> doFloat
  , className =? "Blender"     --> doShift "<action=xdotool key super+5>graphics</action>"
  , (className =? "Blender" <&&> resource =? "Dialog") --> doFloat
  , className =? "cmus"        --> doShift "<action=xdotool key super+7>mus</action>"
  , className =? "vlc"         --> doShift "<action=xdotool key super+7>media</action>"
  , className =? "Gimp"        --> doShift "<action=xdotool key super+8>graphics</action>"
  ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
-- Layouts
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ noBorders $
               gaps [(U,3), (D,3), (L,3), (R,3)] $
               spacingRaw True (Border 4 4 4 4) True (Border 4 4 4 4) True $
               Tall 1 (3/100) (1/2) ||| mirrorTall ||| Full ||| simpleFloat
             where
               mirrorTall = Mirror (Tall 1 (3/100) (1/2))

------------------------------------------------------------------------
---SCRATCHPADS
------------------------------------------------------------------------

myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "cmus" spawnCmus findCmus manageCmus
                ]

    where
    spawnTerm  = myTerminal ++  " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCmus  = myTerminal ++  " -n cmus 'cmus'"
    findCmus   = resource =? "cmus"
    manageCmus = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w