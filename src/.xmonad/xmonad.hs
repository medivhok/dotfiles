import XMonad
import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing

import XMonad.Util.EZConfig
import XMonad.Util.Run

import GHC.IO.Handle
import Graphics.X11.Xinerama
import System.Directory

-- -----------------------------------------------------------------------------
-- Configurations
--
defaultFont :: String
defaultFont = "xft:Mononoki Nerd Font:regular:pixelsize=12"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

textEditor :: String
textEditor = "emacs"

defaultBorderWidth :: Dimension
defaultBorderWidth = 0

batteryFilePath :: String
batteryFilePath = "/sys/class/power_supply/"

-- -----------------------------------------------------------------------------
-- Autostart
--
myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

-- -----------------------------------------------------------------------------
-- Keybindings
--
--keybindings :: [(String, m ())]
keybindings = [ ("M-p", spawn "rofi -show run")
              , ("M-d", spawn "dia --integrated")
              -- Open MyTerminal
              , ("M-<Return>", spawn myTerminal)
              , ("<XF86AudioMute>", spawn "amixer set Master toggle")
              , ("<XF86AudioLowerVolume>", spawn "amixer set Master 4%- unmute")
              , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 4%+ unmute")
              , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
              , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")

              -- Applications
              , ("M-o b", spawn "chromium")
              , ("M-o e", spawn "emacs")
              , ("M-o f", spawn "dolphin")
              , ("M-o l", spawn "libreoffice")
              , ("M-o p", spawn "qpdfview")
              ]

-- -----------------------------------------------------------------------------
-- Workspaces
--
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
             $ [ "1"
               , "2"
               , "3"
               , "4"
               , "5"
               , "6"
               , "7"
               , "8"
               , "9"
               ]
  where
    clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                    (i,ws) <- zip [1..9] l,
                    let n = i ]

-- -----------------------------------------------------------------------------
-- Layouts
--
myLayoutHook = avoidStruts $ noBorders $
             gaps [(U,4), (D,4), (L,4), (R,4)] $
             spacingRaw True (Border 4 4 4 4) True (Border 4 4 4 4) True $
             Tall 1 (3/100) (1/2) ||| mirrorTall ||| Full ||| simpleFloat
  where
    mirrorTall = Mirror (Tall 1 (3/100) (1/2))

-- -----------------------------------------------------------------------------
-- XMonad configurations
--
main :: IO()
main = do
  screens <- getScreens
  xmobars <- spawnXMobars screens

  XMonad.xmonad $ ewmh desktopConfig
    { manageHook         = manageDocks
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = updateXMobars xmobars
                , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                , ppTitle = xmobarColor "#d0d0d0" "" . shorten 80     -- Title of active window in xmobar
                , ppSep =  "<fc=#9AEDFE> : </fc>"                     -- Separators in xmobar
                , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                , ppOrder  = \(ws:_:_:_) -> [ws]
                }
    , modMask            = myModMask
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , layoutHook         = myLayoutHook
    , workspaces         = myWorkspaces
    , borderWidth        = defaultBorderWidth
    , normalBorderColor  = "#292d3e"
    , focusedBorderColor = "#bbc5ff"
    } `additionalKeysP` keybindings


-- -----------------------------------------------------------------------------
-- Utility functions
--
getScreens :: XMonad.MonadIO m => m [Int]
getScreens = XMonad.liftIO $ do
  screens <- do
    dpy <- XMonad.openDisplay ""
    rects <- getScreenInfo dpy
    XMonad.closeDisplay dpy
    return rects
  let ids = zip [0 .. ] screens
  return [fst x | x <- ids]

spawnXMobar :: Int -> IO Handle
spawnXMobar sid = do
    hasBattery <- doesDirectoryExist $ batteryFilePath ++ "BAT0"
    if hasBattery
      then spawnPipe $ "xmobar ~/.config/xmobar/xmobarrc_with_battery -x " ++ show sid
      else spawnPipe $ "xmobar -x " ++ show sid

spawnXMobarsRecursively :: [Int] -> [Handle] -> IO [Handle]
spawnXMobarsRecursively [] xmobars = return xmobars
spawnXMobarsRecursively (x:xs) xmobars = do
    handle <- spawnXMobar x
    spawnXMobarsRecursively xs (handle:xmobars)

spawnXMobars :: [Int] -> IO [Handle]
spawnXMobars sids = spawnXMobarsRecursively sids []

updateXMobars :: [Handle] -> String -> IO ()
updateXMobars [] _ = return ()
updateXMobars (x:xs) dataString = do
    hPutStrLn x dataString
    updateXMobars xs dataString
