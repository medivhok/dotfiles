import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import System.IO

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { terminal        = "termite"
    , modMask         = mod4Mask
    , workspaces      = ["1:Code", "2:Shell", "3:Web", "4:Files", "5:Graphics", "6:FTP", "7:Office", "8:Profit", "9:Misc."]
    , manageHook      = manageDocks <+> manageSpawn <+> manageHook def
    , layoutHook      = avoidStruts  $  layoutHook def
    , handleEventHook = def <+> docksEventHook
    , logHook         = dynamicLogWithPP xmobarPP
        { ppOutput          = hPutStrLn xmproc
        , ppTitle           = xmobarColor "darkgreen" "" . shorten 90
        , ppHiddenNoWindows = xmobarColor "grey" ""
        }
    , startupHook = do
        spawnOnce "compton"
        spawnOnce "xcape -t 300 -e \"Control_L=Escape;Control_R=Return\""
        spawnOnce "light-locker"
        spawnOn "3" "chromium"
        spawnOnce "stalonetray"
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "light-locker-command --lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    ]
