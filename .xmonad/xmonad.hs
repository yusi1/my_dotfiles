-- My XMonad Configuration File
-- Written By Yusef Aslam
-- Based on John Goerzens Configuration
-- Default Keys https://gist.github.com/c33k/1ecde9be24959f1c738d
-- This config may be messy, but I am a beginner so please don't mind that.

----------------------Imports----------------------------------------------

module Main (main) where

import XMonad

import qualified Data.Map as M
--import Data.Default
--import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
--import System.Directory
--import System.IO
--import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import Data.Monoid
import Control.Monad (liftM2)
--import Data.Ratio ((%))

--import Text.Printf

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops(fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers   -- Might take this out later (Improving Multi-mon support) #MM
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
--import XMonad.Hooks.ToggleHook
--import XMonad.Hooks.DynamicBars

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare
--import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
--import XMonad.Util.Scratchpad

import XMonad.Layout
--import XMonad.Layout.Combo
--import XMonad.Layout.LayoutCombinators ((|||))
--import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
--import XMonad.Layout.NoBorders(OnlyFloat)
import XMonad.Layout.NoBorders
--import XMonad.Layout.Grid
--import XMonad.Layout.TwoPane (TwoPane(..))
--import XMonad.Layout.Tabbed
-- import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
--import XMonad.Layout.Maximize
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.AvoidFloats
import XMonad.Layout.ResizableTile -- Resizable Tall Layout
import XMonad.Layout.Renamed -- Rename Layouts
import XMonad.Layout.MultiToggle as MT (Toggle(..))
--import XMonad.Layout.Drawer
import XMonad.Layout.ThreeColumns
--import XMonad.Layout.IfMax
import XMonad.Layout.BinarySpacePartition hiding (Swap) -- unambiguise (Swap)
import XMonad.Layout.Accordion
import XMonad.Layout.AvoidFloats

import XMonad.Actions.UpdatePointer -- update pointer location to edge of new focused window, to prevent unintended focus stealing
import XMonad.Actions.CycleRecentWS -- cycle recent workspaces with keys defined in myKeys
--import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.Promote -- Promote selected window to master pane
--import XMonad.Actions.Search -- use search engine in XMonad
import XMonad.Actions.CycleWS -- Cycle Workspaces, for example using the arrow keys
--import XMonad.Actions.CycleWindows -- Cycle windows in current workspace
-- import XMonad.Actions.WindowNavigation -- Experimental rewrite of layout with same name, allows window navigation with arrow keys
--import XMonad.Actions.Volume

import System.IO

---------------------------------------------------------------------
-- App defaults names

myTerminal = "alacritty"
myFallBackTerminal = "xterm"

---------------------------------------------------------------------
-- Define some variables for the manageHook, which contain application names
-- To help with mass editing of how application windows are manipulated
-- Got this idea from (https://wiki.haskell.org/Xmonad/Using_xmonad_in_KDE)

gameApps = [
            --"Minecraft* 1.16.5",
            "Minecraft Launcher",
            --,"minecraft-launcher",
            "Steam","powder-toy","Lutris"
           ]

mediaApps = ["Audacity","mpv","vlc","LBRY","obs"]
officeApps = ["Xarchiver","Soffice","Epdfview","llpp","libreoffice","LibreOffice"]

webApps = ["firefox","IceCat","Chromium","LibreWolf","Brave-browser","qutebrowser"]
systemApps = ["qnvsm","Gnome-disks","Nvidia-settings","ckb-next","openrgb"]
virtApps = ["Vmware","VirtualBox","Virt-manager"]

generalApps = ["firetools","qBittorrent","calibre","Pcmanfm","Mailspring","KeePassXC","Mousepad"]
devApps = ["Code","Godot"]
osintApps = ["Maltego"]

socialApps = ["Microsoft Teams"]
otherApps = ["Progress","Xmessage"]
floatApps = ["Dialog"]
customClasses = ["sandboxed","scratchpad"]

-- # [ excluded apps: ]
-- [*] Pavucontrol


-- [ Manipulate windows as they are created.
-- | The list is processed from top -> bottom, 
-- [ the first matching rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.


viewShift = doF . liftM2 (.) W.greedyView W.shift

doShiftWS :: Int -> ManageHook
doShiftWS a = doShift ( myWorkspaces !! a ) <+> viewShift ( myWorkspaces !! a )

doWSNoShift :: Int -> ManageHook
doWSNoShift a = doShift ( myWorkspaces !! a )

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
    [
    [className =? gA --> doShiftWS 6 | gA <- gameApps]
    , [className =? head customClasses --> doShiftWS 8]
    , [className =? sA --> doShiftWS 2 | sA <- systemApps]
    , [className =? dA --> doShiftWS 0 | dA <- devApps]
    , [resource =? flA --> doFloat | flA <- floatApps]
    , [className =? otA --> doFloat | otA <- otherApps]
    , [className =? vA --> doShiftWS 3 | vA <- virtApps]
    , [className =? head generalApps --> doFloat]
    , [className =? floA --> doFloat | floA <- take 2 officeApps] 
    , [className =? mA --> doShiftWS 5 | mA <- mediaApps]
    , [isFullscreen --> doFullFloat]
    ]

--------------------------------------------------------------------
-- [ My Workspaces ]

myWorkspaces :: [String]
myWorkspaces = [" 1:dev ", " 2:www ", " 3:sys ", " 4:virt ", " 5:doc ", " 6:media ", " 7:game ", " 8:osint ", " 9:sbox "]
-- Offset:     ["   0   ", "   1   ", "   2   ", "    3   ", "    4    ", "   5   ", "    6   ", "   7   ", "    8    "]

-------------------------------------------------------------------
-- [ Scratchpad config ]

--manageScratchpad :: ManageHook
--manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)    
                   --where
                        --h = 0.4    -- terminal height
                        --w = 0.3       -- terminal width
                        --t = 0.6   -- distance from top edge
                        --l = 0.7 -- distance from left edge

-- ^- for util.scratchpad, below is for util.namedscratchpad

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "volumectl" spawnMixer findMixer manageMixer
                , NS "dnsmon" spawnDnsMon findDnsMon manageDnsMon
                , NS "notes" spawnNotes findNotes manageNotes
                , NS "emacs" spawnEmacs findEmacs manageEmacs
                , NS "torrent" spawnqB findqB manageqB
                , NS "fileman" spawnfileMan findfileMan managefileMan
                ]
        where
        -- [Alacritty]
            spawnTerm = myTerminal ++ " --class Alacritty,scratchpad"
            findTerm = className =? "scratchpad"
            manageTerm = customFloating $ W.RationalRect l t w h
                where
                    h = 0.9
                    w = 0.9
                    t = 0.95 - h
                    l = 0.95 - w
            
        -- [Pavucontrol]
            spawnMixer = "pavucontrol"
            findMixer = className =? "Pavucontrol"
            manageMixer = customFloating $ W.RationalRect l t w h
                where
                    h = 0.9
                    w = 0.9
                    t = 0.95 - h
                    l = 0.95 - w

        -- [Firejail DNS Monitoring]
            spawnDnsMon = myTerminal ++ " --class Alacritty,scratchdnsmon -e fdns --monitor"
            findDnsMon = className =? "scratchdnsmon"
            manageDnsMon = customFloating $ W.RationalRect l t w h
                where
                    h = 0.4
                    w = 0.4
                    t = 1 - h
                    l = 0.6

        -- [Quick Notes]
            spawnNotes = "mousepad"
            findNotes = className =? "Mousepad"
            manageNotes = customFloating $ W.RationalRect l t w h
                where
                    h = 0.4
                    w = 0.4
                    t = 1 - h
                    l = 0.0

        -- [Emacs]
            spawnEmacs = "emacs -T 'scratchemacs'"
            findEmacs = title =? "scratchemacs"
            manageEmacs = customFloating $ W.RationalRect l t w h
                where
                    h = 0.9
                    w = 0.9
                    t = 0.95 - h
                    l = 0.95 - w

        -- [QBittorrent]
            spawnqB = "qbittorrent"
            findqB = className =? "qBittorrent"
            manageqB = customFloating $ W.RationalRect l t w h
                where
                    h = 0.9
                    w = 0.9
                    t = 0.95 - h
                    l = 0.95 - w

        -- [PcManFM] File Manager
            spawnfileMan = "pcmanfm"
            findfileMan = className =? "Pcmanfm"
            managefileMan = customFloating $ W.RationalRect l t w h
                where
                    h = 0.9
                    w = 0.9
                    t = 0.95 - h
                    l = 0.95 - w

-- Hide scratchpad workspace
noScratchPad ws = if ws == "NSP" then "" else ws

scratchTerm = namedScratchpadAction myScratchPads "terminal"
scratchMixer = namedScratchpadAction myScratchPads "volumectl"
scratchDnsMon = namedScratchpadAction myScratchPads "dnsmon"
scratchNotes = namedScratchpadAction myScratchPads "notes"
scratchEmacs = namedScratchpadAction myScratchPads "emacs"
scratchqB = namedScratchpadAction myScratchPads "torrent"
scratchfileMan = namedScratchpadAction myScratchPads "fileman"

-------------------------------------------------------------------

mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tiled :: ModifiedLayout Rename ResizableTall a
tiled = renamed [Replace "Tall"] $ ResizableTall 1 (3/100) (1/2) []       -- Rename Resizable Tall to Tall. Easier Tall layout assignment & changing

defSpacing :: l a -> ModifiedLayout Spacing l a
defSpacing = mySpacing 8            -- Default Spacing

tiledSp = defSpacing tiled
bspSp = defSpacing bsp
threecolSp = defSpacing threecol
threecolMSp = defSpacing threecolmid

threecol = renamed [Replace "ThreeCol"] $ ThreeCol 1 (3/100) (1/2)
threecolmid = renamed [Replace "ThreeColMid"] $ ThreeColMid 1 (3/100) (1/2)
bsp = renamed [Replace "BSP"] $ emptyBSP 
accordion = renamed [Replace "Accordion"] $ Accordion
avoidfloats = renamed [Replace "AvoidFloats"] $ avoidFloats Full

-- Toggle Layouts in "Pairs" (Very Useful)
tiledToggle = toggleLayouts tiledSp (tiled)
bspToggle = toggleLayouts bspSp (bsp)
threecolToggle = toggleLayouts threecolSp (threecol)
threecolToggle' = toggleLayouts threecolMSp (threecolmid)

---- Add Some More Modifiers To The Layouts ----

--tiled' = avoidStruts $ smartBorders tiled

--tiledSp' = avoidStruts $ smartBorders tiledSp

--threecol' = avoidStruts $ smartBorders threecol
--threecolmid' = avoidStruts $ smartBorders threecolmid

--------------------------------------------

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--------------------------------------------------------------------

myLayoutHook = windowNavigation $ mkToggle (NBFULL ?? EOT) $ avoidStruts $ smartBorders ( 
                tiledToggle 
                ||| threecolToggle ||| threecolToggle' ||| bspToggle ||| avoidfloats ||| accordion
                )

--------------------------------------------------------------------
-- [ Fade Inactive Windows ]
-- Currently set to one, but is here in case I want to change it;
-- In the future.

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1

-------------------------------------------------------------------
-- Startup Hook (check .xprofile too)

myStartupHook :: X ()
myStartupHook = do
            -- Put apps you want XMonad to start in here
            -- example:
            -- spawnOnce "ckb-next & disown"
            setWMName "LG3D"    -- For java application support
            spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --transparent true --alpha 0 --tint 0x1A1C21 --widthtype request --monitor 0 --height 24 & disown"
            spawnOnce "ckb-next -b & disown"
            spawnOnce "openrgb --startminimized & disown; openrgb -p new.orp & disown"
            spawnOnce "protonvpn-applet & disown"

-------------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar -x 0 /home/yusef/.config/xmobar/.xmobarrc"
    --xmproc1 <- spawnPipe "xmobar -x 1 /home/yusef/.config/xmobar/.xmobarrc2"
    xmonad $ ewmh $ docks def
      {
          borderWidth         = 3
          , terminal          = myTerminal
          --, layoutHook        = smartBorders . avoidStruts . spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ layoutHook defaultConfig
          , startupHook = myStartupHook
          , workspaces  = myWorkspaces
          , manageHook  = myManageHook <+> namedScratchpadManageHook myScratchPads
          , layoutHook        = myLayoutHook
          , logHook           = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP {
                                --ppOutput = \x -> hPutStrLn xmproc x  >> hPutStrLn xmproc1 x
                                ppOutput = hPutStrLn xmproc
                              , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]" -- Current workspace in xmobar
                              , ppVisible = xmobarColor "#98be65" ""                -- Visible but not current workspace
                              , ppHidden = xmobarColor "#98be65" "" . wrap "*" "" . noScratchPad -- Hidden workspaces in xmobar
                              , ppHiddenNoWindows = xmobarColor "#c792ea" "" . noScratchPad       -- Hidden workspaces (no windows)
                              , ppTitle = xmobarColor "#b3afc2" "" . shorten 40    -- Title of active window in xmobar
                              , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"          -- Separators in xmobar
                              , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                              , ppExtras  = [windowCount]   -- # of windows current workspace
                              --, ppSort = getSortByXineramaRule
                              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                              }
                              -- >> updatePointer (0.95, 0.95) (0.95, 0.95)
                              -- >> updatePointer (1, 1) (0, 0)
                              >> updatePointer (0.95, 0.95) (0, 0)
          --, focusedBorderColor = "#2aa198"
          , focusedBorderColor = "#46d9ff"
          , normalBorderColor = "#282c34"           
          , handleEventHook    = handleEventHook def <+> fullscreenEventHook
          -- , modMask = mod1Mask    -- Rebind Mod (Default is ALT) to the Windows Key
      }
          `additionalKeys`
           [
            --((controlMask, xK_F1), spawn "pcmanfm")       -- spawn app (CTRL F1)
            ((controlMask, xK_F2), spawn "librewolf")    -- spawn app (CTRL F2)
            --, ((controlMask, xK_F3), spawn "epdfview")  -- spawn app (CTRL F3)
            , ((controlMask, xK_F3), spawn "brave")  -- spawn app (CTRL F3)
            --, ((mod1Mask, xK_r), spawn "alacritty -e ~/spawnjailedapps.sh")
            -- [Turn off pc using script]
            , ((mod1Mask .|. shiftMask, xK_q), spawn "~/Documents/powermenu.sh")
            -- [Toggle AvoidFloats]
            , ((mod1Mask .|. shiftMask, xK_equal), sendMessage AvoidFloatToggle)
            , ((mod1Mask .|. controlMask, xK_equal), withFocused $ sendMessage . AvoidFloatToggleItem)
            , ((mod1Mask .|. shiftMask .|. controlMask, xK_equal), sendMessage (AvoidFloatSet False) >> sendMessage AvoidFloatClearItems)
            ------
            , ((controlMask .|. mod4Mask, xK_F3), spawn "~/./spawnjailedbravebrowser.sh")
            , ((controlMask .|. mod4Mask, xK_F2), spawn "~/./spawnjailedlibrewolf.sh")
            , ((mod1Mask, xK_b), spawn "buku-dmenu")
            , ((mod1Mask .|. shiftMask, xK_b), spawn "blueman-manager & disown")
            , ((mod1Mask, xK_p), spawn "dmenu_run -nb '#1a1c21' -nf '#c792ea' -sb '#ff6c6b' -fn 'UbuntuMono Nerd Font Mono:style=Bold:size=11'")
            , ((mod1Mask, xK_c), spawn "clipmenu -nb '#1a1c21' -nf '#c792ea' -sb '#ff6c6b' -fn 'UbuntuMono Nerd Font Mono:style=Bold:size=11'")
            , ((controlMask .|. mod1Mask, xK_b), spawn "bitwarden")
            , ((controlMask, xK_F4), spawn "emacs")        -- spawn app (CTRL F4)
            --, ((mod1Mask .|. controlMask, xK_b), spawn "icecat") -- spawn browser (C-M-b)
            , ((mod4Mask, xK_r), spawn "jgmenu_run") -- Open application menu Windows Key+r
            -- [Scratchpads]
            , ((mod1Mask, xK_a), scratchTerm)
            , ((mod1Mask, xK_v), scratchMixer)
            , ((mod1Mask, xK_m), scratchDnsMon)
            , ((mod1Mask, xK_n), scratchNotes)
            , ((mod1Mask, xK_e), scratchEmacs)
            , ((mod1Mask .|. mod4Mask, xK_f), scratchfileMan)
            , ((mod1Mask .|. mod4Mask, xK_t), scratchqB)
            ----
            --, ((mod1Mask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave) -- Cycle workspaces (ALT TAB)
            , ((mod1Mask .|. mod4Mask, xK_Return), promote)                          -- Promote selected window to master pane
            , ((mod1Mask .|. controlMask, xK_Right), nextWS)           -- shift to next WS (ALT UP-ARROW)
            , ((mod1Mask .|. controlMask, xK_Left), prevWS)            -- shift to previous WS (ALT DOWN-ARROW)
            --, ((mod1Mask .|. controlMask, xK_Left), DO.swapWith Prev NonEmptyWS)
            --, ((mod1Mask .|. controlMask, xK_Right), DO.swapWith Next NonEmptyWS)
            , ((mod1Mask .|. controlMask .|. shiftMask, xK_n),  shiftToNext)         -- shift to next WS (ALT + SHIFT DOWN ARROW)
            , ((mod1Mask .|. controlMask .|. shiftMask, xK_p),  shiftToPrev)           -- shift window to previous workspace (ALT + SHIFT UP ARROW)
            --------------------------------------------------
            -- Manage Windows Easily Using Arrowkeys
            , ((mod1Mask,                 xK_Right), sendMessage $ Go R)
            , ((mod1Mask,                 xK_Left ), sendMessage $ Go L)
            , ((mod1Mask,                 xK_Up   ), sendMessage $ Go U)
            , ((mod1Mask,                 xK_Down ), sendMessage $ Go D)
            , ((mod1Mask .|. shiftMask, xK_Right), sendMessage $ Swap R)
            , ((mod1Mask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
            , ((mod1Mask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
            , ((mod1Mask .|. shiftMask, xK_Down ), sendMessage $ Swap D)
            -- [Specific window manipulations keys for BSP layout]
            , ((mod1Mask .|. mod4Mask .|. shiftMask, xK_Up), sendMessage $ ShrinkFrom U)
            , ((mod1Mask .|. mod4Mask .|. shiftMask, xK_Down), sendMessage $ ShrinkFrom D)
            , ((mod1Mask .|. mod4Mask .|. shiftMask, xK_Left), sendMessage $ ShrinkFrom L)
            , ((mod1Mask .|. mod4Mask .|. shiftMask, xK_Right), sendMessage $ ShrinkFrom R)
            , ((mod1Mask .|. mod4Mask .|. controlMask, xK_Up), sendMessage $ ExpandTowards U)
            , ((mod1Mask .|. mod4Mask .|. controlMask, xK_Down), sendMessage $ ExpandTowards D)
            , ((mod1Mask .|. mod4Mask .|. controlMask, xK_Left), sendMessage $ ExpandTowards L)
            , ((mod1Mask .|. mod4Mask .|. controlMask, xK_Right), sendMessage $ ExpandTowards R)
            , ((mod1Mask, xK_r), sendMessage Rotate)
            , ((mod1Mask .|. mod4Mask, xK_a), sendMessage Balance)
            , ((mod1Mask .|. mod4Mask .|. shiftMask, xK_a), sendMessage Equalize)
            -- Window Resizing
            , ((mod1Mask .|. controlMask .|. shiftMask, xK_Up), sendMessage MirrorExpand)
            , ((mod1Mask .|. controlMask .|. shiftMask, xK_Down), sendMessage MirrorShrink)
            , ((mod1Mask .|. controlMask .|. shiftMask, xK_Left), sendMessage Shrink)
            , ((mod1Mask .|. controlMask .|. shiftMask, xK_Right), sendMessage Expand)
            -------------------------------------------------
            -- Switch focus to different screens easily
            -- Default: ALT (W,E,R)
            -- Where :
            -- [+] W is screen 1 
            -- [+] E is screen 2 
            -- [+] R is screen 3
            -- But now, no matter how much screens I have, this will make sense as a keybind
            
            , ((mod1Mask .|. mod4Mask, xK_Right), nextScreen)
            , ((mod1Mask .|. mod4Mask, xK_Left), prevScreen)

            -- [ Shift Windows to Other Screens Easily ]
            , ((mod1Mask .|. mod4Mask, xK_Up), shiftNextScreen)
            , ((mod1Mask .|. mod4Mask, xK_Down), shiftPrevScreen)

            --------------------------------------------------
            -- Toggle Modes
            , ((mod1Mask, xK_Return), sendMessage (MT.Toggle NBFULL))
            --, ((mod1Mask, xK_f), sendMessage (Toggle "realFull"))
            , ((mod1Mask .|. mod4Mask, xK_b), sendMessage ToggleStruts)  -- Toggle struts aka XMobar using a keybinding (ALT + F)

            --------------------------------------------------
            -- Seperate Workspace shortcuts (2nd monitor)
            --, ((mod1Mask, xK_k), windows $ onCurrentScreen f i)
            --------------------------------------------------


            , ((mod1Mask, xK_F7), spawn "/usr/bin/pamixer -d 2") -- decrease volume by n
            , ((mod1Mask, xK_F8), spawn "/usr/bin/pamixer -i 2") -- increase volume by n
            , ((mod1Mask, xK_F5), spawn "/usr/bin/pamixer -t") -- togglemute

            
            , ((mod1Mask .|. controlMask, xK_i), incWindowSpacing 4)    -- Increase Window Spacing on the Fly
            , ((mod1Mask .|. controlMask, xK_d), decWindowSpacing 4)    -- Decrease Window Spacing on the Fly
            
            , ((mod1Mask, xK_Print), spawn "flameshot gui") -- screenshot
            
            , ((mod1Mask, xK_F12), spawn "killall picom; picom -b & disown") -- Restart Compositor

            , ((mod1Mask .|. controlMask, xK_k), spawn "mousepad ~/Documents/keybinds.txt")  -- Show keybinds
            
            , ((mod1Mask, xK_F11), spawn "killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --transparent true --alpha 0 --tint 0x000000 --widthtype request --monitor 1 --height 24 & disown") -- Restart Trayer
            
            --, ((mod1Mask .|. controlMask, xK_m), spawn "mailspring") -- spawn mail client

            , ((mod1Mask .|. controlMask, xK_space), sendMessage ToggleLayout) -- Toggle Layouts, specified in LayoutHook

            --, ((mod1Mask, xK_f), moveTo Next EmptyWS)                   -- find a free workspace (ALT F)
            --, ((mod1Mask .|. controlMask, xK_f), moveTo Next NonEmptyWS)  -- (ALT + SHIFT F) cycle between non-empty workspaces (application opened in them)
            --, ((modm .|. shiftMask, xK_Up),    shiftToPrev)
            --, ((modm,               xK_Right), nextScreen)
            --, ((modm,               xK_Left),  prevScreen)
            --, ((modm .|. shiftMask, xK_Right), shiftNextScreen)
            --, ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
            , ((mod1Mask .|. controlMask, xK_z), toggleWS)                -- (ALT + Z) cycle between workspaces that are being used
            --, ((mod1Mask, xK_F7, lowerVolume 3 >> return ()))

            --, ((mod1Mask, xK_Return), sendMessage ToggleLayout) -- Toggle Layouts (VERY HANDY)
            --, ((mod1Mask .|. controlMask, xK_f), sendMessage (Toggle "nBFull"))
            --, ((mod1Mask .|. shiftMask, xK_f), myLayout)
            --, ((mod1Mask .|. controlMask, xK_Right),                  -- a crazy keybinding!
                  --do t <- findWorkspace getSortByXineramaRule Next NonEmptyWS 2
                    --windows . view $ t )
           ]
                
