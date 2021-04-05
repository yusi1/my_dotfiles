-- My XMonad Configuration File
-- Written By Yusef Aslam
-- Based on John Goerzens Configuration
-- Default Keys https://gist.github.com/c33k/1ecde9be24959f1c738d
-- This config may be messy, but I am a beginner so please don't mind that.

----------------------Imports----------------------------------------------

module Main (main) where

import XMonad

import qualified Data.Map as M
import Data.Maybe (fromJust)
--import Data.Default
--import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
--import System.Directory
--import System.IO
--import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import Data.Monoid
import Control.Monad (liftM2)
import Data.Tree
--import Data.Ratio ((%))

--import Text.Printf

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops(fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers   -- Might take this out later (Improving Multi-mon support) #MM
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
--import XMonad.Hooks.ToggleHook
--import XMonad.Hooks.DynamicBars
import XMonad.Hooks.ServerMode -- XMonad server mode: read input from external clients

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.WorkspaceCompare
--import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
--import XMonad.Util.Scratchpad

-- Prompt Libs
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch

-- Prompts
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
--import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Ssh
--import XMonad.Prompt.Pass

import XMonad.Layout
--import XMonad.Layout.Combo
--import XMonad.Layout.LayoutCombinators ((|||))
--import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
--import XMonad.Layout.NoBorders(OnlyFloat)
import XMonad.Layout.NoBorders
--import XMonad.Layout.Grid
--import XMonad.Layout.TwoPane (TwoPane(..))
-- import XMonad.Layout.Tabbed
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
import XMonad.Actions.Search -- use search engine in XMonad
import XMonad.Actions.CycleWS -- Cycle Workspaces, for example using the arrow keys
--import XMonad.Actions.CycleWindows -- Cycle windows in current workspace
-- import XMonad.Actions.WindowNavigation -- Experimental rewrite of layout with same name, allows window navigation with arrow keys
--import XMonad.Actions.Volume
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.GridSelect

import System.IO

---------------------------------------------------------------------
-- App defaults names

myTerminal :: String
myTerminal = "alacritty"

myFallBackTerminal :: String
myFallBackTerminal = "xterm"

-- Other good variables
myFont :: String
myFont = "xft:Terminus:pixelsize=11"

myBrowser = "/usr/bin/librewolf"
myBrowser' = "/usr/bin/brave"

myDDG = intelligent duckduckgo
myHak = intelligent hackage

myServer = "/home/yusef/.xmonad/xmonadctl"

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

mediaApps = ["Audacity","mpv","vlc","LBRY","obs","Clementine","music"]
officeApps = ["Xarchiver","Soffice","Epdfview","llpp","libreoffice","LibreOffice"]

webApps = ["firefox","IceCat","Chromium","LibreWolf","Brave-browser","qutebrowser"]
systemApps = ["qnvsm","Gnome-disks","Nvidia-settings","ckb-next","openrgb"]
virtApps = ["Vmware","VirtualBox","Virt-manager"]

generalApps = ["firetools","qBittorrent","calibre","Pcmanfm","Mailspring","KeePassXC","Mousepad"]
devApps = ["Code","Godot"]
osintApps = ["Maltego"]

socialApps = ["Microsoft Teams"]
otherApps = ["Progress","Xmessage","XClock","Zenity"]
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
    [className =? c --> doShiftWS 6 | c <- gameApps]
    , [className =? head customClasses --> doShiftWS 8]
    , [className =? c --> doShiftWS 2 | c <- systemApps]
    , [className =? c --> doShiftWS 0 | c <- devApps]
    , [resource =? r --> doFloat | r <- floatApps]
    , [className =? c --> doFloat | c <- otherApps]
    , [className =? c --> doShiftWS 3 | c <- virtApps]
    , [className =? head generalApps --> doFloat]
    , [className =? c --> doFloat | c <- take 2 officeApps] 
    , [className =? c --> doShiftWS 5 | c <- mediaApps]
    , [isFullscreen --> doFullFloat]
    ]

--------------------------------------------------------------------
-- [ My Workspaces ]

myWorkspaces :: [String]
myWorkspaces = ["1:<fn=1>\xe62b </fn>", "2:<fn=1>\xfa9e </fn>", "3:<fn=1>\xf992 </fn>", "4:<fn=1>\xf17a </fn>", "5:<fn=1>\xf724 </fn>", "6:<fn=1>\xf9c2 </fn>", "7:<fn=1>\xf1b7 </fn>", "8:<fn=1>\xf002 </fn>", "9:<fn=1>\xfb36 </fn>"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

clickable ws = "<action=xdotool key alt+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

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
                    h = 0.4
                    w = 0.4
                    t = 1 - h
                    l = 0.0

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
defSpacing :: l a -> ModifiedLayout Spacing l a
defSpacing = mySpacing 8            -- Default Spacing

-- Add Spacing to layouts
tiledSp = defSpacing tiled
bspSp = defSpacing bsp
threecolSp = defSpacing threecol
threecolMSp = defSpacing threecolmid

-- Rename layouts
tiled           = renamed [Replace "Tall"]  
                $ ResizableTall 1 (3/100) (1/2) []

threecol        = renamed [Replace "ThreeCol"] 
                $ ThreeCol 1 (3/100) (1/2)

threecolmid     = renamed [Replace "ThreeColMid"] 
                $ ThreeColMid 1 (3/100) (1/2)

bsp             = renamed [Replace "BSP"] 
                $ emptyBSP 

accordion       = renamed [Replace "Accordion"] 
                $ Accordion

avoidfloats     = renamed [Replace "AvoidFloats"] 
                $ avoidFloats Full

-- Toggle Layouts in "Pairs" (Very Useful)
tiledToggle = toggleLayouts tiledSp (tiled)
bspToggle = toggleLayouts bspSp (bsp)
threecolToggle = toggleLayouts threecolSp (threecol)
threecolToggle' = toggleLayouts threecolMSp (threecolmid)

-- Prompts & Configuration
myXPConfig :: XPConfig
myXPConfig = def 
        { font = myFont 
        , bgColor = "#1a1c21"
        , fgColor = "#bbc2cf"
        , bgHLight = "#c792ea"
        , fgHLight = "#000000"
        , borderColor = "#535974"
        , position = Top
        , height = 23
        , promptBorderWidth = 1
        , defaultText = ""
        , historySize = 256
        , historyFilter = id
        , searchPredicate = fuzzyMatch
        -- , sorter = fuzzySort
        --, autoComplete = Just 1000000
        }

-- [Grid Select Config]
myColourizer :: Window -> Bool -> X (String, String)
myColourizer = colorRangeFromClassName
                   (0x28,0x2c,0x34) -- lowest inactive bg
                   (0x28,0x2c,0x34) -- highest inactive bg
                   (0xc7,0x92,0xea) -- active bg
                   (0xc0,0xa7,0x9a) -- inactive fg
                   (0x28,0x2c,0x34) -- active fg

myGridConfig :: p -> GSConfig Window
myGridConfig colorizer = (buildDefaultGSConfig myColourizer)
        { gs_cellheight = 45
        , gs_cellwidth = 200
        , gs_cellpadding = 6
        , gs_originFractX = 0.5
        , gs_originFractY = 0.5
        , gs_font = myFont }

-- For shortcuts (GridSelect)
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight = 45
                   , gs_cellwidth = 200
                   , gs_cellpadding = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font = myFont }

-- App Grid for GridSelect
myAppGrid = [("LibreWolf", "librewolf")
            , ("Brave Browser", "brave")
            , ("Doom Emacs", "emacs")
            , ("Nvidia Settings [pkexec]", "pkexec sudo nvidia-settings")
            , ("QBittorrent", "qbittorrent")
            , ("Email (Thunderbird)", "thunderbird")
            , ("OBS-Studio", "obs")
            , ("VLC Media Player", "vlc")
            , ("Terminal (Alacritty)", "alacritty")
            , ("VMware", "vmware")
            , ("Virtualbox", "virtualbox")
            , ("LightDM GTK Greeter Settings", "lightdm-gtk-greeter-settings-pkexec")
            , ("LXAppearance", "lxappearance")
            , ("MOCP (Music-On-Console)", "alacritty --class=Alacritty,music -e mocp --theme=dylanwh")
            , ("Netflix", ((myBrowser') ++ " https://netflix.com"))
            --, ("LightDM GTK Greeter Settings", "lightdm-gtk-greeter-settings-pkexec")
            , ("OpenRGB", "openrgb")
            , ("Ckb-Next", "ckb-next")
            --, ("QBittorrent", "qbittorrent")
            , ("Handbrake", "handbrake")
            , ("Steam", "steam")
            , ("Powder Toy (TPT)", "powder-toy")
            , ("Maltego (OSINT)", "maltego")
            , ("Bluetooth Manager (Blueman)", "blueman-manager")
            , ("Libreoffice", "libreoffice")
            , ("K3B (KDE Disk Application)", "k3b")
            , ("Audacity", "audacity")
            , ("My Dotfiles", ((myBrowser) ++ " https://github.com/newyusi01/dotfiles"))
            , ("MoviesJoy", ((myBrowser') ++ " https://moviesjoy.to"))
            ]

-- [TreeSelect Config]
-- Define Some Functions
nodehead a b = Node (TS.TSNode a b (return ()))
nodesub a b c d = Node (TS.TSNode a b c) d

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
    [ nodehead "+ Accessories" "Accessory Applications"
        [ nodesub "Gnome Disks" "Tool for managing disks" (spawn "gnome-disks") []
        , nodesub "Tclip" "Clipboard Manager" (spawn "tclip") [] 
        , nodesub "ClipMan" "Dmenu Clipboard Manager" (spawn "xdotool key alt+c") [] 
        , nodesub "Variety" "Wallpaper switcher" (spawn "variety") []
        ]
    , nodehead "+ OSINT" "Open-Source Intelligence Applications"
        [ nodesub "Maltego" "Maltego is an open source intelligence (OSINT) and graphical link analysis tool for any investigative task" (spawn "maltego") [] 
        , nodesub "Sherlock" "Hunt down social media accounts by username across social networks" (spawn (myTerminal))  []
        ]
    , nodehead "+ Gaming" "Gaming Applications"
        [ nodesub "Steam" "Gaming library" (spawn "steam") []
        , nodesub "Lutris" "External Gaming Library" (spawn "lutris") []
        , nodesub "Powder-Toy" "Classic falling sand sandbox" (spawn "powder-toy") []
        , nodesub "Minecraft (Launcher)" "Open world block game" (spawn "xdotool key alt+7; minecraft-launcher") []
        ]
    , nodehead "+ Development" "Programming / Chill Zone"
        [ nodesub "Visual Studio Code" "Microsoft's coding IDE" (spawn "code") [] 
        , nodesub "Doom Emacs" "Emacs DOOM configuration" (spawn "emacs") [] 
        ]
    , nodehead "+ Internet" "Internet Applications"
        [ nodehead "+ Torrenting" "Torrenting Applications"
            [ nodesub "QBittorrent" "C++ Bittorrent client" (spawn "qbittorrent") [] ]
        , nodesub "LibreWolf" "Privacy focused web browser, based on Firefox and GNU Icecat" (spawn "librewolf") []
        , nodesub "Brave Browser" "Chromium based web browser" (spawn "brave") []
        , nodesub "Amfora" "Gemini web protocol based web browser" (spawn ((myTerminal) ++ " -e amfora")) [] 
        , nodehead "+ Bookmarks" "Web bookmarks & useful websites"
            [ nodesub "My Dotfiles" "View my dotfiles on Github" (spawn ((myBrowser) ++ " https://github.com/newyusi01/dotfiles")) []
            , nodesub "Hackage XMonad-Contrib" "XMonad-Contrib documentation" (spawn ((myBrowser) ++ " https://hackage.haskell.org/package/xmonad%2Dcontrib")) []
            , nodesub "Netflix" "Web content streaming" (spawn ((myBrowser') ++ " https://netflix.com")) []
            , nodesub "MoviesJoy" "Free movies & tv shows streaming" (spawn ((myBrowser') ++ " https://moviesjoy.to")) []
            ]
        ]
    , nodehead "+ Virtualization" "Virtualization Tools"
        [ nodesub "VMware" "VMware virtualization software" (spawn "vmware") [] 
        , nodesub "Virtualbox" "Virtualbox virtualization software" (spawn "virtualbox") [] 
        ]
    , nodesub "--------------------" "" (spawn "xdotools key Escape") [] 
    , nodehead "+ XMonad Tools" "XMonad WM Commands" 
        [ nodehead "+ Switch Workspaces" "Workspace switching commands"
            [ nodesub "Move to WS 1" "" (spawn ((myServer) ++ " 1")) []
            , nodesub "Move to WS 2" "" (spawn ((myServer) ++ " 3")) []
            , nodesub "Move to WS 3" "" (spawn ((myServer) ++ " 5")) []
            , nodesub "Move to WS 4" "" (spawn ((myServer) ++ " 7")) []
            , nodesub "Move to WS 5" "" (spawn ((myServer) ++ " 9")) []
            , nodesub "Move to WS 6" "" (spawn ((myServer) ++ " 11")) []
            , nodesub "Move to WS 7" "" (spawn ((myServer) ++ " 13")) []
            , nodesub "Move to WS 8" "" (spawn ((myServer) ++ " 15")) []
            , nodesub "Move to WS 9" "" (spawn ((myServer) ++ " 17")) []
            ]
        , nodehead "+ Shift Window To Workspace" "Shift selected window to workspace"
            [ nodesub "Shift to WS 1" "" (spawn ((myServer) ++ " 2")) []
            , nodesub "Shift to WS 2" "" (spawn ((myServer) ++ " 4")) []
            , nodesub "Shift to WS 3" "" (spawn ((myServer) ++ " 6")) []
            , nodesub "Shift to WS 4" "" (spawn ((myServer) ++ " 8")) []
            , nodesub "Shift to WS 5" "" (spawn ((myServer) ++ " 10")) []
            , nodesub "Shift to WS 6" "" (spawn ((myServer) ++ " 12")) []
            , nodesub "Shift to WS 7" "" (spawn ((myServer) ++ " 14")) []
            , nodesub "Shift to WS 8" "" (spawn ((myServer) ++ " 16")) []
            , nodesub "Shift to WS 9" "" (spawn ((myServer) ++ " 18")) [] 
            ]
        , nodesub "Next Layout" "Switch to next layout" (spawn ((myServer) ++ " next-layout")) []
        , nodesub "Recompile" "Recompile XMonad" (spawn "xmonad --recompile") []
        , nodesub "Restart" "Restart XMonad" (spawn "xmonad --restart; notify-send XMonad Restarted..") []
        ]
    , nodehead "+ Power Menu" "Power Commands"
        [ nodesub "Shutdown" "Power the system off" (spawn "shutdown now") []
        , nodesub "Restart" "Restart the system" (spawn "shutdown -r now") []
        , nodesub "Logout" "Logout of the system" (spawn "killall xmonad") []
        ]
    ]

-- TreeSelect (TS) Config For UI
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                             , TS.ts_background = 0xdd282c34
                             , TS.ts_font = myFont
                             , TS.ts_node = (0xffd0d0d0, 0xff1c1f24)
                             , TS.ts_nodealt = (0xffd0d0d0, 0xff282c34)
                             , TS.ts_highlight = (0xffffffff, 0xff755999)
                             , TS.ts_extra = 0xffd0d0d0
                             , TS.ts_node_width = 200
                             , TS.ts_node_height = 20
                             , TS.ts_originX = 100
                             , TS.ts_originY = 100
                             , TS.ts_indent = 40
                             , TS.ts_navigate = myTreeNavigation
                             }

-- TreeSelect Navigation Keybinds
myTreeNavigation = M.fromList
    [ ((0, xK_Escape), TS.cancel)
    , ((0, xK_Return), TS.select)
    , ((0, xK_Up), TS.movePrev)
    , ((0, xK_Down), TS.moveNext)
    , ((0, xK_Left), TS.moveParent)
    , ((0, xK_Right), TS.moveChild)
    ]

-- [Tabs Config]
--myTabTheme = def { 
            --fontName            = myFont
            --, activeColor         = "#46d9ff"
            --, inactiveColor       = "#313846"
            --, activeBorderColor   = "#46d9ff"
            --, inactiveBorderColor = "#282c34"
            --, activeTextColor     = "#282c34"
            --, inactiveTextColor   = "#d0d0d0"
            --} 


-------------------------------------------

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--------------------------------------------------------------------

myLayoutHook = windowNavigation $ mkToggle (NBFULL ?? EOT) $ avoidStruts $ smartBorders ( 
                tiledToggle ||| bspToggle ||| threecolToggle' ||| avoidfloats  
                ||| threecolToggle ||| accordion
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
            --spawnOnce "openrgb --startminimized & disown; openrgb -p new.orp & disown"
            --spawnOnce "protonvpn-applet & disown"

-------------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar -x 0 /home/yusef/.config/xmobar/.xmobarrc" -- top scr1
    xmprocbtm <- spawnPipe "xmobar -x 0 /home/yusef/.config/xmobar/.xmobarrc3" -- btm scr1
    xmproc1 <- spawnPipe "xmobar -x 1 /home/yusef/.config/xmobar/.xmobarrc2" -- top scr2
    xmproc2 <- spawnPipe "xmobar -x 1 /home/yusef/.config/xmobar/.xmobarrc4scr2" --btm scr2
    xmonad $ ewmh $ docks def
      {
          borderWidth         = 2
          , terminal          = myTerminal
          , startupHook = myStartupHook
          , workspaces  = myWorkspaces
          , manageHook  = myManageHook <+> namedScratchpadManageHook myScratchPads <+> workspaceByPos
          , layoutHook        = myLayoutHook
          , logHook           = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP {
                                ppOutput = \x -> hPutStrLn xmproc x  >> hPutStrLn xmproc1 x
                                --ppOutput = hPutStrLn xmproc
                              , ppCurrent = xmobarColor "#98be65" "" . wrap "[+] " "" -- Current workspace in xmobar
                              , ppVisible = xmobarColor "#98be65" "" -- . clickable             -- Visible but not current workspace
                              , ppHidden = xmobarColor "#98be65" "" . wrap "* " "" . noScratchPad -- . clickable -- Hidden workspaces in xmobar
                              , ppHiddenNoWindows = xmobarColor "#c792ea" "" . noScratchPad -- . clickable       -- Hidden workspaces (no windows)
                              , ppTitle = xmobarColor "#b3afc2" "" . shorten 40    -- Title of active window in xmobar
                              , ppSep =  "<fc=#666666><fn=1> | </fn></fc>"          -- Separators in xmobar
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
          , handleEventHook    = handleEventHook def 
                                 <+> fullscreenEventHook 
                                 <+> serverModeEventHook
                                 <+> serverModeEventHookCmd
                                 <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
          -- , modMask = mod1Mask
      }
          `additionalKeysP`
           [
            --((controlMask, xK_F1), spawn "pcmanfm")       -- spawn app (CTRL F1)
            ("C-<F2>", spawn "librewolf")    -- spawn app (CTRL F2)
            --, ((controlMask, xK_F3), spawn "epdfview")  -- spawn app (CTRL F3)
            , ("C-<F3>", spawn "brave")  -- spawn app (CTRL F3)
            --, ((mod1Mask, xK_r), spawn "alacritty -e ~/spawnjailedapps.sh")
            -- [Recompile XMonad Properly and restart xmobar(s)
            , ("M1-q", spawn "killall xmobar && killall xmobar; xmonad --recompile && xmonad --restart")
            , ("M4-r", spawn "jgmenu_run")
            , ("M1-c", spawn "/home/yusef/launchdmenu.sh")
            -- [Turn off pc using script]
            , ("M1-S-q", spawn "~/Documents/powermenu.sh")
            -- [Music Player (MOCP)]
            , ("M1-C-m", spawn "alacritty --class Alacritty,music -e mocp --theme=dylanwh -A")
            -- [Toggle AvoidFloats]
            , ("M1-S-<KP_Equal>", sendMessage AvoidFloatToggle)
            , ("M1-C-<KP_Equal>", withFocused $ sendMessage . AvoidFloatToggleItem)
            , ("M1-S-C-<KP_Equal>", sendMessage (AvoidFloatSet False) >> sendMessage AvoidFloatClearItems)
            ------
            --, ((controlMask .|. mod4Mask, xK_F3), spawn "~/./spawnjailedbravebrowser.sh")
            --, ((controlMask .|. mod4Mask, xK_F2), spawn "~/./spawnjailedlibrewolf.sh")
            
            -- [Prompts]
            , ("M1-p r", shellPrompt myXPConfig)
            , ("M1-p m", manPrompt myXPConfig)
            , ("M1-p n", do
                         spawn ("date>>"++"/home/yusef/Documents/tmpnotes.txt")
                         appendFilePrompt myXPConfig "/home/yusef/Documents/tmpnotes.txt"
                         )
            , ("M1-p b", promptSearchBrowser myXPConfig myBrowser myDDG)
            , ("M1-p h", promptSearchBrowser myXPConfig myBrowser myHak) 
            , ("M1-p c", selectSearchBrowser myBrowser myDDG)
            , ("M1-p s", sshPrompt myXPConfig)
            -- -[GridSelect (Using Actions)]
            , ("M1-g", spawnSelected' myAppGrid)
            , ("M1-w", goToSelected $ myGridConfig myColourizer)
            , ("M1-b", bringSelected $ myGridConfig myColourizer)
            -- -[TreeSelect (Using Actions)]
            , ("M1-t", treeselectAction tsDefaultConfig)
            -----
            , ("M1-S-b", spawn "blueman-manager & disown")
            , ("C-M1-b", spawn "bitwarden")
            , ("C-<F4>", spawn "emacs")        -- spawn app (CTRL F4)
            --, ((mod1Mask .|. controlMask, xK_b), spawn "icecat") -- spawn browser (C-M-b)
            -- [Scratchpads]
            , ("M1-s t", scratchTerm)
            , ("M1-s v", scratchMixer)
            , ("M1-s m", scratchDnsMon)
            , ("M1-s e", scratchNotes)
            , ("M1-s n", scratchEmacs)
            , ("M1-s f", scratchfileMan)
            , ("M1-s q", scratchqB)
            ----
            --, ((mod1Mask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave) -- Cycle workspaces (ALT TAB)
            , ("M1-M4-<Return>", promote)                          -- Promote selected window to master pane
            , ("M1-C-<R>", nextWS)           -- shift to next WS (ALT UP-ARROW)
            , ("M1-C-<L>", prevWS)            -- shift to previous WS (ALT DOWN-ARROW)
            --, ((mod1Mask .|. controlMask, xK_Left), DO.swapWith Prev NonEmptyWS)
            --, ((mod1Mask .|. controlMask, xK_Right), DO.swapWith Next NonEmptyWS)
            , ("M1-C-S-n",  shiftToNext)
            , ("M1-C-S-p",  shiftToPrev)
            --------------------------------------------------
            -- Manage Windows Easily Using Arrowkeys
            , ("M1-<R>", sendMessage $ Go R)
            , ("M1-<L>", sendMessage $ Go L)
            , ("M1-<U>", sendMessage $ Go U)
            , ("M1-<D>", sendMessage $ Go D)
            , ("M1-S-<R>", sendMessage $ Swap R)
            , ("M1-S-<L>", sendMessage $ Swap L)
            , ("M1-S-<U>", sendMessage $ Swap U)
            , ("M1-S-<D>", sendMessage $ Swap D)
            -- [Specific window manipulations keys for BSP layout]
            , ("M1-M4-S-<U>", sendMessage $ ShrinkFrom U)
            , ("M1-M4-S-<D>", sendMessage $ ShrinkFrom D)
            , ("M1-M4-S-<L>", sendMessage $ ShrinkFrom L)
            , ("M1-M4-S-<R>", sendMessage $ ShrinkFrom R)
            , ("M1-M4-C-<U>", sendMessage $ ExpandTowards U)
            , ("M1-M4-C-<D>", sendMessage $ ExpandTowards D)
            , ("M1-M4-C-<L>", sendMessage $ ExpandTowards L)
            , ("M1-M4-C-<R>", sendMessage $ ExpandTowards R)
            , ("M1-r", sendMessage Rotate)
            , ("M1-M4-a", sendMessage Balance)
            , ("M1-M4-S-a", sendMessage Equalize)
            -- Window Resizing
            , ("M1-C-S-<U>", sendMessage MirrorExpand)
            , ("M1-C-S-<D>", sendMessage MirrorShrink)
            , ("M1-C-S-<L>", sendMessage Shrink)
            , ("M1-C-S-<R>", sendMessage Expand)
            -------------------------------------------------
            -- Switch focus to different screens easily
            -- Default: ALT (W,E,R)
            -- Where :
            -- [+] W is screen 1 
            -- [+] E is screen 2 
            -- [+] R is screen 3
            -- But now, no matter how much screens I have, this will make sense as a keybind
            
            , ("M1-M4-<R>", nextScreen)
            , ("M1-M4-<L>", prevScreen)

            -- [ Shift Windows to Other Screens Easily ]
            , ("M1-M4-<U>", shiftNextScreen)
            , ("M1-M4-<D>", shiftPrevScreen)

            --------------------------------------------------
            -- Toggle Modes
            , ("M1-<Return>", sendMessage (MT.Toggle NBFULL))
            --, ((mod1Mask, xK_f), sendMessage (Toggle "realFull"))
            , ("M1-M4-b", sendMessage ToggleStruts)  -- Toggle struts aka XMobar using a keybinding (ALT + F)

            --------------------------------------------------
            -- Seperate Workspace shortcuts (2nd monitor)
            --, ((mod1Mask, xK_k), windows $ onCurrentScreen f i)
            --------------------------------------------------


            , ("M1-<F7>", spawn "/usr/bin/pamixer -d 2") -- decrease volume by n
            , ("M1-<F8>", spawn "/usr/bin/pamixer -i 2") -- increase volume by n
            , ("M1-<F5>", spawn "/usr/bin/pamixer -t") -- togglemute

            
            , ("M1-C-i", incWindowSpacing 4)    -- Increase Window Spacing on the Fly
            , ("M1-C-d", decWindowSpacing 4)    -- Decrease Window Spacing on the Fly
            
            , ("M1-<Print>", spawn "flameshot gui") -- screenshot
            
            , ("M1-<F12>", spawn "killall picom; picom -b & disown") -- Restart Compositor

            , ("M1-C-k", spawn "mousepad ~/Documents/keybinds.txt")  -- Show keybinds
            
            , ("M1-<F11>", spawn "killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --transparent true --alpha 0 --tint 0x000000 --widthtype request --monitor 1 --height 24 & disown") -- Restart Trayer
            
            --, ((mod1Mask .|. controlMask, xK_m), spawn "mailspring") -- spawn mail client

            , ("M1-C-<Space>", sendMessage ToggleLayout) -- Toggle Layouts, specified in LayoutHook

            --, ((mod1Mask, xK_f), moveTo Next EmptyWS)                   -- find a free workspace (ALT F)
            --, ((mod1Mask .|. controlMask, xK_f), moveTo Next NonEmptyWS)  -- (ALT + SHIFT F) cycle between non-empty workspaces (application opened in them)
            --, ((modm .|. shiftMask, xK_Up),    shiftToPrev)
            --, ((modm,               xK_Right), nextScreen)
            --, ((modm,               xK_Left),  prevScreen)
            --, ((modm .|. shiftMask, xK_Right), shiftNextScreen)
            --, ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
            , ("M1-C-z", toggleWS)                -- (ALT + Z) cycle between workspaces that are being used
            --, ((mod1Mask, xK_F7, lowerVolume 3 >> return ()))

            --, ((mod1Mask, xK_Return), sendMessage ToggleLayout) -- Toggle Layouts (VERY HANDY)
            --, ((mod1Mask .|. controlMask, xK_f), sendMessage (Toggle "nBFull"))
            --, ((mod1Mask .|. shiftMask, xK_f), myLayout)
            --, ((mod1Mask .|. controlMask, xK_Right),                  -- a crazy keybinding!
                  --do t <- findWorkspace getSortByXineramaRule Next NonEmptyWS 2
                    --windows . view $ t )
           ]
                
