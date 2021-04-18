-- My XMonad Configuration File
-- Written By Yusef Aslam
-- Based on John Goerzens Configuration
-- Default Keys https://gist.github.com/c33k/1ecde9be24959f1c738d
-- This config may be messy, but I am a beginner so please don't mind that.

-- [Imports] --

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
import XMonad.Util.EZConfig(additionalKeysP)--,additionalMouseBindings)
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
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Ssh
import XMonad.Prompt.DirExec

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
import XMonad.Layout.Maximize
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
--import XMonad.Layout.Maximize

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
--import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.FloatKeys -- tmp solution to https://github.com/xmonad/xmonad/issues/290

import System.IO

---------------------------------------------------------------------
-- App defaults names

myTerminal :: String
myTerminal = "alacritty"

myFallBackTerminal :: String
myFallBackTerminal = "xterm"

-- Setting more function names / string names
myFont :: String
myFont = "xft:Terminus:pixelsize=11"

myBrowser :: String
myBrowser = "/usr/bin/librewolf"

myBrowser' :: String
myBrowser' = "/usr/bin/brave"

-- Custom Search Engines
myArchWiki = searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="

myDDG = intelligent duckduckgo
--myHak = intelligent hackage
--myAWIntell = intelligent myArchWiki

myServer :: String
myServer = "/home/yusef/.xmonad/xmonadctl"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

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
officeApps = ["Xarchiver","Soffice","Epdfview","llpp","libreoffice","LibreOffice","libreoffice-impress","feh"]

webApps = ["firefox","IceCat","Chromium","LibreWolf","Brave-browser","qutebrowser"]
systemApps = ["qnvsm","Gnome-disks","Nvidia-settings","ckb-next","openrgb"]
virtApps = ["Vmware","VirtualBox","Virt-manager"]

generalApps = ["firetools","qBittorrent","calibre","Pcmanfm","Mailspring","KeePassXC","Mousepad"]
devApps = ["Code","Godot"]
osintApps = ["Maltego"]

socialApps = ["Microsoft Teams","discord"]
otherApps = ["Progress","Xmessage","XClock","Zenity"]
floatApps = ["Dialog","Picture-in-Picture","Confirm","Error"]
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

doWsShiftNoView :: Int -> ManageHook
doWsShiftNoView a = doShift ( myWorkspaces !! a )

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
    [
      [className =? c --> doShiftWS 6 | c <- gameApps]
    , [className =? head customClasses   --> doShiftWS 8]
    , [className =? c --> doShiftWS 2 | c <- systemApps]
    , [className =? c --> doShiftWS 0 | c <- devApps]
    , [resource  =? r --> doFloat     | r <- floatApps]
    , [className =? c --> doFloat     | c <- otherApps]
    , [className =? c --> doShiftWS 3 | c <- virtApps]
    , [className =? head generalApps     --> doFloat]
    , [className =? c --> doFloat     | c <- take 2 officeApps] 
    , [className =? c --> doShiftWS 5 | c <- mediaApps]
    , [title     =? t --> doFloat     | t <- drop 1 floatApps]
    , [className =? last officeApps      --> doFloat]
    , [isFullscreen   --> doFullFloat]
    ]

--------------------------------------------------------------------
-- [ My Workspaces ]

myWorkspaces :: [String]
myWorkspaces = ["1:<fn=1>\xe62b </fn>", "2:<fn=1>\xfa9e </fn>", 
                "3:<fn=1>\xf992 </fn>", "4:<fn=1>\xf17a </fn>",
                "5:<fn=1>\xf724 </fn>", "6:<fn=1>\xf9c2 </fn>", 
                "7:<fn=1>\xf1b7 </fn>", "8:<fn=1>\xf002 </fn>",
                "9:<fn=1>\xfb36 </fn>"]

--myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

--clickable ws = "<action=xdotool key alt+"++show i++">"++ws++"</action>"
    --where i = fromJust $ M.lookup ws myWorkspaceIndices

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
                , NS "discord" spawnDiscord findDiscord manageDiscord
                , NS "bitwarden" spawnBitW findBitW manageBitW
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

        -- [PcManFM File Manager]
            spawnfileMan = "pcmanfm"
            findfileMan = className =? "Pcmanfm"
            managefileMan = customFloating $ W.RationalRect l t w h
                where
                    h = 0.9
                    w = 0.9
                    t = 0.95 - h
                    l = 0.95 - w

        -- [Discord]
            spawnDiscord = "~/Documents/jailedappscripts/spawnjaileddiscord.sh"
            findDiscord = className =? "discord"
            manageDiscord = customFloating $ W.RationalRect l t w h
                where
                    h = 0.9
                    w = 0.9
                    t = 0.95 - h
                    l = 0.95 - w

        -- [Bitwarden]
            spawnBitW = "~/Documents/jailedappscripts/spawnjailedbitwarden.sh"
            findBitW = className =? "Bitwarden"
            manageBitW = customFloating $ W.RationalRect l t w h
                where
                    h = 0.95
                    w = 0.45
                    t = 0.025
                    l = 0.9989 - w

-- Hide scratchpad workspace
noScratchPad ws = if ws == "NSP" then "" else ws

scratchTerm = namedScratchpadAction myScratchPads "terminal"
scratchMixer = namedScratchpadAction myScratchPads "volumectl"
scratchDnsMon = namedScratchpadAction myScratchPads "dnsmon"
scratchNotes = namedScratchpadAction myScratchPads "notes"
scratchEmacs = namedScratchpadAction myScratchPads "emacs"
scratchqB = namedScratchpadAction myScratchPads "torrent"
scratchfileMan = namedScratchpadAction myScratchPads "fileman"
scratchDiscord = namedScratchpadAction myScratchPads "discord"
scratchBitW = namedScratchpadAction myScratchPads "bitwarden"

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
                $ maximize
                $ ResizableTall 1 (3/100) (1/2) []

threecol        = renamed [Replace "ThreeCol"]
                $ maximize
                $ ThreeCol 1 (3/100) (1/2)

threecolmid     = renamed [Replace "ThreeColMid"]
                $ maximize
                $ ThreeColMid 1 (3/100) (1/2)

bsp             = renamed [Replace "BSP"]
                $ maximize
                $ emptyBSP 

accordion       = renamed [Replace "Accordion"]
                $ maximize
                $ Accordion

avoidfloats     = renamed [Replace "AvoidFloats"]
                $ maximize
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
            , ("My Dotfiles", ((myBrowser) ++ " https://github.com/yusi1/my_dotfiles"))
            , ("MoviesJoy", ((myBrowser') ++ " https://moviesjoy.to"))
            , ("Prayer Times (Aberdeen Mosque)", ((myBrowser) ++ " https://www.aberdeenmosque.org/"))
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
        , nodesub "Sherlock" "Hunt down social media accounts by username across social networks" (spawn ((myTerminal) ++ " --hold -e sherlock -h"))  []
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
            [ nodesub "My Dotfiles" "View my dotfiles on Github" (spawn ((myBrowser) ++ " https://github.com/yusi1/my_dotfiles")) []
            , nodesub "Hackage XMonad-Contrib" "XMonad-Contrib documentation" (spawn ((myBrowser) ++ " https://hackage.haskell.org/package/xmonad%2Dcontrib")) []
            , nodesub "Netflix" "Web content streaming" (spawn ((myBrowser') ++ " https://netflix.com")) []
            , nodesub "MoviesJoy" "Free movies & tv shows streaming" (spawn ((myBrowser') ++ " https://moviesjoy.to")) []
            , nodesub "Prayer Times" "Aberdeen Mosque prayer times" (spawn ((myBrowser) ++ " https://www.aberdeenmosque.org/")) []
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
        , nodesub "Logout" "Logout of the system" (spawn "killall $HOME/.xmonad/xmonad-x86_64-linux") []
        ]
    ]

-- TreeSelect (TS) Config For UI
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { 
                TS.ts_hidechildren = True
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

-- Show the window count of the current workspace in XMobar
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--------------------------------------------------------------------

-- Defining the LayoutHook
myLayoutHook = windowNavigation 
               $ mkToggle (NBFULL ?? EOT) 
               $ avoidStruts
               $ smartBorders
               ( tiledToggle ||| bspToggle ||| threecolToggle' ||| avoidfloats  
               ||| threecolToggle ||| accordion )

--------------------------------------------------------------------
-- [ Fade Inactive Windows ]
-- Currently set to one, but is here in case I want to change it;
-- In the future.

-- Fade inactive windows to the amount of (fadeAmount)
myLogHook :: X ()
myLogHook = return () 
    --fadeInactiveLogHook fadeAmount
    --where fadeAmount = 1

-------------------------------------------------------------------
-- Startup Hook (check .xprofile too)

myStartupHook :: X ()
myStartupHook = do
            -- Put apps you want XMonad to start in here
            -- example:
            -- spawnOnce "ckb-next & disown"
            setWMName "LG3D"    -- For java application support
            spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --transparent true --alpha 0 --tint 0x1A1C21 --widthtype request --monitor 0 --height 24 & disown"
            --spawnOnce "ckb-next -b & disown"
            --spawnOnce "openrgb --startminimized & disown; openrgb -p new.orp & disown"
            --spawnOnce "protonvpn-applet & disown"
            --spawnOnce "firetools --minimize & disown"

-------------------------------------------------------------------
-- Where the config comes together

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar -x 0 /home/yusef/.config/xmobar/.xmobarrc" -- top scr1
    xmprocbtm <- spawnPipe "xmobar -x 0 /home/yusef/.config/xmobar/.xmobarrc3" -- btm scr1
    --xmproc1 <- spawnPipe "xmobar -x 1 /home/yusef/.config/xmobar/.xmobarrc2" -- top scr2
    --xmproc2 <- spawnPipe "xmobar -x 1 /home/yusef/.config/xmobar/.xmobarrc4scr2" --btm scr2
    xmonad $ ewmh $ docks def
      {
          borderWidth         = 2
          , terminal          = myTerminal
          , startupHook       = myStartupHook
          , workspaces        = myWorkspaces
          , focusFollowsMouse = myFocusFollowsMouse
          , manageHook        = myManageHook <+> namedScratchpadManageHook myScratchPads -- <+> workspaceByPos
          , layoutHook        = myLayoutHook
          , logHook           = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP {
                                --ppOutput = \x -> hPutStrLn xmproc x  >> hPutStrLn xmproc1 x
                                ppOutput = hPutStrLn xmproc
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
                              -- >> updatePointer (0, 0) (0.95, 0.95) 
          --, focusedBorderColor = "#2aa198"
          , focusedBorderColor = "#46d9ff"
          , normalBorderColor = "#282c34"           
          , handleEventHook    = handleEventHook def 
                                 <+> fullscreenEventHook 
                                 <+> serverModeEventHook
                                 -- <+> serverModeEventHookCmd
                                 -- <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
          -- , modMask = mod1Mask
      } `additionalKeysP` [
            -- [Spawn Applications]
            ("C-<F2>", spawn "librewolf")
            , ("C-<F3>", spawn "brave")
            , ("M1-S-b", spawn "blueman-manager & disown")
            --, ("C-M1-b", spawn "bitwarden")
            , ("C-<F4>", spawn "emacs")
            , ("M4-y", spawn "scrot Pictures/scrot_%H_%M_%S.png") -- screenshot
            , ("M4-u", spawn "scrot -u Pictures/scrot_%H_%M_%S.png") -- screenshot focused
            , ("M4-i", spawn "scrot -f -s Pictures/scrot_%H_%M_%S.png") -- screenshot select
            -- [*] [Kill The Compositor]
            , ("M1-<F12>", spawn "killall picom; picom -b & disown")

            -- [*] [Show Keybinds (Heavily WIP)]
            , ("M1-C-k", spawn "mousepad ~/Documents/keybinds.txt")

            -- [*] [Change Volume]
            , ("M1-<F7>", spawn "/usr/bin/pamixer -d 2")
            , ("M1-<F8>", spawn "/usr/bin/pamixer -i 2")
            , ("M1-<F5>", spawn "/usr/bin/pamixer -t")
            ---------------------------------------
            -- [Recompile XMonad Properly and restart running XMobar Instances]
            , ("M1-q", spawn "killall xmobar && killall xmobar; xmonad --recompile && xmonad --restart")
            , ("M4-r", spawn "jgmenu_run")
            , ("M1-c", spawn "/home/yusef/launchdmenu.sh")
            ---------------------------------------
            -- [Turn off pc using script]
            , ("M1-S-q", spawn "~/Documents/powermenu.sh")
            ---------------------------------------
            -- [Music Player (MOCP)]
            , ("M1-C-m", spawn "alacritty --class Alacritty,music -e mocp --theme=dylanwh -A")
            ---------------------------------------
            -- [Toggle AvoidFloats]
            , ("M1-S-<KP_Equal>", sendMessage AvoidFloatToggle)
            , ("M1-C-<KP_Equal>", withFocused $ sendMessage . AvoidFloatToggleItem)
            , ("M1-S-C-<KP_Equal>", sendMessage (AvoidFloatSet False) >> sendMessage AvoidFloatClearItems)
            ---------------------------------------
            -- [Prompts]
            , ("M1-p r", shellPrompt myXPConfig)
            , ("M1-p m", manPrompt myXPConfig)
            , ("M1-p n", do spawn ("date>>"++"/home/yusef/Documents/tmpnotes.txt")
                            appendFilePrompt myXPConfig "/home/yusef/Documents/tmpnotes.txt")
            , ("M1-p b", promptSearchBrowser myXPConfig myBrowser myDDG)
            , ("M1-p h", promptSearchBrowser myXPConfig myBrowser hackage)
            , ("M1-p a", promptSearchBrowser myXPConfig myBrowser myArchWiki)
            , ("M1-p c", selectSearchBrowser myBrowser myDDG)
            , ("M1-p s", sshPrompt myXPConfig)
            , ("M1-p x", dirExecPromptNamed myXPConfig spawn "/home/yusef/Documents/jailedappscripts" "Spawn Jailed: ")
            -- [*] [ShellPrompt extra prompts]
            , ("M1-p e", prompt ("alacritty" ++ " --hold -e") myXPConfig)
            , ("M1-p o", prompt ("llpp" ++ " ") myXPConfig)
            , ("M1-p l", prompt ("alacritty" ++ " -e epr ") myXPConfig) 
            , ("M1-p v", prompt ("mpv" ++ " ") myXPConfig)
            ---------------------------------------
            -- [GridSelect (Using Actions)]
            , ("M4-g", spawnSelected' myAppGrid)
            , ("M4-w", goToSelected $ myGridConfig myColourizer)
            , ("M4-b", bringSelected $ myGridConfig myColourizer)
            ---------------------------------------
            -- [TreeSelect (Using Actions)]
            , ("M4-t", treeselectAction tsDefaultConfig)
            ---------------------------------------
            -- [Scratchpads]
            , ("M1-M4-t", scratchTerm)
            , ("M1-M4-v", scratchMixer)
            , ("M1-M4-m", scratchDnsMon)
            , ("M1-M4-e", scratchNotes)
            , ("M1-M4-n", scratchEmacs)
            , ("M1-M4-f", scratchfileMan)
            , ("M1-M4-q", scratchqB)
            , ("M1-M4-d", scratchDiscord)
            , ("M1-M4-b", scratchBitW)
            ---------------------------------------
            -- [More Window Management]
            , ("M1-M4-<Return>", promote) -- Promote selected window to master pane
            , ("M1-C-<R>", nextWS) -- shift to next WS
            , ("M1-C-<L>", prevWS) -- shift to previous WS
            , ("M1-S-n",  shiftToNext)
            , ("M1-S-p",  shiftToPrev)
            -- [*] [Shift to a previously used workspace]
            , ("M1-C-z", toggleWS)

            -- [*] [ConstrainedResize (Resize windows while maintaining aspect ratio)]
            --("M1-S-<button3>", (\w -> focus w >> Sqr.mouseResizeWindow w True))
            ---------------------------------------
            -- [Manage Windows Easily Using Arrowkeys]
            , ("M1-<R>", sendMessage $ Go R)
            , ("M1-<L>", sendMessage $ Go L)
            , ("M1-<U>", sendMessage $ Go U)
            , ("M1-<D>", sendMessage $ Go D)
            , ("M1-S-<R>", sendMessage $ Swap R)
            , ("M1-S-<L>", sendMessage $ Swap L)
            , ("M1-S-<U>", sendMessage $ Swap U)
            , ("M1-S-<D>", sendMessage $ Swap D)
            -- [*] [Control Spacing Layout's Spacing]
            , ("M1-C-i", incWindowSpacing 4)
            , ("M1-C-d", decWindowSpacing 4)
            
            -- [*] [Toggle Spacing Layout]
            , ("M1-C-<Space>", sendMessage ToggleLayout)
            ---------------------------------------
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
            ---------------------------------------
            -- [Window Resizing]
            , ("M1-C-S-<U>", sendMessage MirrorExpand)
            , ("M1-C-S-<D>", sendMessage MirrorShrink)
            , ("M1-C-S-<L>", sendMessage Shrink)
            , ("M1-C-S-<R>", sendMessage Expand)

            -- [*] [Float Window Resizing (WIP)]
            -- Shrink Windows
            --, ("M1-M4-<R>", withFocused (keysResizeWindow (10,0) (0,0)))
            --, ("M1-M4-<L>", withFocused (keysResizeWindow (-10,0) (0,0)))
            --, ("M1-M4-<D>", withFocused (keysResizeWindow (0,-10) (1,1)))
            --, ("M1-M4-<U>", withFocused (keysResizeWindow (0,-10) (0,0)))
            -- Expand Windows
            --, ("M4-C-<R>", withFocused (keysResizeWindow (0,10) (1,1)))
            ---------------------------------------
            -- [Toggle Modes]
            , ("M1-<Return>", sendMessage (MT.Toggle NBFULL))
            , ("M1-f", withFocused (sendMessage . maximizeRestore))
            , ("M1-C-b", sendMessage ToggleStruts)
            ---------------------------------------
            ] 
            --`additionalMouseBindings` [
                --((0,9), \w -> withFocused (sendMessage . maximizeRestore)) 
            --]
