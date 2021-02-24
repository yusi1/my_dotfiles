-- My XMonad Configuration File
-- Written By Yusef Aslam
-- Copied stuff from many sources including: https://pastebin.com/BWrvV4SH
-- Also, https://www.reddit.com/r/xmonad/comments/hiysu2/cant_open_steam_games/
-- https://www.reddit.com/r/xmonad/comments/herxnc/steam_and_xmonad/
-- Based on John Goerzens Configuration
-- Bits of the file have been gained (different features) as I read documentation etc..
-- Remember READ documentation, it's very useful (or even find random questions on reddit/github/stackoverflow etc...)
-- Default Keys https://gist.github.com/c33k/1ecde9be24959f1c738d
-- My goal for my specific config is to keep it functional, but simple and well defined, keeping bloat to a minimum

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

--import Text.Printf

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops(fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.FadeInactive

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare
--import XMonad.Util.Loggers

import XMonad.Layout
--import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
--import XMonad.Layout.NoBorders(OnlyFloat)
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
--import XMonad.Layout.TwoPane (TwoPane(..))
--import XMonad.Layout.Tabbed (simpleTabbed)
-- import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
--import XMonad.Layout.Maximize
import XMonad.Layout.LayoutModifier

--import XMonad.Layout.MultiToggle
--import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ToggleLayouts

import XMonad.Layout.WindowNavigation
import XMonad.Layout.AvoidFloats

import XMonad.Actions.UpdatePointer -- update pointer location to edge of new focused window, to prevent unintended focus stealing
import XMonad.Actions.CycleRecentWS -- cycle recent workspaces with keys defined in myKeys
--import XMonad.Actions.Promote -- Promote selected window to master pane
--import XMonad.Actions.Search -- use search engine in XMonad
import XMonad.Actions.CycleWS -- Cycle Workspaces, for example using the arrow keys
--import XMonad.Actions.CycleWindows -- Cycle windows in current workspace
-- import XMonad.Actions.WindowNavigation -- Experimental rewrite of layout with same name, allows window navigation with arrow keys
--import XMonad.Actions.Volume

import System.IO

---------------------------------------------------------------------

-- [ Manipulate windows as they are created.
-- | The list is processed from top -> bottom, 
-- [ the first matching rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.

viewShift = doF . liftM2 (.) W.greedyView W.shift
doShiftWS a = doShift ( myWorkspaces !! a ) <+> viewShift ( myWorkspaces !! a )

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll         -- Add Custom Hook to make certain windows open in floating mode
    [
      -- For some reason the doShift ( variable !! WS ) function has offset workspaces by 1 (so the 2nd workspace would be the 1st)
      -- Added New Variable (doShiftWS (n)) to shorten these lines, before it had to be two different definitions (doShift) and (viewShift);
      -- Along with duplicate numbers. This was inefficient, as I had to write the same line with duplicate numbers each time, it was becoming a pain -
      -- to write all of those lines, which were the same thing anyway.
      className =? "Steam"    --> ( doShiftWS 6 )
      , className =? "Alacritty" --> ( doShiftWS 0 )
      , (className =? "Steam" <&&> resource =? "Dialog") --> doFloat
      , className =? "mpv" --> ( doShiftWS 5 )
      , className =? "vlc" --> ( doShiftWS 5 )
      , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
      , (className =? "IceCat" <&&> resource =? "Dialog") --> doFloat  -- Float IceCat Dialog
      , className =? "IceCat"     --> ( doShiftWS 1 )
      , className =? "LBRY"       --> ( doShiftWS 5 )
      , className =? "qnvsm"      --> ( doShiftWS 2 )
      , className =? "Vmware"     --> ( doShiftWS 3 )
      , className =? "powder-toy" --> ( doShiftWS 6 )
      , className =? "Chromium"   --> ( doShiftWS 1 )
      , className =? "Code"       --> ( doShiftWS 0 )
      , className =? "Epdfview"   --> ( doShiftWS 4 )
      , className =? "llpp"       --> ( doShiftWS 4 ) 
      , className =? "LibreWolf"  --> ( doShiftWS 1 )
      , className =? "Progress"   --> doFloat
      , className =? "Pcmanfm"    --> doFloat
      , className =? "pcmanfm"    --> doFloat
      , className =? "Mailspring" --> ( doShiftWS 1 )
      , className =? "Xmessage"   --> doFloat
      , className =? "ckb-next"   --> ( doShiftWS 10 )
      , className =? "obs"        --> ( doShiftWS 7 )
      , className =? "Maltego"    --> ( doShiftWS 8 )
      , className =? "Nvidia-settings"  --> ( doShiftWS 2 )
      , className =? "Lutris"     --> ( doShiftWS 6 )
      , isFullscreen --> doFullFloat
    ]

--------------------------------------------------------------------
-- [ My Workspaces ]

myWorkspaces = [" 1:dev ", " 2:www ", " 3:sys ", " 4:virt ", " 5:doc ", " 6:media ", " 7:game ", " 8:rec ", " 9:osint ", " 10:osint1 ", " 11:misc "]
-- Offset:      "   0   ", "   1   ", "   2   ", "    3   ", "    4    ", "   5   ", "    6   ", "   7   ", "    8    ", "     9     ", "    10   "      Offset=n-1



-------------------------------------------------------------------


--mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tiled = Tall 1 (3/100) (1/2)        -- Easier Tall layout assignment & changing
defSpacing = mySpacing 8            -- Default Spacing

tiledSp = defSpacing (tiled)       -- For not needing to define spacing for Tall Layout
nBFull = noBorders Full             -- NoBorders on Full without defining each time

--defLayouts = tiled                    -- Layouts to be used in LayoutHook
defLayouts = toggleLayouts (tiled) (nBFull)    -- Layouts to be used in LayoutHook, but ALT+ENTER can be used in Tall to toggle between Full and Tall Layouts
--defLayoutsT a b = a (nBFull) b (tiledSp)     -- Layouts for toggleLayouts
--dLT2 = defLayoutsT

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
--mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

--------------------------------------------------------------------

myLayoutHook = avoidStruts $ smartBorders $ windowNavigation
              (
                --noBorders Full
                toggleLayouts (nBFull) (tiledSp)
                ||| defLayouts
                -- ||| mySpacing 8 (Tall 1 (3/100) (1/2))
                -- ||| Grid
                -- ||| toggleLayouts Full (Tall 1 (3/100) (1/2))
              )

--------------------------------------------------------------------

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1

-------------------------------------------------------------------


main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ docks defaultConfig
      {
          borderWidth         = 3
          , terminal          = "alacritty"
          --, layoutHook        = smartBorders . avoidStruts . spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ layoutHook defaultConfig
          , workspaces = myWorkspaces
          , manageHook = myManageHook
          , layoutHook        = myLayoutHook
          , logHook           = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP {
                                ppOutput = hPutStrLn xmproc
                              , ppCurrent = xmobarColor "#56B24E" "" . wrap "[" "]" -- Current workspace in xmobar
                              , ppVisible = xmobarColor "#56B24E" ""                -- Visible but not current workspace
                              , ppHidden = xmobarColor "#368d33" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                              , ppHiddenNoWindows = xmobarColor "#18EB00" ""        -- Hidden workspaces (no windows)
                              , ppTitle = xmobarColor "#b3afc2" "" . shorten 25     -- Title of active window in xmobar
                              , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"          -- Separators in xmobar
                              , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                              , ppExtras  = [windowCount]                           -- # of windows current workspace
                              --, ppSort = getSortByXineramaRule
                              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                              }
                              -- >> updatePointer (0.95, 0.95) (0.95, 0.95)
                              -- >> updatePointer (1, 1) (0, 0)
                              >> updatePointer (0.95, 0.95) (0, 0)
          --, focusedBorderColor = "#2aa198"
          , focusedBorderColor = "#56B24E"
          , normalBorderColor = "#282c34"
          , handleEventHook    = handleEventHook def <+> fullscreenEventHook
          -- , modMask = mod1Mask    -- Rebind Mod (Default is ALT) to the Windows Key
      }
          `additionalKeys`
           [
            ((controlMask, xK_F1), spawn "pcmanfm")       -- spawn app (CTRL F1)
            , ((controlMask, xK_F2), spawn "chromium")    -- spawn app (CTRL F2)
            , ((controlMask, xK_F3), spawn "epdfview")       -- spawn app (CTRL F3)
            , ((controlMask, xK_F4), spawn "code")        -- spawn app (CTRL F4)

            , ((mod1Mask .|. controlMask, xK_b), spawn "icecat") -- spawn browser (C-M-b)
            , ((mod4Mask, xK_r), spawn "jgmenu_run") -- Open application menu Windows Key+r

            , ((mod1Mask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave) -- Cycle workspaces (ALT TAB)
            --, ((mod1Mask, xK_Return), promote)                          -- Promote selected window to master pane (ALT ENTER)
            , ((mod1Mask .|. controlMask, xK_Right), nextWS)           -- shift to next WS (ALT UP-ARROW)
            , ((mod1Mask .|. controlMask, xK_Left), prevWS)            -- shift to previous WS (ALT DOWN-ARROW)
            --, ((mod1Mask .|. controlMask, xK_Left), DO.swapWith Prev NonEmptyWS)
            --, ((mod1Mask .|. controlMask, xK_Right), DO.swapWith Next NonEmptyWS)
            --, ((mod1Mask .|. controlMask, xK_Up),  shiftToNext)         -- shift to next WS (ALT + SHIFT DOWN ARROW)
            --, ((mod1Mask .|. controlMask, xK_Down),  shiftToPrev)           -- shift window to previous workspace (ALT + SHIFT UP ARROW)
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

            --------------------------------------------------
            -- Toggle Modes
            --, ((mod1Mask, xK_x), sendMessage $ Toggle MIRROR)
            
            --------------------------------------------------

            , ((mod1Mask, xK_F7), spawn "/usr/bin/pamixer -d 3") -- decrease volume by 3
            , ((mod1Mask, xK_F8), spawn "/usr/bin/pamixer -i 3") -- increase volume by 3
            , ((mod1Mask, xK_F5), spawn "/usr/bin/pamixer -t") -- togglemute

            
            , ((mod1Mask .|. controlMask, xK_i), incWindowSpacing 4)    -- Increase Window Spacing on the Fly
            , ((mod1Mask .|. controlMask, xK_d), decWindowSpacing 4)    -- Decrease Window Spacing on the Fly
            
            , ((mod1Mask, xK_Print), spawn "flameshot gui") -- screenshot
            
            , ((mod1Mask, xK_F12), spawn "killall picom; picom -b & disown") -- Restart Compositor

            , ((mod1Mask .|. controlMask, xK_k), spawn "mousepad ~/Documents/keybinds.txt")  -- Show keybinds
            
            , ((mod1Mask, xK_F11), spawn "killall stalonetray; stalonetray & disown") -- Restart Stalonetray
            
            , ((mod1Mask .|. controlMask, xK_m), spawn "mailspring") -- spawn mail client

            --, ((mod1Mask .|. controlMask, xK_space), sendMessage ToggleLayout) -- Toggle Layouts, specified in LayoutHook

            --, ((mod1Mask, xK_f), moveTo Next EmptyWS)                   -- find a free workspace (ALT F)
            --, ((mod1Mask .|. controlMask, xK_f), moveTo Next NonEmptyWS)  -- (ALT + SHIFT F) cycle between non-empty workspaces (application opened in them)
            --, ((modm .|. shiftMask, xK_Up),    shiftToPrev)
            --, ((modm,               xK_Right), nextScreen)
            --, ((modm,               xK_Left),  prevScreen)
            --, ((modm .|. shiftMask, xK_Right), shiftNextScreen)
            --, ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
            , ((mod1Mask .|. controlMask, xK_z), toggleWS)                -- (ALT + Z) cycle between workspaces that are being used
            
            --, ((mod1Mask, xK_F7, lowerVolume 3 >> return ()))

            , ((mod1Mask, xK_Return), sendMessage ToggleLayout) -- Toggle Layouts (VERY HANDY)
            --, ((mod1Mask .|. controlMask, xK_f), sendMessage (Toggle "Full"))
            --, ((mod1Mask .|. shiftMask, xK_f), myLayout)
            --, ((mod1Mask .|. controlMask, xK_Right),                  -- a crazy keybinding!
                  --do t <- findWorkspace getSortByXineramaRule Next NonEmptyWS 2
                    --windows . view $ t )
           ]
