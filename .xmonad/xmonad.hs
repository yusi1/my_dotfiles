-- My XMonad Configuration File
-- Written By Yusef Aslam
-- Copied stuff from many sources including: https://pastebin.com/BWrvV4SH
-- Also, https://www.reddit.com/r/xmonad/comments/hiysu2/cant_open_steam_games/
-- https://www.reddit.com/r/xmonad/comments/herxnc/steam_and_xmonad/
-- Based on John Goerzens Configuration
-- Bits of the file have been gained (different features) as I read documentation etc..
-- Remember READ documentation, it's very useful (or even find random questions on reddit/github/stackoverflow etc...)
-- Default Keys https://gist.github.com/c33k/1ecde9be24959f1c738d

----------------------Imports----------------------------------------------

module Main (main) where

import XMonad

import qualified Data.Map as M
--import Data.Default
--import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops(fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare

import XMonad.Layout
--import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
--import XMonad.Layout.NoBorders(smartBorders, noBorders)
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
--import XMonad.Layout.TwoPane (TwoPane(..))
--import XMonad.Layout.Tabbed (simpleTabbed)
-- import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Maximize
import XMonad.Layout.LayoutModifier

--import XMonad.Layout.MultiToggle
--import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ToggleLayouts

import XMonad.Layout.WindowNavigation
import XMonad.Layout.AvoidFloats

import XMonad.Actions.UpdatePointer -- update pointer location to edge of new focused window, to prevent unintended focus stealing
import XMonad.Actions.CycleRecentWS -- cycle recent workspaces with keys defined in myKeys
import XMonad.Actions.Promote -- Promote selected window to master pane
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

myManageHook = composeAll         -- Add Custom Hook to make certain windows open in floating mode
    [ 
      [ className =? "Steam"    --> doFloat ]
      , [ title =? "Steam" --> doFloat ]
      , [ className =? "steam"    --> doFullFloat ] -- bigpicture-mode
      , [ (className =? "Steam" <&&> resource =? "Dialog") --> doFloat ]
      , [ className =? "Progress" --> doFloat ]
      , [ className =? "Pcmanfm"  --> doFloat ]
      , [ className =? "pcmanfm"  --> doFloat ]
      --, [ className =? "yusef"  --> doFloat ]
      , [ className =? "Xmessage" --> doFloat ]
      , [ isFullscreen --> doFullFloat ]
    ]

--------------------------------------------------------------------


--mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tiled = Tall 1 (3/100) (1/2)        -- Easier Tall layout assignment & changing
defSpacing = mySpacing 8            -- Default Spacing

tiledSp = defSpacing (tiled)        -- For not needing to define spacing for Tall Layout
nBFull = noBorders Full             -- NoBorders on Full without defining each time

defLayouts = tiled ||| Grid ||| nBFull      -- Layouts to be used in LayoutHook
-- defLayoutsT a b = a (nBFull) b (tiledSp)        -- Layouts for toggleLayouts
--dLT2 = defLayoutsT

--mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
--mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

--------------------------------------------------------------------

myLayoutHook = avoidStruts $ smartBorders 
              $ windowNavigation(
                                  --noBorders Full
                                  toggleLayouts (nBFull) (tiledSp)
                                  ||| defLayouts
                                  -- ||| mySpacing 8 (Tall 1 (3/100) (1/2))
                                  -- ||| Grid
                                  -- ||| toggleLayouts Full (Tall 1 (3/100) (1/2))
                                )

--------------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ docks defaultConfig
      {
          borderWidth         = 3
          , terminal          = "alacritty"
          --, layoutHook        = smartBorders . avoidStruts . spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ layoutHook defaultConfig
          , layoutHook        = myLayoutHook
          , logHook           = dynamicLogWithPP xmobarPP {
                                ppOutput = hPutStrLn xmproc
                              , ppTitle = xmobarColor "#93a1a1" "" . shorten 50
                              , ppSort = getSortByXineramaRule
                              }
                              >> updatePointer (0.95, 0.95) (0.95, 0.95)
          , focusedBorderColor = "#2aa198"
          , normalBorderColor = "#545454"
          , handleEventHook    = handleEventHook def <+> fullscreenEventHook
          -- , modMask = mod1Mask    -- Rebind Mod (Default is ALT) to the Windows Key
      }
          `additionalKeys`
           [
            ((controlMask, xK_F1), spawn "pcmanfm")       -- spawn app (CTRL F1)
            , ((controlMask, xK_F2), spawn "chromium")    -- spawn app (CTRL F2)
            , ((controlMask, xK_F3), spawn "brave")       -- spawn app (CTRL F3)
            , ((controlMask, xK_F4), spawn "code")        -- spawn app (CTRL F4)

            , ((mod1Mask .|. controlMask, xK_b), spawn "icecat") -- spawn browser (C-M-b)
            , ((mod4Mask, xK_r), spawn "jgmenu_run") -- Open application menu Windows Key+r

            , ((mod1Mask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave) -- Cycle workspaces (ALT TAB)
            --, ((mod1Mask, xK_Return), promote)                          -- Promote selected window to master pane (ALT ENTER)
            --, ((mod1Mask .|. controlMask, xK_Right), nextWS)           -- shift to next WS (ALT UP-ARROW)
            --, ((mod1Mask .|. controlMask, xK_Left), prevWS)            -- shift to previous WS (ALT DOWN-ARROW)
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

            --, ((mod1Mask .|. shiftMask, xK_f), myLayout)
            --, ((mod1Mask .|. controlMask, xK_Right),                  -- a crazy keybinding!
                  --do t <- findWorkspace getSortByXineramaRule Next NonEmptyWS 2
                    --windows . view $ t )
           ]
