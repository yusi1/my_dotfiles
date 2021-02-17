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
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops(fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare

--import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.ToggleLayouts
--import XMonad.Layout.Grid (Grid(..))
--import XMonad.Layout.TwoPane (TwoPane(..))
--import XMonad.Layout.Tabbed (simpleTabbed)
-- import XMonad.Layout.Gaps
import XMonad.Layout.Spacing

import XMonad.Actions.UpdatePointer -- update pointer location to edge of new focused window, to prevent unintended focus stealing
import XMonad.Actions.CycleRecentWS -- cycle recent workspaces with keys defined in myKeys
import XMonad.Actions.Promote -- Promote selected window to master pane
--import XMonad.Actions.Search -- use search engine in XMonad
import XMonad.Actions.CycleWS -- Cycle Workspaces, for example using the arrow keys
import XMonad.Actions.CycleWindows -- Cycle windows in current workspace
-- import XMonad.Actions.WindowNavigation -- Experimental rewrite of layout with same name, allows window navigation with arrow keys

import System.IO

---------------------------------------------------------------------

-- [ Manipulate windows as they are created.
-- | The list is processed from top -> bottom, 
-- [ the first matching rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.

myManageHook = composeAll         -- Add Custom Hook to make certain windows open in floating mode
    [ [ className =? "Steam"    --> doFloat ]
    , [ title =? "Steam" --> doFloat ]
    , [ className =? "steam"    --> doFullFloat ] -- bigpicture-mode
    , [ (className =? "Steam" <&&> resource =? "Dialog") --> doFloat ]
    , [ className =? "Progress" --> doFloat ]
    --, className =? "Pcmanfm"  --> doFloat
    --, className =? "pcmanfm"  --> doFloat
    , [ isFullscreen --> doFullFloat ]
    ]

---------------------------------------------------------------------


--myLayoutHook =
  --smartBorders $ -- layouts begin below
  --noBorders Full
  --`||| Tall 1 10/100 60/100
  --`||| TwoPane 15/100 55/100
  --`||| Mirror (Tall 1 10/100 60/100)
  --`||| Grid
  --`||| simpleTabbed

--------------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ docks defaultConfig
      {
          borderWidth         = 3
          , terminal          = "alacritty"
          , layoutHook        = smartBorders . avoidStruts . spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ layoutHook defaultConfig
          , logHook           = dynamicLogWithPP xmobarPP {
                                ppOutput = hPutStrLn xmproc
                              , ppTitle = xmobarColor "green" "" . shorten 50
                              , ppSort = getSortByXineramaRule
                              }
                              >> updatePointer (0.95, 0.95) (0.95, 0.95)
          , focusedBorderColor = "darkgreen"
          , normalBorderColor = "black" -- Temporary Workaround, window borders become annoying when window is not focused.
          , handleEventHook    = handleEventHook def <+> fullscreenEventHook
          -- , modMask = mod1Mask    -- Rebind Mod (Default is ALT) to the Windows Key
      }
          `additionalKeys`
           [
            ((controlMask, xK_F1), spawn "pcmanfm")       -- spawn app (CTRL F1)
            , ((controlMask, xK_F2), spawn "chromium")    -- spawn app (CTRL F2)
            , ((controlMask, xK_F3), spawn "brave")       -- spawn app (CTRL F3)
            , ((controlMask, xK_F4), spawn "code")        -- spawn app (CTRL F4)
            , ((mod1Mask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave) -- Cycle workspaces (ALT TAB)
            , ((mod1Mask, xK_Return), promote)                          -- Promote selected window to master pane (ALT ENTER)
            , ((mod1Mask .|. controlMask, xK_Right), nextWS)           -- shift to next WS (ALT UP-ARROW)
            , ((mod1Mask .|. controlMask, xK_Left), prevWS)            -- shift to previous WS (ALT DOWN-ARROW)
            --, ((mod1Mask .|. controlMask, xK_Left), DO.swapWith Prev NonEmptyWS)
            --, ((mod1Mask .|. controlMask, xK_Right), DO.swapWith Next NonEmptyWS)
            , ((mod1Mask .|. controlMask, xK_Up),  shiftToNext)         -- shift to next WS (ALT + SHIFT DOWN ARROW)
            , ((mod1Mask .|. controlMask, xK_Down),  shiftToPrev)           -- shift window to previous workspace (ALT + SHIFT UP ARROW)
            --, ((mod1Mask, xK_f), moveTo Next EmptyWS)                   -- find a free workspace (ALT F)
            --, ((mod1Mask .|. controlMask, xK_f), moveTo Next NonEmptyWS)  -- (ALT + SHIFT F) cycle between non-empty workspaces (application opened in them)
            --, ((modm .|. shiftMask, xK_Up),    shiftToPrev)
            --, ((modm,               xK_Right), nextScreen)
            --, ((modm,               xK_Left),  prevScreen)
            --, ((modm .|. shiftMask, xK_Right), shiftNextScreen)
            --, ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
            , ((mod1Mask .|. controlMask, xK_z), toggleWS)                -- (ALT + Z) cycle between workspaces that are being used
            --, ((mod1Mask .|. controlMask, xK_f), sendMessage ToggleLayout)
            --, ((mod1Mask .|. shiftMask, xK_f), myLayout)
            --, ((mod1Mask .|. controlMask, xK_Right),                  -- a crazy keybinding!
                  --do t <- findWorkspace getSortByXineramaRule Next NonEmptyWS 2
                    --windows . view $ t )
           ]
