{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import XMonad.Config.Azerty (azertyKeys)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Tabbed

import Control.Monad (liftM2, when)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Layout.LayoutModifier

import Safe (headMay)
import Data.Maybe (fromJust, listToMaybe, maybeToList, isNothing)
import Data.List (delete, nub, partition, find)
import qualified XMonad.StackSet as S
import qualified XMonad.Operations as O
import qualified Data.Map
import XMonad.Util.NamedWindows (getName)
import Data.Monoid
-- import XMonad.Actions.OnScreen

import XMobar
import DocksFullscreen
import Scratchpad (scratchpadConfig, toggleScratchpad)

import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh

main = xmonad
    $ docksFullscreenConfig
    $ scratchpadConfig
    $ customXMobar defaultXmConfig
    $ gnomeConfig {
          modMask = mod4Mask
        , terminal = "gnome-terminal-wrapper"
        , workspaces = workspaces'
        , layoutHook = layoutHook'
        , manageHook = placeHook simpleSmart <+> manageHook gnomeConfig <+> manageHook'
        , handleEventHook = eventHook'
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#004080"
        , mouseBindings = mouseBindings'
        , keys = azertyKeys <+> keys defaultConfig
        } `additionalKeysP` keys'


workspaces' = ["main","web","dev"]


layoutHook' =
        onWorkspaces ["term"] (doubletiled ||| full ||| tiled) $
        full ||| tiled
    where
        full = noBorders simpleTabbed
        tiled = smartBorders (Tall 1 (3/100) (1/2))
        doubletiled = smartBorders (Tall 2 (3/100) (1/2))
        -- accordion = smartBorders (Mirror (Tall 0 (3/100) (1/2)))

manageHook' = composeAll $
    [appName  =? r --> doIgnore             |   r   <- _ignored] ++
    [className =? c --> doCenterFloat        |   c   <- _floating ] ++
    [className =? c --> viewShift wkspace  | (wkspace, classes) <- wkspaceByClass, c <- classes] -- ++

    where
        viewShift = doF . liftM2 (.) S.greedyView S.shift

        wkspaceByClass = [
            -- , ("web", ["Firefox","Google-chrome","Chromium", "Chromium-browser"])
            ]

        _floating  = ["Xmessage","Nm-connection-editor"]
        _ignored = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]


keys' = [ ("M-S-q", spawn "gnome-session-quit")
        , ("M-S-l", spawn "gnome-screensaver-command -l")

        , ("M-<D>", nextWS)
        , ("M-<U>", prevWS)
        , ("M-S-<D>", shiftToNext >> nextWS)
        , ("M-S-<U>", shiftToPrev >> prevWS)
        , ("M-<R>", nextScreen)
        , ("M-<L>", prevScreen)
        , ("M-S-<R>", shiftNextScreen >> nextScreen)
        , ("M-S-<L>", shiftPrevScreen >> prevScreen)
        , ("M-<Tab>", toggleWS)
        , ("M-z", toggleScratchpad)

        , ("M1-<F4>", kill)
        , ("M1-<Tab>", windows S.focusDown)
        , ("M1-S-<Tab>", windows S.focusUp)

        , ("M-s", prompt "gnome-terminal-wrapper -e " defaultXPConfig)
        , ("M-S-s", sshPrompt defaultXPConfig)
        , ("M-p", spawn "exec $(yeganesh -x)")]


mouseBindings' (XConfig {XMonad.modMask = modMask}) = Data.Map.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    , ((modMask, button2), windows . S.sink)
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
    , ((modMask, button4), const $ windows S.focusDown)
    , ((modMask, button5), const $ windows S.focusUp)
    , ((modMask, 8), const nextWS)
    , ((modMask, 9), const prevWS)
    ]
