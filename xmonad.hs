{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

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
import Data.Maybe (fromJust, listToMaybe, maybeToList)
import Data.List (delete, nub)
import qualified XMonad.StackSet as S
import qualified XMonad.Operations as O
import qualified Data.Map
import XMonad.Util.NamedWindows (getName)
import Data.Monoid
-- import XMonad.Actions.OnScreen

import XMobar
import DocksFullscreen

import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.Ssh

main = do
    -- spawn "stalonetray -c /home/nadrieril/.xmonad/stalonetrayrc"
    conf <- customXMobar defaultXmConfig (gnomeConfig
        {
          modMask = mod4Mask
        , terminal = "gnome-terminal-wrapper"
        , workspaces = workspaces'
        , layoutHook = layoutHook'
        , manageHook = placeHook simpleSmart <+> manageHook gnomeConfig <+> manageHook'
        , handleEventHook = eventHook'
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#004080"
        , mouseBindings = mouseBindings'
        } `additionalKeysP` keys')
    xmonad $ docksFullscreenConfig conf


workspaces' = ["1:main","2:web","3:dev","4:term"]


layoutHook' =
        -- onWorkspaces ["3:dev", "4:term"] (tiled ||| Mirror tiled ||| accordion ||| full) $
        full ||| tiled
    where
        full = noBorders simpleTabbed
        tiled = smartBorders (Tall 1 (3/100) (1/2))
        accordion = smartBorders (Mirror (Tall 0 (3/100) (1/2)))

-- manageHook' = (<+>) workspaceByPos $ composeAll $
manageHook' = composeAll $
    [resource  =? r --> doIgnore             |   r   <- _ignored] ++
    [className =? c --> doCenterFloat        |   c   <- _floating ] ++
    [className =? c --> viewShift wkspace  | (wkspace, classes) <- wkspaceByClass, c <- classes] -- ++

    where
        viewShift = doF . liftM2 (.) S.greedyView S.shift

        wkspaceByClass = [
            -- ("2:web", ["Firefox","Google-chrome","Chromium", "Chromium-browser"])
            -- , ("4:term", ["Gnome-terminal"])
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
        , ("M-z", toggleWS)
        -- , ("M-t", withFocused $ windows . S.sink)
        -- , ("M-S-t", withFocused $ windows . S.float)

        , ("M1-<F4>", kill)
        , ("M1-<Tab>", windows S.focusDown)
        , ("M1-S-<Tab>", windows S.focusUp)

        , ("M-s", sshPrompt defaultXPConfig)
        , ("M-p", spawn "exe=$(yeganesh -x) && exec $exe")]

        mouseBindings' (XConfig {XMonad.modMask = modMask}) = Data.Map.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    , ((modMask, button2), windows . S.sink)
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
    , ((modMask, button4), const $ windows S.focusDown)
    , ((modMask, button5), const $ windows S.focusUp)
    , ((modMask, 8), const nextWS)
    , ((modMask, 9), const prevWS)
    ]
