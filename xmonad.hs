{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
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
import Data.Maybe (fromJust, listToMaybe, maybeToList, isNothing, maybe)
import Data.List (delete, nub, partition, find)
import Control.Monad (forM_)
import qualified XMonad.StackSet as S
import qualified XMonad.Operations as O
import qualified Data.Map
import XMonad.Util.NamedWindows (getName)
import Data.Monoid
-- import XMonad.Actions.OnScreen

import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Actions.CycleWS

import XMonad.Actions.TopicSpace (TopicConfig(..), defaultTopicConfig, topicAction)

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
------------------------------------------------------
import XMobar
import DocksFullscreen
import Scratchpad (scratchpadConfig, toggleScratchpad)
------------------------------------------------------

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
        -- , logHook = logHook'
        , handleEventHook = eventHook'
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#004080"
        , mouseBindings = mouseBindings'
        , keys = azertyKeys <+> keys defaultConfig
        } `additionalKeysP` keys'


workspaces' = ["web","dev","git","irc","music"]

topicConfig = defaultTopicConfig
    { topicDirs = Data.Map.fromList
        [ ("dev", "projects")
        ]
    , defaultTopicAction = const $ return ()
    , defaultTopic = "dashboard"
    , topicActions = Data.Map.fromList
        [ ("web", spawn "google-chrome")
        , ("git", spawn "smartgithg")
        , ("irc", spawn "quasselclient")
        , ("music", spawn "ario")
        ]
    }

manageHook' = composeAll $
       [appName  =? r --> doIgnore             |   r   <- _ignored]
    ++ [className =? c --> doCenterFloat        |   c   <- _floating ]
    ++ [className =? c --> viewShift wkspace
            | (wkspace, classes) <- wkspaceByClass, c <- classes]
    -- ++ [isInProperty "WM_NAME" "Quassel IRC" --> shift "irc"]

    where
        viewShift = doF . liftM2 (.) S.view S.shift
        -- shift = doF . S.shift

        wkspaceByClass =
            [ ("web", ["Firefox","Google-chrome","Chromium","Chromium-browser"])
            , ("dev", ["Atom"])
            , ("git", ["SmartGit/Hg"])
            , ("irc", ["quasselclient"])
            , ("music", ["Rhythmbox"])
            ]

        _floating  = ["Xmessage","Nm-connection-editor"]
        _ignored = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]


layoutHook' =
        onWorkspaces ["term"] (doubletiled ||| full ||| tiled) $
        full ||| tiled
    where
        full = noBorders simpleTabbed
        tiled = smartBorders (Tall 1 (3/100) (1/2))
        doubletiled = smartBorders (Tall 2 (3/100) (1/2))
        -- accordion = smartBorders (Mirror (Tall 0 (3/100) (1/2)))


logHook' = do
    ws <- gets windowset
    let crnt = S.workspace $ S.current ws
    when (isNothing (S.stack crnt)) $ topicAction topicConfig (S.tag crnt)

        wkspaceByClass =
            [ ("git", ["SmartGit/Hg"], False)
            , ("irc", ["quasselclient"], False)
            , ("web", ["Firefox","Google-chrome","Chromium","Chromium-browser"], True)
            , ("dev", ["Atom"], True)
            ]

        _floating  = ["Xmessage","Nm-connection-editor"]
        _ignored = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]


keys' = [ ("M-S-q", spawn "gnome-session-quit")
        , ("M-S-l", spawn "gnome-screensaver-command -l")

        , ("M-<U>", nextHiddenWS)
        , ("M-<D>", prevHiddenWS)
        , ("M-S-<U>", shiftToNextHidden >> nextHiddenWS)
        , ("M-S-<D>", shiftToPrevHidden >> prevHiddenWS)
        , ("M-<R>", nextScreen)
        , ("M-<L>", prevScreen)
        , ("M-S-<R>", shiftNextScreen >> nextScreen)
        , ("M-S-<L>", shiftPrevScreen >> prevScreen)
        , ("M-<Tab>", toggleWS)
        , ("M-z", toggleScratchpad)

        , ("M1-<F4>", kill)
        , ("M1-<Tab>", windows S.focusDown)
        , ("M1-S-<Tab>", windows S.focusUp)

        , ("M-s", sshPrompt defaultXPConfig)
        , ("M-p", spawn "exec $(yeganesh -x)")]


mouseBindings' (XConfig {XMonad.modMask = modMask}) = Data.Map.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    , ((modMask, button2), killWindow)
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
    , ((modMask, button4), const $ windows S.focusDown)
    , ((modMask, button5), const $ windows S.focusUp)
    , ((modMask, 8), const prevHiddenWS)
    , ((modMask, 9), const nextHiddenWS)
    , ((modMask .|. shiftMask, 8), const $ shiftToPrevHidden >> prevHiddenWS)
    , ((modMask .|. shiftMask, 9), const $ shiftToNextHidden >> nextHiddenWS)
    ]

hiddenWsBy = findWorkspace getSortByIndex Next HiddenWS

prevHiddenWS = switchHiddenWorkspace (-1)
nextHiddenWS = switchHiddenWorkspace 1
switchHiddenWorkspace d = windows . S.view =<< hiddenWsBy d

shiftToPrevHidden = shiftHiddenWorkspace (-1)
shiftToNextHidden = shiftHiddenWorkspace 1
shiftHiddenWorkspace d = windows . S.shift =<< hiddenWsBy d
