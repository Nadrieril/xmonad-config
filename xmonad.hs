{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import XMonad.Config.Azerty (azertyKeys)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Tabbed

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Layout.LayoutModifier

import qualified XMonad.StackSet as S
import qualified XMonad.Operations as O
import XMonad.Util.NamedWindows (getName)
import Control.Monad (liftM2, when, forM_)
import Safe (headMay)
import Data.Maybe (fromJust, listToMaybe, maybeToList, isNothing, isJust, maybe)
import Data.List (delete, nub, partition, find)
import qualified Data.Map
import Data.Monoid
-- import XMonad.Actions.OnScreen

import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Actions.CycleWS

import XMonad.Actions.TopicSpace (TopicConfig(..), defaultTopicConfig, topicAction)
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace, removeEmptyWorkspace)

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
------------------------------------------------------
import XMobar
import DocksFullscreen
import Scratchpad (scratchpadConfig, toggleScratchpad)
import qualified DynamicTopicSpace as DTS
------------------------------------------------------

main = xmonad
    $ docksFullscreenConfig
    $ scratchpadConfig
    $ customXMobar defaultXmConfig
    $ DTS.dynamicTopicsConfig topics
    $ gnomeConfig {
          modMask = mod4Mask
        , terminal = "gnome-terminal-wrapper"
        , layoutHook = layoutHook'
        , manageHook = placeHook simpleSmart <+> manageHook gnomeConfig <+> manageHook'
        , handleEventHook = eventHook'
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#004080"
        , mouseBindings = mouseBindings'
        , keys = azertyKeys' <+> azertyKeys <+> keys defaultConfig
        } `additionalKeysP` keys'

azertyKeys' (XConfig {modMask = modm}) = Data.Map.fromList
    [((m .|. modm, k), withNthWorkspace f i)
        | (i, k) <- zip [0..] [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0],
          (f, m) <- [(S.greedyView, 0), (liftM2 (.) S.view S.shift, shiftMask)]]



topics =
    [ ("main",      Nothing,                    Nothing)
    , ("web",       Nothing,                    Just $ spawn "google-chrome")
    , ("dev",       Just "$HOME/projects",      Nothing)
    ] ++ projecttopics
        [ ("xm", "xmonad", return ())
        , ("b-a", "bars-angular", return ())
        , ("b-d", "bars-django", return ())
        ]
    ++
    [ ("git",       Nothing,                    Just $ spawn "smartgithg")
    , ("irc",       Nothing,                    Just $ spawn "quasselclient")
    , ("music",     Just "$HOME/Music",         Just $ spawn "ario")
    , ("videos",    Just "$HOME/Videos",        Just $ spawn "nautilus $HOME/Videos")
    ]
    where projecttopics = map
            (\(n, p, a) -> ( "dev/"++n
                           , Just $ "$HOME/projects/"++p
                           , Just $ spawn ("atom ~/projects/"++p) >> a))


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
            -- , ("dev", ["Atom"])
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


eventHook' :: Event -> X All
eventHook' e@ClientMessageEvent { ev_message_type = mt, ev_data = dt } = do
    switch_evt <- getAtom "XMONAD_SWITCHWKSP"
    shift_evt <- getAtom "XMONAD_SHIFTWKSP"
    killw_evt <- getAtom "XMONAD_KILLWKSP"
    killf_evt <- getAtom "XMONAD_KILLFOCUSED"

    all_workspaces <- asks (workspaces . config)
    let wk = all_workspaces !! fromIntegral (head dt)
    when (mt==switch_evt) $ windows $ S.greedyView wk
    when (mt==shift_evt) $ windows $ liftM2 (.) S.greedyView S.shift wk
    when (mt==killw_evt) $ do
        ws <- gets windowset
        maybe (return ()) clearWorkspace $ find ((== wk). S.tag) $ S.workspaces ws
        DTS.removeWorkspace wk
    when (mt==killf_evt) $ do
        ws <- gets windowset
        let stk = S.stack $ S.workspace $ S.current ws
        maybe (return ()) (killWindow . S.focus) stk

    return (All True)
eventHook' _ = return (All True)

clearWorkspace wk = forM_ (S.integrate' $ S.stack wk) killWindow



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
        , ("M-w", DTS.topicPrompt DTS.goto)

        , ("M1-<F4>", kill)
        , ("M1-C-t", DTS.spawnShell)
        , ("M1-<Tab>", windows S.focusDown)
        , ("M1-S-<Tab>", windows S.focusUp)

        , ("M-s", sshPrompt defaultXPConfig)
        , ("M-p", spawn "exec $(yeganesh -x)")]


mouseBindings' (XConfig {XMonad.modMask = modMask}) = Data.Map.fromList
    [ ((modMask, button1), mouseMoveWindow)
    , ((modMask, button2), killWindow)
    , ((modMask, button3), mouseResizeWindow)
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
