{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
import XMonad
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Config.Azerty (azertyKeys)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.Maximize (maximize, maximizeRestore)

import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.Place (placeHook, simpleSmart)

import XMonad.Actions.OnScreen (onScreen, onScreen', Focus(..))
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, toggleWS, findWorkspace, WSType(..), Direction1D(..))
import qualified XMonad.Actions.TopicSpace as TS
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)

import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh (sshPrompt)

import Control.Monad (liftM2, when, forM_)
import qualified Data.Map
import Data.List (delete, nub, partition, find)
import Data.Maybe (fromJust, listToMaybe, maybeToList, isNothing, isJust, maybe)
import Data.Monoid (All(..))
import Safe (headMay)
------------------------------------------------------
import XMobar
import DocksFullscreen
import qualified DynamicTopicSpace as DTS
import ManageNext (manageNext, manageManageNext)
------------------------------------------------------

main = xmonad
    $ docksFullscreenConfig
    $ customXMobar defaultXmConfig
    $ DTS.dynamicTopicsConfig topicConfig
    $ gnomeConfig {
          modMask = mod4Mask
        , terminal = "gnome-terminal.wrapper"
        , layoutHook = layoutHook'
        , manageHook = manageManageNext <+> manageSpawn <+> placeHook simpleSmart <+> manageHook gnomeConfig <+> manageHook'
        , handleEventHook = eventHook'
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#004080"
        , mouseBindings = mouseBindings'
        , keys = azertyKeys' <+> azertyKeys <+> numpadKeys <+> keys defaultConfig
        } `additionalKeysP` keys'

azertyKeys' (XConfig {modMask = modm}) = Data.Map.fromList
    [((m .|. modm, k), withNthWorkspace f i)
        | (i, k) <- zip [0..] [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0],
          (f, m) <- [(S.greedyView, 0), (liftM2 (.) S.view S.shift, shiftMask)]]

numpadKeys (XConfig {modMask = modm}) = Data.Map.fromList
    [((m .|. modm, k), withNthWorkspace f i)
        | (i, k) <- zip [0..] numpadKeys_,
          (f, m) <- [(S.greedyView, 0), (liftM2 (.) S.view S.shift, shiftMask)]]
    where numpadKeys_ =
            [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
            , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
            , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
            , xK_KP_Insert] -- 0


runOnByClass :: FilePath -> [String] -> WorkspaceId -> X ()
runOnByClass prog classNames wk = do
    manageNext query (doF $ S.shift wk)
    spawn prog
    where query = foldl1 (<||>) (map (className =?) classNames)


topicConfig = DTS.fromList $
    [ ("main",      Nothing,                      Nothing)
    , ("web",       Nothing,                      Just $ spawnOn "web" "google-chrome")
    , ("game",      Nothing,                      Nothing)
    , ("video",     Just "$HOME/Videos",          Just spawnFilemanager)
    ] ++

    [ ("dev",       Just "$HOME/projects",        Nothing)
    , ("dev/java",  Just "$HOME/projects/java",   Just $ spawnOn "dev/java" "eclipse")
    ] ++ projecttopics
        [ ("xm",  "xmonad", return ())
        , ("b-a", "bars-angular", return ())
        , ("b-d", "bars-django", return ())
        , ("psc", "PSC", return ())
        ]
    ++
    [ ("backup",    Nothing,                      Just $ spawnOn "backup" "grsync")
    , ("mail",      Nothing,                      Just $ spawnOn "mail" "icedove")
    , ("git",       Nothing,                      Just $ spawnOn "git" "smartgithg")
    , ("irc",       Nothing,                      Just $ spawnOn "irc" "quasselclient")
    , ("music",     Just "$HOME/Music",           Just $ spawnOn "music" "rhythmbox")
    , ("term",      Nothing,                      Just spawnLocalShell)
    ] ++ [ (show i, Nothing, Nothing) | i <- [0..5] ]
    where projecttopics l = do
            (n, p, a) <- l
            let wk = "dev/"++n
            return ( wk
                   , Just $ "$HOME/projects/"++p
                --    , Just $ spawnOn wk ("atom ~/projects/"++p) >> a)
                   , Just $ runOnByClass ("atom ~/projects/"++p) classes wk >> a)
            where classes = ["Atom"]


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
            , ("music", ["ario"])
            ]

        _floating  = ["Xmessage","Nm-connection-editor"]
        _ignored = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]


layoutHook' = maximize $
        onWorkspaces ["term"] (doubletiled ||| full ||| tiled) $
        onWorkspaces ["dev/b-a", "dev/b-d"] (full ||| topbar ||| tiled) $
        full ||| tiled
    where
        full = noBorders simpleTabbed
        tiled = smartBorders (Tall 1 (3/100) (1/2))
        topbar = smartBorders (Mirror $ Tall 1 (3/100) (7/100))
        doubletiled = smartBorders (Tall 2 (3/100) (1/2))
        -- accordion = smartBorders (Mirror (Tall 0 (3/100) (1/2)))


eventHook' :: Event -> X All
eventHook' e@ClientMessageEvent { ev_message_type = mt, ev_data = dt } = do
    all_workspaces <- asks (workspaces . config)
    let n = fromIntegral (head dt)
    let wk = all_workspaces !! (n `div` 10) -- arbitrary limit to 10 screens
    let scr = S $ n `mod` 10

    ifEvt "XMONAD_NEXTWKSP" $ onScreen' nextHiddenWS FocusNew scr
    ifEvt "XMONAD_PREVWKSP" $ onScreen' prevHiddenWS FocusNew scr
    ifEvt "XMONAD_SWITCHWKSP" $ windows $ onScreen (S.greedyView wk) FocusNew scr
    ifEvt "XMONAD_SHIFTWKSP" $ windows $ onScreen (liftM2 (.) S.greedyView S.shift wk) FocusNew scr
    ifEvt "XMONAD_KILLWKSP" $ do
        empty <- isEmpty wk
        if empty
            then DTS.removeWorkspace wk
            else DTS.clearWorkspace wk

    ifEvt "XMONAD_FOCUSUP" $ windows $ onScreen S.focusUp FocusNew scr
    ifEvt "XMONAD_FOCUSDOWN" $ windows $ onScreen S.focusDown FocusNew scr
    ifEvt "XMONAD_KILLFOCUSED" $ onScreen' kill FocusNew scr

    return (All True)
    where ifEvt name x = getAtom name >>= \evt -> when (mt == evt) x
          isEmpty wk = do
            wks <- gets (S.workspaces . windowset)
            return $ isNothing $ S.stack =<< find ((==wk) . S.tag) wks
eventHook' _ = return (All True)


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
        , ("M-w", DTS.topicGridSelect >>= maybe (return ()) DTS.goto)
        , ("M-S-w", do
            wk <- gets (S.currentTag . windowset)
            DTS.clearWorkspace wk
            DTS.removeWorkspace wk)

        , ("M1-C-t", spawnLocalShell)
        , ("M-n", spawnFilemanager)

        , ("M1-<Tab>", windows S.focusDown)
        , ("M1-S-<Tab>", windows S.focusUp)
        , ("M-<Esc>", withFocused (sendMessage . maximizeRestore))
        , ("M-c", kill)
        , ("M1-<F4>", kill)

        , ("M-s", sshPrompt defaultXPConfig)
        , ("M-p", spawn "exec $(yeganesh -x)")

        -- , ("<XF86AudioMute>", spawn "mpc volume 0")
        -- , ("<XF86AudioRaiseVolume>", spawn "mpc volume +2")
        -- , ("<XF86AudioLowerVolume>", spawn "mpc volume -2")
        , ("<XF86AudioPlay>", spawn "mpc toggle")
        , ("<XF86AudioStop>", spawn "mpc stop")
        , ("<XF86AudioPrev>", spawn "mpc prev")
        , ("<XF86AudioNext>", spawn "mpc next")
        ]




spawnLocalShell = XS.gets DTS.makeTopicConfig >>= TS.currentTopicDir >>= spawnShellIn
-- spawnShellIn dir = spawn $ "xterm -e 'cd \"" ++ dir ++ "\" && $SHELL'"
spawnShellIn dir = spawn $ "gnome-terminal --working-directory=\"" ++ dir ++ "\""
spawnFilemanager = XS.gets DTS.makeTopicConfig >>= TS.currentTopicDir >>= spawnFilemanagerIn
spawnFilemanagerIn dir = spawn $ "nautilus " ++ dir


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
