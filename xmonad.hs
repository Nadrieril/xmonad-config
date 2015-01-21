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

import XMonad.Hooks.ManageHelpers (doCenterFloat, isInProperty)
import XMonad.Hooks.Place (placeHook, simpleSmart)

import XMonad.Actions.Volume (lowerVolume, raiseVolume, toggleMute)
import XMonad.Actions.OnScreen (onScreen, onScreen', Focus(..))
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, toggleWS, findWorkspace, WSType(..), Direction1D(..))
import qualified XMonad.Actions.TopicSpace as TS
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import XMonad.Actions.Warp (warpToWindow)
-- import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import qualified XMonad.Actions.Search as Search

import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Prompt.Man (manPrompt)

import Control.Monad (liftM2, when, forM_, void)
import Control.Applicative ((<$>))
import qualified Data.Map
import Data.List (delete, nub, partition, find)
import Data.Maybe (fromJust, listToMaybe, maybeToList, isNothing, isJust, maybe)
import Data.Monoid (All(..), mconcat)
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
        , startupHook = spawn "killall unclutter; unclutter"
        , layoutHook = layoutHook'
        -- , logHook = updatePointer (Relative 0.9 0.9)
        , manageHook = mconcat
            [ manageManageNext
            , manageSpawn
            , placeHook simpleSmart
            , manageHook gnomeConfig
            , manageHook' ]
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


queryFromClasses :: [String] -> Query Bool
queryFromClasses classNames = foldl1 (<||>) (map (className =?) classNames)
topic = DTS.defaultTopic

topicConfig = DTS.fromList $
    [ ("web", topic {
        DTS.topicAction = flip spawnOn "google-chrome",
        DTS.topicWindows = queryFromClasses ["Firefox","Google-chrome","Chromium","Chromium-browser"]
    })
    , ("main", topic)
    , ("game", topic)
    , ("video", topic {
        DTS.topicDir = "$HOME/Videos",
        DTS.topicAction = const spawnFilemanager,
        DTS.topicWindows = queryFromClasses ["Vlc"]
    })
    ] ++

    [ ("dev", topic {
        DTS.topicDir = "$HOME/projects"
    })
    , ("dev/java", topic {
        DTS.topicDir = "$HOME/projects/java",
        DTS.topicAction = flip spawnOn "eclipse"
    })
    ] ++ projecttopics
        [ ("xm",  "xmonad", return ())
        , ("b-a", "bars-angular", spawnLocalIShellCmd "grunt serve")
        , ("b-d", "bars-django", do
            spawnLocalIShellCmd "python manage.py runserver_plus"
            spawnLocalIShellCmd "ssh -t srv@nadrieril 'cd /srv/bars/bars-django; $SHELL'"
            spawnLocalShell)
        , ("psc", "PSC", return ())
        , ("24h", "24hnatation", return ())
        ]
    ++
    [ ("backup", topic {
        DTS.topicAction = flip spawnOn "grsync"
    })
    , ("mail", topic {
        DTS.topicAction = flip spawnOn "icedove"
    })
    , ("git", topic {
        DTS.topicAction = flip spawnOn "smartgithg",
        DTS.topicWindows = queryFromClasses ["SmartGit/Hg"]
    })
    , ("irc", topic {
        DTS.topicAction = flip spawnOn "quasselclient"
        -- DTS.topicWindows = queryFromClasses ["quasselclient"]
        --     <||> isInProperty "_NET_WM_NAME" "Quassel IRC"
        --     <||> isInProperty "WM_COMMAND" "quasselclient"
    })
    , ("music", topic {
        DTS.topicDir = "$HOME/Music",
        DTS.topicAction = flip spawnOn "rhythmbox",
        DTS.topicWindows = queryFromClasses ["Rhythmbox", "ario"]
    })
    , ("term", topic {
        DTS.topicAction = const spawnLocalShell
    })
    ] ++ [ (show i, topic) | i <- [0..5] ]
    where projecttopics l = do
            (n, p, a) <- l
            return ("dev/"++n, topic {
                DTS.topicDir = "$HOME/projects/"++p,
                DTS.topicAction = \wk -> runOnByClass ("atom ~/projects/"++p) classes wk >> a
            })
            where classes = ["Atom"]


layoutHook' = maximize $
        onWorkspaces ["term"] (doubletiled ||| full ||| tiled) $
        onWorkspaces ["dev/b-a"] (topbar ||| full) $
        onWorkspaces ["dev/b-d"] (topbar2 ||| full) $
        full ||| tiled
    where
        full = noBorders simpleTabbed
        tiled = smartBorders (Tall 1 (3/100) (1/2))
        topbar = smartBorders (Mirror $ Tall 1 (3/100) (7/100))
        topbar2 = smartBorders (Mirror $ Tall 2 (3/100) (7/100))
        doubletiled = smartBorders (Tall 2 (3/100) (1/2))
        -- accordion = smartBorders (Mirror (Tall 0 (3/100) (1/2)))


manageHook' = composeAll $
       [appName  =? r --> doIgnore | r <- _ignored]
    ++ [className =? c --> doCenterFloat | c <- _floating ]

    where
        _floating  = ["Xmessage","Nm-connection-editor"]
        _ignored = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]


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
        , ("M-<Tab>", toggleWS)

        , ("M-C-<Tab>", nextScreen >> warpToWindow 0.9 0.9)
        , ("M-C-S-<Tab>", shiftNextScreen >> nextScreen >> warpToWindow 0.9 0.9)
        , ("M-M1-<Tab>", swapScreens)
        -- , ("M-<R>", nextScreen >> warpToWindow 0.9 0.9)
        -- , ("M-<L>", prevScreen >> warpToWindow 0.9 0.9)
        -- , ("M-S-<R>", shiftNextScreen >> nextScreen >> warpToWindow 0.9 0.9)
        -- , ("M-S-<L>", shiftPrevScreen >> prevScreen >> warpToWindow 0.9 0.9)

        , ("M-w", DTS.topicGridSelect >>= maybe (return ()) DTS.goto)
        , ("M-M1-w", DTS.currentTopicAction)
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

        , ("<XF86AudioMute>", void toggleMute)
        , ("<XF86AudioRaiseVolume>", void $ raiseVolume 2)
        , ("<XF86AudioLowerVolume>", void $ lowerVolume 2)
        , ("<XF86AudioPlay>", spawn "rhythmbox-client --play-pause")
        , ("<XF86AudioStop>", spawn "rhythmbox-client --stop")
        , ("<XF86AudioPrev>", spawn "rhythmbox-client --previous")
        , ("<XF86AudioNext>", spawn "rhythmbox-client --next")

        , ("M-s", sshPrompt defaultXPConfig)
        , ("M-p", spawn "exec $(yeganesh -x)")
        , ("M-x", spawn "exec $(yeganesh -x)")
        , ("M-f m", maximizeNext >> manPrompt defaultXPConfig)
        ] ++ [("M-f "++k, Search.promptSearchBrowser defaultXPConfig "google-chrome" f) | (k,f) <- searchList]
          ++ [("M-S-f " ++ k, Search.selectSearchBrowser "google-chrome" f) | (k,f) <- searchList]
            where searchList :: [(String, Search.SearchEngine)]
                  searchList = [ ("g", Search.google)
                               , ("w", Search.wikipedia)
                               , ("a", Search.alpha)
                               , ("d", Search.dictionary)
                               , ("h", Search.hoogle)
                               , ("y", Search.youtube)
                               ]


swapScreens = do
    visible <- gets (S.visible . windowset)
    case visible of
        [] -> return ()
        s:_ -> windows $ S.greedyView (S.tag $ S.workspace s)

maximizeNext :: X ()
maximizeNext = manageNext (return True) $ ask >>= \w -> do
    liftX $ S.workspace . S.current <$> gets windowset
        >>= sendMessageWithNoRefresh (maximizeRestore w)
    idHook


runOnByClass :: FilePath -> [String] -> WorkspaceId -> X ()
runOnByClass prog classNames wk = nextToWorkspaceByClass classNames wk >> spawn prog

nextToWorkspaceByClass :: [String] -> WorkspaceId -> X ()
nextToWorkspaceByClass classNames wk =
    manageNext query (doF $ S.shift wk)
    where query = foldl1 (<||>) (map (className =?) classNames)

spawnLocalIShellCmdOn :: WorkspaceId -> String -> X ()
spawnLocalIShellCmdOn wk c = do
    nextToWorkspaceByClass ["Gnome-Terminal"] wk
    spawnLocalIShellCmd c

spawnLocalShell = XS.gets DTS.makeTopicConfig >>= TS.currentTopicDir >>= spawnShellIn
-- spawnShellIn dir = spawn $ "xterm -e 'cd \"" ++ dir ++ "\" && $SHELL'"

spawnLocalIShellCmd c = XS.gets DTS.makeTopicConfig >>= TS.currentTopicDir >>= (\d -> spawnShellCmd d c True)
spawnShellCmd :: FilePath -> String -> Bool -> X ()
spawnShellCmd dir cmd interactive = spawn $
    "gnome-terminal" ++
        (if null dir then ""
                     else " --working-directory=\"" ++ dir ++ "\"") ++
        (if null cmd
         then ""
         else if not interactive
              then " -- " ++ cmd
              else " -- zsh -c '" ++ cmd ++ "; zsh'")

spawnShellIn "" = spawn "gnome-terminal"
spawnShellIn dir = spawn $ "gnome-terminal --working-directory=\"" ++ dir ++ "\""
spawnIShellInCmd dir cmd = spawn $ "gnome-terminal --working-directory=\"" ++ dir ++ "\" -- zsh -c '" ++ cmd ++ "; zsh'"
spawnFilemanager = XS.gets DTS.makeTopicConfig >>= TS.currentTopicDir >>= spawnFilemanagerIn
spawnFilemanagerIn dir = spawn $ "nautilus " ++ dir


mouseBindings' (XConfig {XMonad.modMask = modMask}) = Data.Map.fromList
    [ ((modMask, button1), mouseMoveWindow)
    , ((modMask, button2), killWindow)
    , ((modMask, button3), mouseResizeWindow)
    , ((modMask, button4), const $ windows S.focusDown)
    , ((modMask, button5), const $ windows S.focusUp)
    , ((modMask .|. shiftMask, button4), const $ windows S.swapDown)
    , ((modMask .|. shiftMask, button5), const $ windows S.swapUp)
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
