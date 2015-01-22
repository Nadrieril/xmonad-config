{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Topics (topicConfig, layout) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as S
import XMonad.Actions.SpawnOn (spawnOn)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspaces)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.Maximize (maximize)
------------------------------------------------------
-- Custom libs
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (manageNext, queryFromClasses, nextToWorkspaceByClass)
import XMonad.Util.Terminal (terminalClasses, spawnTerminal, gnomeTerminal)
------------------------------------------------------
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
        DTS.topicAction = flip spawnOn "eclipse",
        DTS.topicWindows = queryFromClasses ["Eclipse"]
    })
    ] ++ projecttopics
        [ ("xm",  "xmonad", return ())
        , ("b-a", "bars-angular", spawnLocalITerminal "grunt serve")
        , ("b-d", "bars-django", do
            spawnLocalITerminal "python manage.py runserver_plus"
            spawnLocalITerminal "ssh -t srv@nadrieril 'cd /srv/bars/bars-django; $SHELL'"
            spawnLocalTerminal)
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
        DTS.topicAction = const spawnLocalTerminal
    })
    ] ++ [ (show i, topic) | i <- [0..5] ]
    where projecttopics l = do
            (n, p, a) <- l
            return ("dev/"++n, topic {
                DTS.topicDir = "$HOME/projects/"++p,
                DTS.topicAction = \wk -> runOnByClass ("atom ~/projects/"++p) classes wk >> a
            })
            where classes = ["Atom"]



layout = maximize $
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


------------------------------------------------------
runOnByClass :: FilePath -> [String] -> WorkspaceId -> X ()
runOnByClass prog classNames wk = nextToWorkspaceByClass classNames wk >> spawn prog

spawnLocalTerminal = spawnLocalITerminal ""
spawnLocalITerminal c = DTS.currentTopicDir >>= (\d -> spawnTerminal gnomeTerminal d c True)
spawnLocalITerminalOn wk c = do
    nextToWorkspaceByClass (terminalClasses gnomeTerminal) wk
    spawnLocalITerminal c

spawnFilemanager = DTS.currentTopicDir >>= spawnFilemanagerIn
spawnFilemanagerIn dir = spawn $ "nautilus " ++ dir
