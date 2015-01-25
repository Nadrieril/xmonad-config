{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Topics (topicConfig, layout) where

import XMonad

import XMonad.Actions.SpawnOn (spawnOn)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspaces)
import XMonad.Layout.Tabbed (simpleTabbed)
-- import XMonad.Layout.Maximize (maximize)
------------------------------------------------------
-- Custom libs
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (queryFromClasses)
import Config.Common
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
    ] ++ map projectTopic
        [ ("xm", topic {
            DTS.topicDir = "xmonad"
        })
        , ("b-a", topic {
            DTS.topicDir = "bars-angular",
            DTS.topicAction = const $ spawnLocalShellCmd "grunt serve",
            DTS.topicLayout = Just $ Layout $ topbar ||| full
        })
        , ("b-d", topic {
            DTS.topicDir = "bars-django",
            DTS.topicAction = const $ do
                spawnLocalShellCmd "python manage.py runserver_plus"
                spawnLocalShellCmd "ssh -t srv@nadrieril 'cd /srv/bars/bars-django; $SHELL'"
                spawnLocalShell,
            DTS.topicLayout = Just $ Layout $ topbar2 ||| full
        })
        , ("psc", topic {
            DTS.topicDir = "PSC"
        })
        , ("24h", topic {
            DTS.topicDir = "24hnatation"
        })
        ] ++

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
        DTS.topicAction = const spawnLocalShell,
        DTS.topicLayout = Just $ Layout $ doubletiled ||| full ||| tiled
    })] ++
    [ ([i], topic) | i <- ['0'..'5'] ]

    where
        projectTopic (w, t@DTS.Topic{DTS.topicDir = dir, DTS.topicAction = action}) = ("dev/"++w, t {
                DTS.topicDir = "$HOME/projects/" ++ dir,
                DTS.topicAction = \wk -> runOnByClass ("atom ~/projects/" ++ dir) classes wk >> action wk
            })
            where classes = ["Atom"]

        full = noBorders simpleTabbed
        tiled = smartBorders (Tall 1 (3/100) (1/2))
        topbar = smartBorders (Mirror $ Tall 1 (3/100) (7/100))
        topbar2 = smartBorders (Mirror $ Tall 2 (3/100) (7/100))
        doubletiled = smartBorders (Tall 2 (3/100) (1/2))
        -- accordion = smartBorders (Mirror (Tall 0 (3/100) (1/2)))


layout = full ||| tiled
    where
        full = noBorders simpleTabbed
        tiled = smartBorders (Tall 1 (3/100) (1/2))
