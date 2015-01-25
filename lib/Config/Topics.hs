{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Topics (topicConfig, layout) where

import XMonad

import XMonad.Actions.SpawnOn (spawnOn)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Tabbed (simpleTabbed)
-- import XMonad.Layout.Maximize (maximize)
------------------------------------------------------
-- Custom libs
import XMonad.Actions.DynamicTopicSpace (Topic(..), fromList, defaultTopic)
import XMonad.Hooks.ManageNext (queryFromClasses)
import Config.Common
------------------------------------------------------
topic = defaultTopic

topicConfig = fromList $
    [ ("web", topic {
        topicAction = flip spawnOn "google-chrome",
        topicWindows = queryFromClasses ["Firefox","Google-chrome","Chromium","Chromium-browser"]
    })
    , ("main", topic)
    , ("game", topic)
    , ("video", topic {
        topicDir = "$HOME/Videos",
        topicAction = const spawnFilemanager,
        topicWindows = queryFromClasses ["Vlc"]
    })
    ] ++

    [ ("dev", topic {
        topicDir = "$HOME/projects"
    })
    , ("dev/java", topic {
        topicDir = "$HOME/projects/java",
        topicAction = flip spawnOn "eclipse",
        topicWindows = queryFromClasses ["Eclipse"]
    })
    ] ++ map projectTopic
        [ ("xm", topic {
            topicDir = "xmonad"
        })
        , ("b-a", topic {
            topicDir = "bars-angular",
            topicAction = const $ spawnLocalShellCmd "grunt serve",
            topicLayout = Just $ Layout $ topbar ||| full
        })
        , ("b-d", topic {
            topicDir = "bars-django",
            topicAction = const $ do
                spawnLocalShellCmd "python manage.py runserver_plus"
                spawnLocalShellCmd "ssh -t srv@nadrieril 'cd /srv/bars/bars-django; $SHELL'"
                spawnLocalShell,
            topicLayout = Just $ Layout $ topbar2 ||| full
        })
        , ("psc", topic {
            topicDir = "PSC"
        })
        , ("24h", topic {
            topicDir = "24hnatation"
        })
        ] ++

    [ ("backup", topic {
        topicAction = flip spawnOn "grsync"
    })
    , ("mail", topic {
        topicAction = flip spawnOn "icedove"
    })
    , ("git", topic {
        topicAction = flip spawnOn "smartgithg",
        topicWindows = queryFromClasses ["SmartGit/Hg"]
    })
    , ("irc", topic {
        topicAction = flip spawnOn "quasselclient"
        -- topicWindows = queryFromClasses ["quasselclient"]
        --     <||> isInProperty "_NET_WM_NAME" "Quassel IRC"
        --     <||> isInProperty "WM_COMMAND" "quasselclient"
    })
    , ("music", topic {
        topicDir = "$HOME/Music",
        topicAction = flip spawnOn "rhythmbox",
        topicWindows = queryFromClasses ["Rhythmbox", "ario"]
    })
    , ("term", topic {
        topicAction = const spawnLocalShell,
        topicLayout = Just $ Layout $ doubletiled ||| full ||| tiled
    })] ++
    [ ([i], topic) | i <- ['0'..'5'] ]

    where
        projectTopic (w, t@Topic{topicDir = dir, topicAction = action}) = ("dev/"++w, t {
                topicDir = "$HOME/projects/" ++ dir,
                topicAction = \wk -> runOnByClass ("atom ~/projects/" ++ dir) classes wk >> action wk
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
