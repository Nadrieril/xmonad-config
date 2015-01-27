{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Topics (topicConfig, layout) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Hooks.ManageHelpers (isDialog)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.Reflect (reflectVert)
-- import XMonad.Layout.Maximize (maximize)

import Control.Applicative ((<$>))
------------------------------------------------------
-- Custom libs
import XMonad.Actions.DynamicTopicSpace (Topic(..), fromList, defaultTopic)
import XMonad.Hooks.ManageNext (queryFromClasses)
import Config.Common
------------------------------------------------------
topic = defaultTopic
devTopic = topic {
          topicLayout = Just $ Layout $ topbar ||| tiled ||| full
        , topicHook = (not <$> (isDialog <||> isAtom) --> doF avoidMaster)
                <+> (isAtom --> doF W.swapMaster)
    }
    where isAtom = queryFromClasses ["Atom"]


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
        [ ("xm", devTopic {
            topicDir = "xmonad"
        })
        , ("b-a", devTopic {
            topicDir = "bars-angular",
            topicAction = const $ spawnLocalShellCmd "grunt serve"
        })
        , ("b-d", devTopic {
            topicDir = "bars-django",
            topicAction = const $ do
                spawnLocalShell
                spawnLocalTerminal "ssh -t srv@nadrieril 'cd /srv/bars/bars-django; $SHELL'"
                spawnLocalShellCmd "python manage.py runserver_plus"
        })
        , ("psc", devTopic {
            topicDir = "PSC"
        })
        , ("24h", devTopic {
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
        projectTopic (w, t@Topic{topicDir = dir, topicAction = action}) =
            ("dev/"++w, t {
                  topicDir = "$HOME/projects/" ++ dir
                , topicAction = \wk -> do
                    runOnByClass ("atom ~/projects/" ++ dir) classes wk
                    action wk
            })
            where classes = ["Atom"]



full = noBorders simpleTabbed
tiled = smartBorders (Tall 1 (3/100) (1/2))
topbar = smartBorders (reflectVert $ Mirror $ Tall 1 (3/100) (93/100))
doubletiled = smartBorders (Tall 2 (3/100) (1/2))
-- accordion = smartBorders (Mirror (Tall 0 (3/100) (1/2)))

layout = full ||| tiled


avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) -> W.Stack t [r] rs
     otherwise           -> c
