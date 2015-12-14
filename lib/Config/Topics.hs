{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Topics (topicConfig, layout) where

import XMonad hiding (terminal)
import qualified XMonad.StackSet as W

import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Hooks.ManageHelpers (isDialog)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Reflect (reflectVert)
import XMonad.Layout.Grid
------------------------------------------------------
-- Custom libs
import XMonad.Actions.DynamicTopicSpace (Topic(..), fromList, defaultTopic, currentTopicDir)
import XMonad.Hooks.ManageNext (queryFromClasses, nextToWorkspaceByClass)
import qualified XMonad.Util.Terminal as Terminal
import Config.Common
------------------------------------------------------
full = noBorders Full
tiled = smartBorders (Tall 1 (3/100) (1/2))
mirrortiled = smartBorders (Mirror $ Tall 1 (3/100) (1/2))
topbar = smartBorders (reflectVert $ Mirror $ Tall 1 (3/100) (93/100))
-- accordion = smartBorders (Mirror (Tall 0 (3/100) (1/2)))
grid = smartBorders Grid

layout = full ||| tiled


topic = defaultTopic
devTopic = topic {
          topicLayout = Just $ Layout $ topbar ||| tiled ||| full
        , topicHook = (not <$> (isDialog <||> isAtom) --> doF avoidMaster)
                <+> (isAtom --> doF W.swapMaster)
    }
    where isAtom = queryFromClasses ["Atom"]
termTopic = topic {
    topicAction = const spawnLocalShell,
    topicLayout = Just $ Layout $ full ||| tiled ||| mirrortiled ||| grid
}


topicConfig = fromList $
    [ ("web", topic {
        topicAction = flip spawnOn "google-chrome",
        topicWindows = queryFromClasses ["Firefox","Google-chrome","Chromium","Chromium-browser"]
    })
    , ("irc", topic {
        topicAction = flip spawnOn "quasselclient",
        topicLayout = Just $ Layout $ mirrortiled ||| tiled ||| full
        -- topicWindows = queryFromClasses ["quasselclient"]
        --     <||> isInProperty "_NET_WM_NAME" "Quassel IRC"
        --     <||> isInProperty "WM_COMMAND" "quasselclient"
    })
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
            topicDir = "xmonad",
            topicLayout = Just $ Layout $ full ||| topbar ||| tiled
        })
        , ("taffy", topic {
            topicDir = "taffybar",
            topicLayout = Just $ Layout $ full ||| topbar ||| tiled
        })
        , ("bars", devTopic {
            topicDir = "bars",
            topicAction = const spawnLocalShell
        })
        , ("b-a", devTopic {
            topicDir = "bars-angular",
            topicAction = \wk -> do
                nextToWorkspaceByClass (Terminal.terminalClasses terminal) wk
                spawnLocalShellCmd "grunt serve"
        })
        , ("b-d", devTopic {
            topicDir = "bars-django",
            topicAction = const $ do
                spawnLocalShellCmd "python manage.py runserver_plus"
                -- spawnLocalTerminal "ssh -t srv@nadrieril 'cd /srv/bars/bars-django; $SHELL'"
                spawnLocalShell
        })
        , ("psc", devTopic {
            topicDir = "PSC"
        })
        , ("cv", topic {
            topicDir = "CV",
            topicAction = const $ do
                dir <- currentTopicDir
                spawn $ "xdg-open " ++ dir ++ "/CV.pdf"
        })
        , ("24h", devTopic {
            topicDir = "24hnatation",
            topicAction = const $ do
                spawnLocalShellCmd "cd server && python manage.py runserver_plus"
                spawnLocalShellCmd "cd client && grunt serve"
                spawnLocalShell
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
    , ("music", topic {
        topicDir = "$HOME/Music",
        topicAction = flip spawnOn "rhythmbox",
        topicWindows = queryFromClasses ["Rhythmbox", "ario"]
    })
    -- , ("term", termTopic)
    ] ++
    [ ([i], termTopic) | i <- ['0'..'5'] ]

    where
        projectTopic (w, t@Topic{topicDir = dir, topicAction = action}) =
            ("dev/"++w, t {
                  topicDir = "$HOME/projects/" ++ dir
                , topicAction = \wk -> do
                    runOnByClass ("cd -P ~/projects/" ++ dir ++ "; atom .") classes wk
                    action wk
            })
            where classes = ["Atom"]


avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) -> W.Stack t [r] rs
     _           -> c
