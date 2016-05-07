{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Topics (topicConfig, layout) where

import XMonad hiding (terminal)
import qualified XMonad.StackSet as W

import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Hooks.ManageHelpers (isDialog)

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Reflect (reflectVert)
import XMonad.Layout.Grid
import XMonad.Layout.MouseResizableTile
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


topic = defaultTopic {
    topicDir = "$HOME"
}
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
        topicAction = flip spawnOn "firefox"
        -- topicWindows = queryFromClasses ["Firefox","Google-chrome","Chromium","Chromium-browser"]
    })
    , ("irc", topic {
        -- topicAction = flip spawnOn "quasselclient",
        topicLayout = Just $ Layout $ full ||| mirrortiled ||| tiled
        -- topicWindows = queryFromClasses ["quasselclient"]
        --     <||> isInProperty "_NET_WM_NAME" "Quassel IRC"
        --     <||> isInProperty "WM_COMMAND" "quasselclient"
    })
    , ("wip", topic {
        topicAction = const spawnLocalShell
    })
    , ("scratch", termTopic)
    , ("game", topic)
    , ("video", topic {
        topicDir = "$HOME/Videos",
        topicAction = const spawnFilemanager,
        topicWindows = queryFromClasses ["Vlc"],
        topicLayout = Just $ Layout $ tiled ||| full
    })
    , ("downloads", topic {
        topicDir = "$HOME/Downloads",
        topicWindows = queryFromClasses ["Filezilla", "Deluge"],
        topicLayout = Just $ Layout $ full ||| mirrortiled ||| tiled
    })
    , ("security", topic {
        topicAction = flip spawnOn "keepass",
        topicWindows = queryFromClasses ["keepass2", "KeePass2"]
    })
    , ("git", topic {
        topicAction = flip spawnOn "smartgit",
        topicWindows = queryFromClasses ["SmartGit/Hg"]
    })
    , ("gimp", topic {
        topicAction = flip spawnOn "gimp",
        topicWindows = queryFromClasses ["gimp-2.8", "Gimp-2.8"],
        topicLayout = Just $ Layout $ (mouseResizableTile {nmaster=3, isMirrored=True}) ||| full
    })
    ] ++

    map projectTopic
        [ ("xm", topic {
            topicDir = "xmonad",
            topicLayout = Just $ Layout $ full ||| topbar ||| tiled
        })
        , ("cv", topic {
            topicDir = "cv"
        })
        ] ++

    [ ("backup", topic {
        topicAction = flip spawnOn "grsync"
    })
    , ("mail", topic {
        topicAction = flip spawnOn "thunderbird"
    })
    , ("music", topic {
        topicDir = "$HOME/Music",
        topicAction = flip spawnOn "amarok",
        topicWindows = queryFromClasses ["Amarok"]
    })
    -- , ("term", termTopic)
    ] ++
    [ ([i], termTopic) | i <- ['0'..'5'] ]

    where
        projectTopic (w, t@Topic{topicDir = dir, topicAction = action}) =
            ("wip/"++w, t {
                  topicDir = "$HOME/wip/" ++ dir
                , topicAction = \wk -> do
                    runOnByClass ("cd -P ~/wip/" ++ dir ++ "; atom .") classes wk
                    action wk
            })
            where classes = ["Atom"]


avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) -> W.Stack t [r] rs
     _           -> c
