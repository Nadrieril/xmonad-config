{-# LANGUAGE DeriveDataTypeable #-}
module DynamicTopicSpace
    ( Topic(..)
    , fromList
    , dynamicTopicsConfig
    , defaultConfig
    , makeTopicConfig

    , goto
    , topicPrompt
    , clearWorkspace
    , removeWorkspace
    ) where

import XMonad
    ( X(..)
    , XConf(..)
    , XConfig(..)
    , ExtensionClass(..)
    , WorkspaceId
    , Query
    , spawn
    , workspaces
    , asks
    , gets
    , windowset
    , windows)

import XMonad.Operations (killWindow)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as S
import qualified XMonad.Actions.TopicSpace as TS
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Util.Dmenu (dmenu)

import Data.Typeable (Typeable)
import qualified Data.Map as M
import qualified Safe
import Control.Monad (liftM, unless, when, forM_)
import Data.List (find, partition)
import Data.Maybe (mapMaybe, isJust, fromMaybe, fromJust)
import Control.Arrow (second)
------------------------------------------------------
data Topic = Topic
    { topicDir :: Maybe FilePath
    , topicAction :: Maybe (X ())
    , topicWindows :: Maybe (Query Bool)
    }

data StoredTopic = StoredTopic
    { storedTopicDir :: Maybe FilePath
    , storedTopicAction :: Maybe (X ())
    }
storeTopic t = StoredTopic (topicDir t) (topicAction t)

data TopicStorage = TopicStorage (M.Map WorkspaceId StoredTopic)
    deriving Typeable

instance ExtensionClass TopicStorage where
    initialValue = TopicStorage M.empty

data TopicConfig = TopicConfig
    { topics :: M.Map WorkspaceId StoredTopic
    , topicNames :: [WorkspaceId]
    }

defaultConfig = TopicConfig M.empty []

fromList l =
    let topics = map (\(n,d,a) -> (n, Topic d a Nothing)) l in
    TopicConfig
        { topics = M.fromList $ map (second storeTopic) topics
        , topicNames = map fst topics
        }

dynamicTopicsConfig tc conf = conf
    { startupHook = do
        XS.put $ TopicStorage $ topics tc
        windows $ \s -> s { S.hidden = filter hasWindows (S.hidden s) }
        ws <- gets windowset
        tstc <- XS.gets makeTopicConfig
        forM_ (map S.workspace $ S.current ws : S.visible ws)
            (\wk -> unless (hasWindows wk) $ TS.topicAction tstc (S.tag wk))
        startupHook conf
    , workspaces = topicNames tc
    }
    where
        hasWindows = isJust . S.stack


makeTopicConfig :: TopicStorage -> TS.TopicConfig
makeTopicConfig (TopicStorage topics) = TS.defaultTopicConfig
    { TS.topicDirs = M.mapMaybe storedTopicDir topics
    , TS.topicActions = M.mapMaybe storedTopicAction topics
    -- , TS.defaultTopic = fromMaybe "1" $ Safe.headMay (topicNames tc)
    , TS.defaultTopicAction = const $ return ()
    }


goto w = do
    addHiddenWorkspace w
    tc <- XS.gets makeTopicConfig
    TS.switchTopic tc w


topicPrompt :: (String -> X ()) -> X ()
topicPrompt f = do
    wks <- asks (workspaces . config)
    -- selection <- menuArgs "yeganesh" ["-f", "-p", "topic"] wks
    selection <- dmenu wks
    unless (null selection) $ f selection


clearWorkspace :: WorkspaceId -> X ()
clearWorkspace wk = do
    wks <- gets (S.workspaces . windowset)
    case find ((== wk). S.tag) wks of
        Nothing -> return ()
        Just x -> forM_ (S.integrate' $ S.stack x) killWindow


removeWorkspace :: WorkspaceId -> X ()
removeWorkspace w = windows $ \s ->
    let removed_from_visible = do
        let visibles = S.current s : S.visible s
        scr_cont_w <- find (hasTag w) visibles
        wk_to_show <- Safe.headMay $ S.hidden s
        nhidden <- Safe.tailMay $ S.hidden s
        let wk_to_remove = S.workspace scr_cont_w
        let replacemt_wk = wk_to_show {S.stack = merge (S.stack wk_to_remove) (S.stack wk_to_show)}
        let replacemt_scr = scr_cont_w {S.workspace = replacemt_wk}
        let nvisibles = map (\scr -> if hasTag w scr then replacemt_scr else scr) visibles
        return s { S.current = head nvisibles
                 , S.visible = tail nvisibles
                 , S.hidden = nhidden }
    in
    let cur = S.current s
        (wks_to_remove, nhidden) = partition ((==w) . S.tag) $ S.hidden s
        freed_stcks = map S.stack wks_to_remove
        new_stk = foldr merge (S.stack (S.workspace cur)) freed_stcks
        removed_from_hidden = s { S.current = cur {S.workspace = (S.workspace cur) {S.stack = new_stk}}
                                , S.hidden = nhidden}
    in fromMaybe removed_from_hidden removed_from_visible
    where
        merge x y = S.differentiate (S.integrate' x ++ S.integrate' y)
        hasTag w = (==w) . S.tag . S.workspace
