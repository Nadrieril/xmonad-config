{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module XMonad.Actions.DynamicTopicSpace
    ( Topic(..)
    , TopicConfig
    , defaultTopic
    , fromList
    , dynamicTopicsConfig
    , defaultTopicConfig
    , makeTopicConfig
    , withLayout

    , goto
    , currentTopicAction
    , currentTopicDir
    , topicPrompt
    , topicGridSelect
    , clearWorkspace
    , removeWorkspace
    ) where

import XMonad hiding (defaultConfig)

import XMonad.Operations (killWindow)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as S
import qualified XMonad.Actions.TopicSpace as TS
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Util.Dmenu (dmenu)
import XMonad.Actions.GridSelect (gridselect, navNSearch, buildDefaultGSConfig, GSConfig(..))
import XMonad.Layout.PerWorkspace (onWorkspace)

import Data.Typeable (Typeable)
import qualified Data.Map as M
import qualified Safe
import Control.Monad (liftM2, unless, forM_)
import Data.List (find, partition)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Control.Arrow (second)
------------------------------------------------------
data Topic = Topic
    { topicDir :: FilePath
    , topicAction :: WorkspaceId -> X ()
    , topicWindows :: Query Bool
    , topicLayout :: Maybe (Layout Window)
    }

defaultTopic = Topic "" (const $ return ()) (return False) Nothing

withLayout :: (Read (l Window), LayoutClass l Window) => Topic -> l Window -> Topic
withLayout t l = t {topicLayout = Just $ Layout l}


data TopicConfig = TopicConfig
    { topicsMap :: M.Map WorkspaceId StoredTopic
    , topicsNames :: [WorkspaceId]
    , topicsManageHook :: ManageHook
    , topicsLayout :: Layout Window -> Layout Window
    }

defaultTopicConfig = TopicConfig M.empty [] idHook id


fromList :: [(WorkspaceId, Topic)] -> TopicConfig
fromList topics =
    TopicConfig
        { topicsMap = M.fromList $ map (second storeTopic) topics
        , topicsNames = map fst topics
        , topicsManageHook = composeAll
               [q --> shiftToWk wk | (wk, q) <- map (second topicWindows) topics]
        , topicsLayout = \l -> foldr (\(ws,Layout lay) (Layout z) -> Layout (onWorkspace ws lay z)) l layoutsMap
        }
    where
        layoutsMap :: [(WorkspaceId, Layout Window)]
        layoutsMap = map (second (fromJust . topicLayout)) $ filter (\(_, t) -> isJust $ topicLayout t) topics

        shiftToWk :: WorkspaceId -> ManageHook
        shiftToWk wk = do
            isNew <- liftX $ gets (not . S.tagMember wk . windowset)
            liftX $ addHiddenWorkspace wk
            if isNew
                then doF $ liftM2 (.) S.view S.shift wk
                else doShift wk


dynamicTopicsConfig :: (Read (l Window), LayoutClass l Window) => TopicConfig -> XConfig l -> XConfig Layout
dynamicTopicsConfig tc conf = conf
    { startupHook = do
        XS.put $ TopicStorage $ topicsMap tc
        windows $ \s -> s { S.hidden = filter hasWindows (S.hidden s) }
        ws <- gets windowset
        tstc <- XS.gets makeTopicConfig
        forM_ (map S.workspace $ S.current ws : S.visible ws)
            (\wk -> unless (hasWindows wk) $ TS.topicAction tstc (S.tag wk))
        startupHook conf
    , layoutHook = topicsLayout tc (Layout $ layoutHook conf)
    , manageHook = manageHook conf <+> topicsManageHook tc
    , workspaces = topicsNames tc
    }
    where
        hasWindows = isJust . S.stack


goto :: WorkspaceId -> X ()
goto w = do
    addHiddenWorkspace w
    tc <- XS.gets makeTopicConfig
    TS.switchTopic tc w

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
        hasTag w' = (==w') . S.tag . S.workspace


currentTopicAction :: X ()
currentTopicAction = XS.gets makeTopicConfig >>= TS.currentTopicAction

currentTopicDir :: X FilePath
currentTopicDir = XS.gets makeTopicConfig >>= TS.currentTopicDir


topicPrompt :: X (Maybe WorkspaceId)
topicPrompt = do
    wks <- asks (workspaces . config)
    -- selection <- menuArgs "yeganesh" ["-f", "-p", "topic"] wks
    selection <- dmenu wks
    return $ if null selection
                then Nothing
                else Just selection

topicGridSelect :: X (Maybe WorkspaceId)
topicGridSelect = do
        let cfg = (buildDefaultGSConfig colorizer) { gs_navigate = navNSearch }
        wks <- asks (workspaces . config)
        gridselect cfg (map (\a -> (a,a)) wks)
    where colorizer _ active =
            if active
            then return ("#B0B0B0", "#000000")
            else return ("#808080", "#000000")


---- Internals
data StoredTopic = StoredTopic
    { storedTopicDir :: FilePath
    , storedTopicAction :: WorkspaceId -> X ()
    }

storeTopic t = StoredTopic (topicDir t) (topicAction t)


data TopicStorage = TopicStorage (M.Map WorkspaceId StoredTopic)
    deriving Typeable

instance ExtensionClass TopicStorage where
    initialValue = TopicStorage M.empty



makeTopicConfig :: TopicStorage -> TS.TopicConfig
makeTopicConfig (TopicStorage topics) = TS.defaultTopicConfig
    { TS.topicDirs = M.filter (not . null) $ M.map storedTopicDir topics
    , TS.topicActions = M.mapWithKey (flip storedTopicAction) topics
    -- , TS.defaultTopic = fromMaybe "1" $ Safe.headMay (topicsNames tc)
    , TS.defaultTopicAction = const $ return ()
    }
