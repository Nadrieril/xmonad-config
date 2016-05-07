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

import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as S
import qualified XMonad.Actions.TopicSpace as TS
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Util.Dmenu (dmenu)
import qualified XMonad.Actions.GridSelect as GS
import XMonad.Layout.PerWorkspace (onWorkspace)

import qualified Data.Map as M
import qualified Safe
import Control.Monad (liftM2, unless, forM, forM_)
import Data.List (find, partition)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.Monoid (Endo(..))
import Control.Arrow (second)
------------------------------------------------------
data Topic = Topic
    { topicDir :: FilePath  -- Topic related directory
    , topicAction :: WorkspaceId -> X ()  -- Action to launch when creating topic
    , topicWindows :: Query Bool  -- Windows related to topic; will get moved to the topic when spawned, creating the topic if necessary
    , topicLayout :: Maybe (Layout Window)  -- Nothing for default layout, Just l for per-topic layout
    , topicHook :: ManageHook  -- Per-topic managehook
    }

defaultTopic = Topic "" (const $ return ()) (return False) Nothing idHook

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
        , topicsManageHook = composeAll . reverse $
               [q --> shiftToWk wk | (wk, q) <- map (second topicWindows) topics]
            ++ [workspaceQuery $ map (second topicHook) topics]
        , topicsLayout = \l -> foldr (\(ws, Layout lay) (Layout z) -> Layout (onWorkspace ws lay z)) l layoutsMap
        }
    where
        -- Execute the hook for the topic in which the window is
        workspaceQuery :: [(WorkspaceId, ManageHook)] -> ManageHook
        workspaceQuery l = ask >>= \w -> do
            hooks <- forM l $ \(wk, hook) -> do
                (Endo res) <- hook
                return (wk, inWksp wk res)
            doF $ \ws -> (fromMaybe id $ S.findTag w ws >>= flip lookup hooks) ws
            where -- Execute hook in correct workspace
                  inWksp :: WorkspaceId -> (WindowSet -> WindowSet) -> WindowSet -> WindowSet
                  inWksp wk f ws =
                    let crnt = S.tag . S.workspace . S.current $ ws
                    in if crnt == wk
                        then f ws
                        else S.view crnt . f . S.view wk $ ws

        layoutsMap :: [(WorkspaceId, Layout Window)]
        layoutsMap = map (second (fromJust . topicLayout)) $ filter (\(_, t) -> isJust $ topicLayout t) topics

        shiftToWk :: WorkspaceId -> ManageHook
        shiftToWk wk = do
            -- isNew <- liftX $ gets (not . S.tagMember wk . windowset)
            liftX $ addHiddenWorkspace wk
            doF $ liftM2 (.) S.view S.shift wk


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
    , manageHook = topicsManageHook tc <+> manageHook conf
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

navNSearch :: GS.TwoD a (Maybe a)
navNSearch = GS.makeXEventhandler $ GS.shadowWithKeymap navNSearchKeyMap navNSearchDefaultHandler
  where navNSearchKeyMap = M.fromList [
           ((0,xK_Escape)     , GS.cancel)
          ,((0,xK_Return)     , GS.select)
          ,((0,xK_KP_Enter)   , GS.select)
          ,((0,xK_Left)       , GS.move (-1,0) >> navNSearch)
          ,((0,xK_Right)      , GS.move (1,0) >> navNSearch)
          ,((0,xK_Down)       , GS.move (0,1) >> navNSearch)
          ,((0,xK_Up)         , GS.move (0,-1) >> navNSearch)
          ,((0,xK_Tab)        , GS.moveNext >> navNSearch)
          ,((shiftMask,xK_Tab), GS.movePrev >> navNSearch)
          ,((0,xK_BackSpace), GS.transformSearchString (\s -> if s == "" then "" else init s) >> navNSearch)
          ]
        -- The navigation handler ignores unknown key symbols, therefore we const
        navNSearchDefaultHandler (_,s,_) = do
          GS.transformSearchString (++ s)
          navNSearch

topicGridSelect :: X (Maybe WorkspaceId)
topicGridSelect = do
        let cfg = (GS.buildDefaultGSConfig colorizer) { GS.gs_navigate = navNSearch }
        wks <- asks (workspaces . config)
        GS.gridselect cfg (map (\a -> (a,a)) wks)
    where colorizer _ active = return $
            if active
            then ("#B0B0B0", "#000000")
            else ("#808080", "#000000")


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
