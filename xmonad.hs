{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
import XMonad
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Config.Azerty (azertyKeys)

import XMonad.Hooks.ManageHelpers (doCenterFloat, isInProperty)
import XMonad.Hooks.Place (placeHook, simpleSmart)

import XMonad.Layout.Maximize (maximizeRestore)

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
import Data.Monoid (All(..), mconcat, mempty)
import Safe (headMay)
------------------------------------------------------
-- Custom libs
import XMonad.Util.XMobar
import XMonad.Hooks.DocksFullscreen
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (manageNext, manageManageNext)

import Config.Topics (topicConfig, layout)
import Config.Mappings (keyMappings, mouseMappings)
------------------------------------------------------

main = xmonad
    $ docksFullscreenConfig
    $ customXMobar defaultXmConfig
    $ DTS.dynamicTopicsConfig topicConfig
    $ gnomeConfig {
          modMask = mod4Mask
        , terminal = "gnome-terminal.wrapper"
        , startupHook = spawn "killall unclutter; unclutter"
        , layoutHook = layout
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
        , mouseBindings = mouseMappings
        , keys = keyMappings <+> keys defaultConfig
        }



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


hiddenWsBy = findWorkspace getSortByIndex Next HiddenWS

prevHiddenWS = switchHiddenWorkspace (-1)
nextHiddenWS = switchHiddenWorkspace 1
switchHiddenWorkspace d = windows . S.view =<< hiddenWsBy d

shiftToPrevHidden = shiftHiddenWorkspace (-1)
shiftToNextHidden = shiftHiddenWorkspace 1
shiftHiddenWorkspace d = windows . S.shift =<< hiddenWsBy d
