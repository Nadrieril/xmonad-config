{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
import XMonad
import qualified XMonad.StackSet as S

import XMonad.Config.Gnome (gnomeConfig)
import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.Place (placeHook, simpleSmart)

import XMonad.Actions.OnScreen (onScreen, onScreen', Focus(FocusNew))
import XMonad.Actions.SpawnOn (manageSpawn)

import XMonad.Layout.Maximize (maximize)

import Control.Monad (liftM2, when)
import Data.List (find, intersect)
import Data.Maybe (isNothing)
import Data.Monoid (All(..))
------------------------------------------------------
-- Custom libs
import XMonad.Util.XMobar
import XMonad.Hooks.DocksFullscreen (docksFullscreenConfig)
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (manageManageNext)

import Config.Common (prevHiddenWS, nextHiddenWS)
import qualified Config.Common as Cfg (terminalCmd)
import qualified Config.Topics as Cfg (topicConfig, layout)
import qualified Config.Mappings as Cfg (keyMappings, mouseMappings)
------------------------------------------------------
main = xmonad'
    $ docksFullscreenConfig'
    $ customXMobar defaultXmConfig
    $ maximizeConfig'
    $ DTS.dynamicTopicsConfig Cfg.topicConfig
    $ pagerHints
    $ gnomeConfig {
          modMask = mod4Mask
        , terminal = Cfg.terminalCmd
        , startupHook = spawn "killall unclutter; unclutter"
        , layoutHook = Cfg.layout
        , manageHook = composeAll
            [ manageManageNext
            , manageSpawn
            , placeHook simpleSmart
            , manageHook gnomeConfig
            , manageHook' ]
        , handleEventHook = eventHook
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#004080"
        , mouseBindings = Cfg.mouseMappings
        , keys = Cfg.keyMappings
    }


xmonad' conf@XConfig{layoutHook = Layout a} = xmonad conf {layoutHook = a}

wrapLayout conf@XConfig{layoutHook = a} = conf{layoutHook = Layout a}
maximizeConfig' conf@XConfig{layoutHook = Layout a} = conf {layoutHook = Layout $ maximize a}
docksFullscreenConfig' conf@XConfig{layoutHook = Layout a} = wrapLayout $ docksFullscreenConfig conf{layoutHook = a}



manageHook' = composeAll $
       [appName  =? r --> doIgnore | r <- _ignored]
    ++ [className =? c --> doCenterFloat | c <- _floating ]

    where
        _floating  = ["Xmessage","Nm-connection-editor"]
        _ignored = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]


eventHook :: Event -> X All
eventHook ClientMessageEvent { ev_message_type = mt, ev_data = dt } = do
    ws <- gets windowset
    all_workspaces <- asks (workspaces . config)
    let open_workspaces = map S.tag (S.workspaces ws)
    let workspaces = all_workspaces `intersect` open_workspaces  -- keep order
    let n = fromIntegral (head dt)
    let wk = workspaces !! (n `div` 10) -- arbitrary limit to 10 screens
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
eventHook _ = return (All True)
