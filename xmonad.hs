{-# LANGUAGE FlexibleContexts, ParallelListComp #-}
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import XMonad.Config.Xfce (xfceConfig)

import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Hooks.ManageDocks (docksEventHook)

import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Layout.Maximize (maximize)

import GHC.Exts (sortWith)
import qualified Data.Map
import Data.Maybe (isJust)
------------------------------------------------------
-- Custom libs
import XMonad.Hooks.LogApplet as LogApplet
import XMonad.Hooks.DocksFullscreen (docksFullscreenConfig)
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (manageManageNext)

import qualified Config.Common as Cfg (terminalCmd)
import qualified Config.Topics as Cfg (topicConfig, layout)
import qualified Config.Mappings as Cfg (keyMappings, mouseMappings)
-----------------------------------------------------------------------


-----------------------------------------------------------------------
main = do
  applet <- LogApplet.initApplet
  xmonad'
    $ docksFullscreenConfig'
    $ maximizeConfig'
    $ DTS.dynamicTopicsConfig Cfg.topicConfig
    $ docksFullscreenConfig
    $ xfceConfig
        { modMask            = mod4Mask
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#004080"
        , mouseBindings = Cfg.mouseMappings
        , keys = Cfg.keyMappings
        , terminal = Cfg.terminalCmd
        , layoutHook = Cfg.layout

        , manageHook = composeAll
            [ manageManageNext
            , manageSpawn
            , placeHook simpleSmart
            , manageHook xfceConfig
            , manageHook' ]
        , handleEventHook    = handleEventHook xfceConfig <+> docksEventHook
        , logHook            = logHook xfceConfig <+> appletlogHook applet
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
        -- _ignored = ["desktop", "desktop_window", "notify-osd", "xfce4-notifyd", "Xfce4-notifyd", "stalonetray", "trayer"]
        _ignored = ["notify-osd", "xfce4-notifyd", "Xfce4-notifyd", "stalonetray", "trayer"]

appletlogHook :: AppletPipe -> X ()
appletlogHook applet = do
  ws_sort <- getSortByIndex
  workspaces <- gets (map W.tag . ws_sort . W.workspaces . windowset)
  let workspaces_map = Data.Map.fromList [(w, show i ++ "." ++ w) | i <- [1::Integer ..] | w <- workspaces]
  let wtag = (Data.Map.!) workspaces_map . W.tag

  current <- gets (W.current . windowset)
  screens <- gets (W.screens . windowset)
  let sorted_screens = sortWith W.screen screens
  let logscreens = flip map sorted_screens $ \s ->
        let name = pangoSanitize $ wtag $ W.workspace s in
        if W.screen s == W.screen current then pangoColor "green" name else name

  hidden <- gets (W.hidden . windowset)
  let loghidden = flip map hidden $ \w ->
        let name = pangoSanitize $ wtag w in
        if isJust (W.stack w) then name else pangoColor "grey" name

  let out = wrap "[" "]" (unwords logscreens) ++ " " ++ unwords loghidden
  io $ LogApplet.outputApplet applet $ wrap "<b>" "</b>" out


pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
