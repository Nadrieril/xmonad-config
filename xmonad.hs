{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
import XMonad
import XMonad.Hooks.DynamicLog

import XMonad.Config.Xfce (xfceConfig)

import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Hooks.ManageDocks (docksEventHook)

import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Layout.Maximize (maximize)

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
        , logHook            = logHook xfceConfig <+> dynamicLogWithPP (prettyPrinter applet)
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

prettyPrinter :: LogApplet.AppletPipe -> PP
prettyPrinter applet = defaultPP
    { ppOutput   = LogApplet.outputApplet applet . wrap "<b>" "</b>"
    , ppTitle    = const ""
    , ppCurrent  = wrap "[" "]" . pangoColor "green" . pangoSanitize
    , ppVisible  = wrap "(" ")" . pangoSanitize
    , ppHidden   = pangoColor "grey" . pangoSanitize
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }


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
