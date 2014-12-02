module XMobar (customXMobar, defaultXmConfig) where

import XMonad

import qualified XMonad.StackSet as S
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook)
import XMonad.Hooks.DynamicLog (PP(..), xmobarPP, xmobarColor, wrap, shorten)
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook (readUrgents)
import Data.Maybe (isJust, fromJust)
import Data.List (intercalate, find, elemIndex)

import qualified System.Posix.Files as Files
import qualified System.Posix.Signals as Signals
import qualified System.Directory
import Control.Monad (when, forM_)

import System.IO (Handle, writeFile, readFile)
-- import System.Posix.Types (ProcessID)

import qualified XMobar.Property as XMProperty
import XMobar.XMobar
------------------------------------------------------

defaultXmConfig = XMobarConfig
    { xmProperties =
        [ ("title", XMProperty.ptyTitle customSBPP)
        , ("workspaces", XMProperty.ptyWorkspaces customSBPP) ]
    }

customXMobar xmconf conf = conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
            logHook conf
            ws <- gets windowset
            forM_ (S.screens ws) update_screen_focused_window_title
        , startupHook = do
            startupHook conf
            ws <- gets windowset
            forM_ (S.screens ws) (spawnXMobar xmconf . screen_id)
        , manageHook = manageHook conf <+> manageDocks
        , handleEventHook = handleEventHook conf <+> docksEventHook
        }
    where
    screen_id scr = case show (S.screen scr) of
                    'S':' ':i -> i
                    _         -> "0"

    update_screen_focused_window_title scr = do
        let S.Screen wksp sid _ = scr
        let i = screen_id scr
        let dir = "/tmp/xmobar/"++i
        ws <- gets windowset

        let writePropertyToPipe (name, p) = do
            value <- p (S.screen scr)
            io $ writeNamedPipe (dir++"/"++name) value

        forM_ (xmProperties xmconf) writePropertyToPipe

customSBPP = xmobarPP
    { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
    , ppTitle = xmobarColor "green"  "" . shorten 100
    , ppHiddenNoWindows = xmobarColor "#6b6b6b" ""}


writeNamedPipe p s = writeFile p (s++"\n")
