module XMobar (customXMobar, defaultXmConfig) where

import XMonad

import qualified XMonad.StackSet as S
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook)
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook (readUrgents)
import Data.Maybe (isJust, fromJust)
import Data.List (intercalate, find, elemIndex)

import qualified System.Posix.Files as Files
import qualified System.Posix.Signals as Signals
import qualified System.Directory
import Control.Monad (when)

import System.IO (Handle, writeFile, readFile)
-- import System.Posix.Types (ProcessID)

import qualified XMobar.Property as XMProperty
------------------------------------------------------

data XMobarConfig = XMobarConfig {
      xmProperties :: [XMProperty.Property]
    , xmConfigFile :: FilePath
    }


defaultXmConfig = XMobarConfig
    { xmConfigFile = "/home/nadrieril/.xmonad/xmobarrc"
    , xmProperties =
        [ XMProperty.ptyTitle "title" customSBPP
        , XMProperty.ptyWorkspaces "workspaces" customSBPP ]
    }

customXMobar xmconf conf = return $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
                logHook conf
                ws <- gets windowset
                foldr1 (>>) $ map update_screen_focused_window_title (S.screens ws)
        , startupHook = do
                startupHook conf
                ws <- gets windowset
                foldr1 (>>) $ map (spawnXMobar xmconf . screen_id) $ S.screens ws
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

        let writePropertyToPipe (XMProperty.Property name f) = do
            value <- f (S.screen scr)
            io $ writeNamedPipe (dir++"/"++name) value

        foldr1 (>>) $ map writePropertyToPipe (xmProperties xmconf)

customSBPP = xmobarPP
    { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
    , ppTitle = xmobarColor "green"  "" . shorten 100 }


writeNamedPipe p s = writeFile p (s++"\n")

killXMobar i = do
        let dir = "/tmp/xmobar/"++i

        direxists <- System.Directory.doesDirectoryExist dir
        when direxists $ do
            pidexists <- System.Directory.doesFileExist (dir++"/pid")
            when pidexists $ do
                pid <- readFile (dir++"/pid")
                catchIO $ Signals.signalProcess Signals.sigTERM (read pid)
            System.Directory.removeDirectoryRecursive dir


spawnXMobar xmconf i = do
        let dir = "/tmp/xmobar/"++i

        io $ do
            killXMobar i
            System.Directory.createDirectoryIfMissing True dir
            Files.createNamedPipe (dir++"/title") pipe_mode
            Files.createNamedPipe (dir++"/workspaces") pipe_mode

        xmonad_dir <- getXMonadDir
        spawn $ "xmobar "++xmConfigFile xmconf++" -x "++i++" -C \"["
                ++"Run PipeReader \\\":"++dir++"/title\\\" \\\"focusedname\\\""
                ++", Run PipeReader \\\":"++dir++"/workspaces\\\" \\\"workspaces\\\""
                ++"]\" & echo $! > "++dir++"/pid;"
    where pipe_mode = Files.unionFileModes (Files.unionFileModes Files.namedPipeMode Files.ownerModes) (Files.unionFileModes Files.groupReadMode Files.otherReadMode)
