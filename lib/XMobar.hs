{-# LANGUAGE DeriveDataTypeable #-}

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
import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as State

import System.IO (Handle, writeFile, readFile)
import System.Posix.Types (ProcessID)
-----------------------------------------------

data XMobarStorage = XMobarStorage (M.Map ScreenId XMobar) deriving Typeable
instance ExtensionClass XMobarStorage where
    initialValue = XMobarStorage M.empty

data XMobar = XMobar {
      xmPid :: ProcessID
    , xmPipes :: M.Map XMobarParameter Handle
    }
type Alias = String
data XMobarParameter = XMobarParameter Alias (ScreenId -> X String)
data XMobarConfig = XMobarConfig {
      xmParameters :: [XMobarParameter]
    , xmConfigFile :: FilePath
    }


defaultXmConfig = XMobarConfig
    { xmConfigFile = "/home/nadrieril/.xmonad/xmobarrc"
    , xmParameters =
        [ XMobarParameter "title" $ \id -> do
            maybe_scr <- lookupScreen id
            case maybe_scr >>= (S.stack . S.workspace) of
                Nothing -> return ""
                Just stk -> do
                    let focus = S.focus stk
                    namedwindow <- getName focus
                    return $ ppTitle customSBPP $ show namedwindow
        , XMobarParameter "workspaces" $ \id -> do
            sort' <- ppSort customSBPP
            winset <- gets windowset
            urgents <- readUrgents
            all_workspaces <- asks (workspaces . config)
            let sepBy sep = intercalate sep . filter (not . null)
            let visibles = map (S.tag . S.workspace) (S.visible winset)
            let fmt w = wrapSwitch $ printer customSBPP (S.tag w)
                 where printer | any (\x -> maybe False (== S.tag w) (S.findTag x winset)) urgents = ppUrgent
                               | S.tag w == S.currentTag winset = ppCurrent
                               | S.tag w `elem` visibles = ppVisible
                               | isJust (S.stack w) = ppHidden
                               | otherwise = ppHiddenNoWindows

                       i = elemIndex (S.tag w) all_workspaces
                       wrapSwitch s = case (s, i) of
                            ("", _) -> ""
                            (_, Nothing) -> s
                            (s, Just i) -> "<action=`"++action++"`>"++s++"</action>"
                                where action = "/home/nadrieril/.xmonad/send_xmonad_evt \"XMONAD_SWITCHWKSP\" "++show i
            return $ sepBy (ppWsSep customSBPP) . map fmt . sort' $ S.workspaces winset
        ]
    }

-- lookupScreen :: sid -> X (Maybe (S.Screen i l a sid sd))
lookupScreen id = do
    ws <- gets windowset
    return $ find (\(S.Screen _ sid _) -> sid == id) (S.screens ws)

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

        let writeParameterToPipe (XMobarParameter name f) = do
            value <- f (S.screen scr)
            io $ writeNamedPipe (dir++"/"++name) value

        foldr1 (>>) $ map writeParameterToPipe (xmParameters xmconf)

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
