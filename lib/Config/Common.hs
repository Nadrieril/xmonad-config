{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Common where

import XMonad hiding (terminal)
import qualified XMonad.StackSet as W
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Actions.CycleWS (findWorkspace, WSType(HiddenWS), Direction1D(Next))
------------------------------------------------------
-- Custom libs
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (nextToWorkspaceByClass)
import qualified XMonad.Util.Terminal as Terminal (terminalCmd)
import XMonad.Util.Terminal (terminalClasses, spawnTerminal, gnomeTerminal)
------------------------------------------------------
terminal = gnomeTerminal
terminalCmd = Terminal.terminalCmd terminal

runOnByClass :: FilePath -> [String] -> WorkspaceId -> X ()
runOnByClass prog classNames wk = nextToWorkspaceByClass classNames wk >> spawn prog

spawnLocalTerminal_ c i = DTS.currentTopicDir >>= (\d -> spawnTerminal terminal d c i)

spawnLocalTerminal c = spawnLocalTerminal_ c False
spawnLocalShell = spawnLocalShellCmd ""
spawnLocalShellCmd c = spawnLocalTerminal_ c True
spawnLocalShellCmdOn wk c = do
    nextToWorkspaceByClass (terminalClasses terminal) wk
    spawnLocalShellCmd c

spawnFilemanager = DTS.currentTopicDir >>= spawnFilemanagerIn
spawnFilemanagerIn dir = spawn $ "nautilus " ++ dir


hiddenWsBy = findWorkspace getSortByIndex Next HiddenWS

prevHiddenWS = switchHiddenWorkspace (-1)
nextHiddenWS = switchHiddenWorkspace 1
switchHiddenWorkspace d = windows . W.view =<< hiddenWsBy d

shiftToPrevHidden = shiftHiddenWorkspace (-1)
shiftToNextHidden = shiftHiddenWorkspace 1
shiftHiddenWorkspace d = windows . W.shift =<< hiddenWsBy d
