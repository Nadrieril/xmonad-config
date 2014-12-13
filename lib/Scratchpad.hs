module Scratchpad (scratchpadConfig, toggleScratchpad) where

import XMonad

import Control.Monad (liftM2, when)
import qualified XMonad.StackSet as W (current, view, hidden, shift, stack, tag, workspace, workspaces)
import Data.Maybe (isNothing)
import Data.List (find)
import XMonad.Util.Run (runInTerm)
------------------------------------------------------
scratchpadName = "term"

scratchpadConfig conf = conf
    {
    -- { logHook = do
    --     logHook conf
    --     ws <- gets windowset
    --     let crnt = W.workspace $ W.current ws
    --     when (W.tag crnt == scratchpadName && isNothing (W.stack crnt)) toggleScratchpad
      workspaces = workspaces conf ++ [scratchpadName]
    , manageHook = manageHook conf <+>
        composeAll [className =? c --> viewShift scratchpadName | c <- classes]
    }
    where classes = ["Gnome-terminal"]
          viewShift = doF . liftM2 (.) W.view W.shift


toggleScratchpad = do
    ws <- gets windowset
    let crnt = W.workspace $ W.current ws
    let scratchpad = find ((== scratchpadName) . W.tag) (W.workspaces ws)
    let switch = windows . W.view
    case () of
        () | W.tag crnt == scratchpadName -> switch $ W.tag $ head $ W.hidden ws
           | isNothing (W.stack =<< scratchpad) -> runInTerm "" "$SHELL"
           | otherwise -> switch scratchpadName
