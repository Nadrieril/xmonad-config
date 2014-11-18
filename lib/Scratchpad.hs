module Scratchpad (scratchpadConfig, toggleScratchpad) where

import XMonad

import Control.Monad (liftM2, when)
import qualified XMonad.StackSet as S
import Data.Maybe (isNothing)
import Data.List (find)
import XMonad.Util.Run (runInTerm)

------------------------------------------------------
scratchpadName = "term"

scratchpadConfig conf = conf
    { logHook = do
        logHook conf
        ws <- gets windowset
        let crnt = S.workspace $ S.current ws
        when (S.tag crnt == scratchpadName && isNothing (S.stack crnt)) toggleScratchpad
    , workspaces = workspaces conf ++ [scratchpadName]
    , manageHook = manageHook conf <+>
        composeAll [className =? c --> viewShift scratchpadName | c <- classes]
    }
    where classes = ["Gnome-terminal"]
          viewShift = doF . liftM2 (.) S.greedyView S.shift


toggleScratchpad = do
    ws <- gets windowset
    let crnt = S.workspace $ S.current ws
    let scratchpad = find ((== scratchpadName) . S.tag) (S.workspaces ws)
    let switch = windows . S.greedyView
    case () of
        () | S.tag crnt == scratchpadName -> switch $ S.tag $ head $ S.hidden ws
           | isNothing (S.stack =<< scratchpad) -> runInTerm "" "$SHELL"
           | otherwise -> switch scratchpadName
