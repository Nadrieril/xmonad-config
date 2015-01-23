module XMonad.Util.Terminal (terminalCmd, terminalClasses, spawnTerminal, gnomeTerminal) where

import XMonad (X, spawn)

import Control.Monad (unless)
import Control.Monad.Writer (execWriter, tell)
------------------------------------------------------
data Terminal = Terminal {
        terminalCmd :: String,
        terminalClasses :: [String],
        spawnTerminal :: FilePath -> String -> Bool -> X ()
    }

gnomeTerminal = Terminal {
        terminalCmd = "gnome-terminal.wrapper",
        terminalClasses = ["Gnome-Terminal"],
        spawnTerminal = spawnTerminal'
    }
    where spawnTerminal' dir cmd i = spawn $ execWriter $ do
            tell "gnome-terminal"
            unless (null dir) $ tell $ " --working-directory=\"" ++ dir ++ "\""
            unless (null cmd) $ do
                tell " -- "
                tell $ if i
                    then "$SHELL -c '" ++ cmd ++ "; $SHELL'"
                    else cmd
