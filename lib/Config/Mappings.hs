{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Mappings (keyMappings, mouseMappings) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig (mkKeymap)

import XMonad.Layout.Maximize (maximizeRestore)

import XMonad.Actions.Volume (lowerVolume, raiseVolume, toggleMute)
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, toggleWS)
import XMonad.Actions.Warp (warpToWindow)
import qualified XMonad.Actions.Search as Search

import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Prompt.Man (manPrompt)

import Control.Concurrent (forkIO)
import System.IO (appendFile)
import Control.Monad (void, (>=>))
import Control.Applicative ((<$>))
import qualified Data.Map
------------------------------------------------------
-- Custom libs
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (manageNext)
import XMonad.Util.Keys (azertyKeys, numpadKeys)
import Config.Common
------------------------------------------------------
keyMappings = flip mkKeymap keys'' <+> logMappings (azertyKeys <+> numpadKeys <+> defaultKeys)

keys'' = map (\(m, x) -> (m, logMapping m >> x)) keys'
keys' = [ ("M-S-q", spawn "gnome-session-quit")
        , ("M-S-l", spawn "gnome-screensaver-command -l")

        , ("M-<U>", nextHiddenWS)
        , ("M-<D>", prevHiddenWS)
        , ("M-S-<U>", shiftToNextHidden >> nextHiddenWS)
        , ("M-S-<D>", shiftToPrevHidden >> prevHiddenWS)
        , ("M-<Tab>", toggleWS)

        , ("M-C-<Tab>", nextScreen >> warpToWindow 0.9 0.9)
        , ("M-C-S-<Tab>", shiftNextScreen >> nextScreen >> warpToWindow 0.9 0.9)
        , ("M-M1-<Tab>", swapScreens)

        , ("M-w", DTS.topicGridSelect >>= maybe (return ()) DTS.goto)
        , ("M-M1-w", DTS.currentTopicAction)
        , ("M-S-w", do
            wk <- gets (W.currentTag . windowset)
            DTS.clearWorkspace wk
            DTS.removeWorkspace wk)

        , ("M1-C-t", spawnLocalTerminal)
        , ("M-n", spawnFilemanager)

        , ("M1-<Tab>", windows W.focusDown)
        , ("M1-S-<Tab>", windows W.focusUp)
        , ("M-<Esc>", withFocused (sendMessage . maximizeRestore))
        , ("M-c", kill)
        , ("M1-<F4>", kill)

        , ("<XF86AudioMute>", void toggleMute)
        , ("<XF86AudioRaiseVolume>", void $ raiseVolume 2)
        , ("<XF86AudioLowerVolume>", void $ lowerVolume 2)
        , ("<XF86AudioPlay>", spawn "rhythmbox-client --play-pause")
        , ("<XF86AudioStop>", spawn $
                "getsong(){ rhythmbox-client --print-playing-format=$1;};" ++
                "notify-send \"$(getsong %tt)\" \"by $(getsong %ta) from $(getsong %at)\"")
        , ("<XF86AudioPrev>", spawn "rhythmbox-client --previous")
        , ("<XF86AudioNext>", spawn "rhythmbox-client --next")

        , ("M-s", sshPrompt defaultXPConfig)
        -- , ("M-p", spawn "exec $(yeganesh -x)")
        , ("M-x", spawn "exec $(yeganesh -x)")
        , ("M-f m", maximizeNext >> manPrompt defaultXPConfig)
        ] ++ [("M-f "++k, Search.promptSearchBrowser defaultXPConfig "google-chrome" f) | (k,f) <- searchList]
          ++ [("M-S-f " ++ k, Search.selectSearchBrowser "google-chrome" f) | (k,f) <- searchList]
            where searchList :: [(String, Search.SearchEngine)]
                  searchList = [ ("g", Search.google)
                               , ("w", Search.wikipedia)
                               , ("a", Search.alpha)
                               , ("d", Search.dictionary)
                               , ("h", Search.hoogle)
                               , ("y", Search.youtube)
                               ]




mouseMappings (XConfig {XMonad.modMask = modMask}) = Data.Map.fromList $
    map (\(m, x) -> (m, x >=> const (logMapping m)))
    [ ((modMask, button1), mouseMoveWindow)
    , ((modMask, button2), killWindow)
    , ((modMask, button3), mouseResizeWindow)
    , ((modMask, button4), const $ windows W.focusDown)
    , ((modMask, button5), const $ windows W.focusUp)
    , ((modMask .|. shiftMask, button4), const $ windows W.swapDown)
    , ((modMask .|. shiftMask, button5), const $ windows W.swapUp)
    , ((modMask, 8), const prevHiddenWS)
    , ((modMask, 9), const nextHiddenWS)
    , ((modMask .|. shiftMask, 8), const $ shiftToPrevHidden >> prevHiddenWS)
    , ((modMask .|. shiftMask, 9), const $ shiftToNextHidden >> nextHiddenWS)
    ]


------------------------------------------------------
swapScreens = do
    visible <- gets (W.visible . windowset)
    case visible of
        [] -> return ()
        s:_ -> windows $ W.greedyView (W.tag $ W.workspace s)

maximizeNext :: X ()
maximizeNext = manageNext (return True) $ ask >>= \w -> do
    liftX $ W.workspace . W.current <$> gets windowset
        >>= sendMessageWithNoRefresh (maximizeRestore w)
    idHook

logMapping :: Show a => a -> X ()
logMapping mapping = io $ void $ forkIO $
    appendFile "/home/nadrieril/.xmonad/mappings.log" (show mapping ++ "\n")

logMappings :: (XConfig l -> Data.Map.Map (KeyMask, KeySym) (X ())) -> XConfig l -> Data.Map.Map (KeyMask, KeySym) (X ())
logMappings mappings cfg = Data.Map.mapWithKey (\k x -> x >> logMapping k) $ mappings cfg
