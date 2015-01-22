module Config.Mappings (keyMappings, mouseMappings) where

import XMonad
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Util.EZConfig (mkKeymap)

import XMonad.Hooks.ManageHelpers (doCenterFloat, isInProperty)
import XMonad.Hooks.Place (placeHook, simpleSmart)

import XMonad.Layout.Maximize (maximizeRestore)

import XMonad.Actions.Volume (lowerVolume, raiseVolume, toggleMute)
import XMonad.Actions.OnScreen (onScreen, onScreen', Focus(..))
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, toggleWS, findWorkspace, WSType(..), Direction1D(..))
import qualified XMonad.Actions.TopicSpace as TS
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import XMonad.Actions.Warp (warpToWindow)
-- import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import qualified XMonad.Actions.Search as Search

import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Prompt.Man (manPrompt)

import Control.Monad (liftM2, when, forM_, void)
import Control.Applicative ((<$>))
import qualified Data.Map
import Data.List (delete, nub, partition, find)
import Data.Maybe (fromJust, listToMaybe, maybeToList, isNothing, isJust, maybe)
import Data.Monoid (All(..), mconcat, mempty)
import Safe (headMay)
------------------------------------------------------
-- Custom libs
import XMonad.Util.XMobar
import XMonad.Hooks.DocksFullscreen
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (manageNext, manageManageNext)
import XMonad.Util.Keys (azertyKeys, numpadKeys)
------------------------------------------------------
keyMappings = azertyKeys <+> numpadKeys <+> flip mkKeymap keys'

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
            wk <- gets (S.currentTag . windowset)
            DTS.clearWorkspace wk
            DTS.removeWorkspace wk)

        , ("M1-C-t", spawnLocalShell)
        , ("M-n", spawnFilemanager)

        , ("M1-<Tab>", windows S.focusDown)
        , ("M1-S-<Tab>", windows S.focusUp)
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
        , ("M-p", spawn "exec $(yeganesh -x)")
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




mouseMappings (XConfig {XMonad.modMask = modMask}) = Data.Map.fromList
    [ ((modMask, button1), mouseMoveWindow)
    , ((modMask, button2), killWindow)
    , ((modMask, button3), mouseResizeWindow)
    , ((modMask, button4), const $ windows S.focusDown)
    , ((modMask, button5), const $ windows S.focusUp)
    , ((modMask .|. shiftMask, button4), const $ windows S.swapDown)
    , ((modMask .|. shiftMask, button5), const $ windows S.swapUp)
    , ((modMask, 8), const prevHiddenWS)
    , ((modMask, 9), const nextHiddenWS)
    , ((modMask .|. shiftMask, 8), const $ shiftToPrevHidden >> prevHiddenWS)
    , ((modMask .|. shiftMask, 9), const $ shiftToNextHidden >> nextHiddenWS)
    ]


------------------------------------------------------

swapScreens = do
    visible <- gets (S.visible . windowset)
    case visible of
        [] -> return ()
        s:_ -> windows $ S.greedyView (S.tag $ S.workspace s)

maximizeNext :: X ()
maximizeNext = manageNext (return True) $ ask >>= \w -> do
    liftX $ S.workspace . S.current <$> gets windowset
        >>= sendMessageWithNoRefresh (maximizeRestore w)
    idHook


runOnByClass :: FilePath -> [String] -> WorkspaceId -> X ()
runOnByClass prog classNames wk = nextToWorkspaceByClass classNames wk >> spawn prog

nextToWorkspaceByClass :: [String] -> WorkspaceId -> X ()
nextToWorkspaceByClass classNames wk =
    manageNext query (doF $ S.shift wk)
    where query = foldl1 (<||>) (map (className =?) classNames)

spawnLocalIShellCmdOn :: WorkspaceId -> String -> X ()
spawnLocalIShellCmdOn wk c = do
    nextToWorkspaceByClass ["Gnome-Terminal"] wk
    spawnLocalIShellCmd c

spawnLocalShell = XS.gets DTS.makeTopicConfig >>= TS.currentTopicDir >>= spawnShellIn
-- spawnShellIn dir = spawn $ "xterm -e 'cd \"" ++ dir ++ "\" && $SHELL'"

spawnLocalIShellCmd c = XS.gets DTS.makeTopicConfig >>= TS.currentTopicDir >>= (\d -> spawnShellCmd d c True)
spawnShellCmd :: FilePath -> String -> Bool -> X ()
spawnShellCmd dir cmd interactive = spawn $
    "gnome-terminal" ++
        (if null dir then ""
                     else " --working-directory=\"" ++ dir ++ "\"") ++
        (if null cmd
         then ""
         else if not interactive
              then " -- " ++ cmd
              else " -- zsh -c '" ++ cmd ++ "; zsh'")

spawnShellIn "" = spawn "gnome-terminal"
spawnShellIn dir = spawn $ "gnome-terminal --working-directory=\"" ++ dir ++ "\""
spawnIShellInCmd dir cmd = spawn $ "gnome-terminal --working-directory=\"" ++ dir ++ "\" -- zsh -c '" ++ cmd ++ "; zsh'"
spawnFilemanager = XS.gets DTS.makeTopicConfig >>= TS.currentTopicDir >>= spawnFilemanagerIn
spawnFilemanagerIn dir = spawn $ "nautilus " ++ dir



hiddenWsBy = findWorkspace getSortByIndex Next HiddenWS

prevHiddenWS = switchHiddenWorkspace (-1)
nextHiddenWS = switchHiddenWorkspace 1
switchHiddenWorkspace d = windows . S.view =<< hiddenWsBy d

shiftToPrevHidden = shiftHiddenWorkspace (-1)
shiftToNextHidden = shiftHiddenWorkspace 1
shiftHiddenWorkspace d = windows . S.shift =<< hiddenWsBy d
