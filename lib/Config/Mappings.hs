{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config.Mappings (keyMappings, mouseMappings) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig (mkKeymap)

import XMonad.Layout.Maximize (maximizeRestore)

import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, prevScreen, shiftPrevScreen, swapNextScreen, swapPrevScreen, toggleWS)
-- import XMonad.Actions.CycleRecentWS (cycleRecentWS)
import XMonad.Actions.Warp (warpToWindow)
import qualified XMonad.Actions.Search as Search

import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Prompt.Man (manPrompt)

import GHC.Exts (sortWith)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Control.Concurrent (forkIO)
import Control.Monad (void, (>=>))
import qualified Data.Map
------------------------------------------------------
-- Custom libs
import qualified XMonad.Actions.DynamicTopicSpace as DTS
import XMonad.Hooks.ManageNext (manageNext)
import XMonad.Util.Keys (azertyKeys, numpadKeys)
import Config.Common
import XMonad.Actions.XMonadDisplay(myGridSelect)
------------------------------------------------------
keyMappings = flip mkKeymap keys'' <+> logMappings (screenKeys <+> azertyKeys <+> numpadKeys)

screenKeys :: XConfig Layout -> Data.Map.Map (KeyMask, KeySym) (X ())
screenKeys XConfig {XMonad.modMask = modMask} = Data.Map.fromList
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

keys'' = map (\(m, x) -> (m, logMapping m >> x)) keys'


keys' = [ ("M-q", spawn "xmonad --restart")
        , ("M-S-q", spawn "xfce4-session-logout")
        , ("M-S-l", spawn "xflock4")


        -- Screen navigation
        , ("M-C-<Tab>", nextScreen >> warpToWindow 0.9 0.9)
        , ("M-C-S-<Tab>", shiftNextScreen >> nextScreen >> warpToWindow 0.9 0.9)
        , ("M-M1-<Tab>", swapScreens)
        , ("M-<Two_Superior>", nextScreen >> warpToWindow 0.9 0.9)
        , ("M-S-<Two_Superior>", shiftNextScreen >> nextScreen >> warpToWindow 0.9 0.9)
        , ("M-M1-<Two_Superior>", swapScreens)

        , ("M-<R>", nextScreen >> warpToWindow 0.7 0.7)
        , ("M-<L>", prevScreen >> warpToWindow 0.7 0.7)
        , ("M-S-<R>", shiftNextScreen >> nextScreen >> warpToWindow 0.7 0.7)
        -- , ("M-S-<R>", shiftNextScreen)
        , ("M-S-<L>", shiftPrevScreen >> prevScreen >> warpToWindow 0.7 0.7)
        -- , ("M-S-<L>", shiftPrevScreen)
        , ("M-C-<R>", swapNextScreen >> nextScreen >> warpToWindow 0.7 0.7)
        -- , ("M-C-<R>", swapNextScreen)
        , ("M-C-<L>", swapPrevScreen >> prevScreen >> warpToWindow 0.7 0.7)
        -- , ("M-C-<L>", swapPrevScreen)


        -- Workspace navigation
        , ("M-<U>", nextHiddenWS)
        , ("M-<D>", prevHiddenWS)
        , ("M-S-<U>", shiftToNextHidden >> nextHiddenWS)
        , ("M-S-<D>", shiftToPrevHidden >> prevHiddenWS)
        -- , ("M-<Tab>", toggleWS)
        , ("M-<Tab>", cycleRecentHiddenWS [xK_Super_L] xK_Tab xK_grave)
        , ("M-S-<Tab>", do
            w <- gets $ W.peek . windowset
            toggleWS
            wk <- gets $ W.currentTag . windowset
            maybe (return ()) (windows . W.shiftWin wk) w)


        -- Topics
        , ("M-w", DTS.topicGridSelect >>= maybe (return ()) DTS.goto)
        , ("M-v", myGridSelect)
        , ("M-M1-w", DTS.currentTopicAction)
        , ("M-S-w", do
            wk <- gets (W.currentTag . windowset)
            DTS.clearWorkspace wk
            DTS.removeWorkspace wk)


        -- Layout
        , ("M-,", sendMessage (IncMasterN 1))
        , ("M-;", sendMessage (IncMasterN (-1)))
        , ("M-<Space>", sendMessage NextLayout)
        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)


        -- Window navigation
        , ("M1-<Tab>", windows W.focusDown)
        , ("M1-S-<Tab>", windows W.focusUp)
        , ("M-j", windows W.focusUp)
        , ("M-k", windows W.focusDown)
        , ("M-m", windows W.focusMaster)
        , ("M-S-j", windows W.swapUp)
        , ("M-S-k", windows W.swapDown)
        , ("M-S-m", windows W.swapMaster)

        , ("M-<Esc>", withFocused (sendMessage . maximizeRestore))
        , ("M-t", withFocused $ windows . W.sink)
        , ("M-c", kill)
        , ("M1-<F4>", kill)


        -- Apps
        , ("M1-C-t", spawnLocalShell)
        , ("M-<Return>", maximizeNext >> spawnLocalShell)
        , ("M-<KP_Enter>", fromJust $ lookup "M-<Return>" keys')
        , ("M-n", spawnFilemanager)
        , ("<XF86Calculator>", maximizeNext >> spawnLocalTerminal "ghci")

        -- Prompts
        , ("M-s", sshPrompt defaultXPConfig)
        -- , ("M-p", spawn "exec $(yeganesh -x)")
        -- , ("M-x", spawn "exec $(yeganesh -x)")
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


mouseMappings XConfig {XMonad.modMask = modMask} = Data.Map.fromList $
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

-- nextScreen = do
--     current <- gets (W.current . windowset)
--     screens <- gets (W.screens . windowset)
--     let sorted_screens = sortWith W.screen screens
--     let Just cid = elemIndex (W.screen current) (fmap W.screen sorted_screens)
--     let next = (cid + 1) `mod` length screens
--     windows $ W.greedyView (W.tag $ W.workspace (sorted_screens !! next))

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
