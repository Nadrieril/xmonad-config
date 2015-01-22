module XMonad.Util.Keys (azertyKeys, numpadKeys) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)

import qualified Data.Map
import Control.Monad (liftM2)
------------------------------------------------------
azertyKeys = mkKeys [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]

numpadKeys = mkKeys
    [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
    , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
    , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
    , xK_KP_Insert] -- 0


mkKeys l (XConfig {modMask = modm}) = Data.Map.fromList $
    ((modm, xK_semicolon), sendMessage (IncMasterN (-1))):
    [((m .|. modm, k), withNthWorkspace f i)
        | (i, k) <- zip [0..] l,
          (f, m) <- [(W.greedyView, 0), (liftM2 (.) W.view W.shift, shiftMask)]]
