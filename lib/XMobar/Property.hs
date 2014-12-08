module XMobar.Property (Property, ptyTitle, ptyWorkspaces) where

import XMonad

import qualified XMonad.StackSet as S
import XMonad.Hooks.DynamicLog (PP(..))
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook (readUrgents)
import Data.Maybe (isJust, fromJust)
import Data.List (intercalate, find, elemIndex)
------------------------------------------------------
type Property = ScreenId -> X String


lookupScreen :: ScreenId -> X (S.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
lookupScreen id = do
    ws <- gets windowset
    return $ fromJust $ find (\(S.Screen _ sid _) -> sid == id) (S.screens ws)


ptyTitle :: PP -> Property
ptyTitle pp sid = do
    scr <- lookupScreen sid
    xmonadDir <- getXMonadDir
    case S.stack $ S.workspace scr of
        Nothing -> return ""
        Just stk -> do
            let focus = S.focus stk
            namedwindow <- getName focus
            let title = ppTitle pp $ show namedwindow
            return $ wrapAction (sendEvtAction xmonadDir "KILLFOCUSED" "0") "2" title


ptyWorkspaces :: PP -> Property
ptyWorkspaces pp sid = do
    sort' <- ppSort pp
    winset <- gets windowset
    urgents <- readUrgents
    all_workspaces <- asks (workspaces . config)
    scr <- lookupScreen sid
    xmonadDir <- getXMonadDir
    let sepBy sep = intercalate sep . filter (not . null)
    let visibles = map (S.tag . S.workspace) (S.current winset : S.visible winset)
    let fmt w = wrapSwitch $ printer pp (S.tag w)
         where printer | any (\x -> maybe False (== S.tag w) (S.findTag x winset)) urgents = ppUrgent
                       | S.tag w == S.tag (S.workspace scr) = ppCurrent
                       | S.tag w `elem` visibles = ppVisible
                       | isJust (S.stack w) = ppHidden
                       | otherwise = ppHiddenNoWindows

               i = elemIndex (S.tag w) all_workspaces
               wrapSwitch s = case (s, i) of
                    ("", _) -> ""
                    (_, Nothing) -> s
                    (s, Just i) -> foldr actionTag s actionMap
                        where action name = sendEvtAction xmonadDir name (show i)
                              actionTag (b,a) = wrapAction (action a) b
                              actionMap = [("1", "SWITCHWKSP"), ("3", "SHIFTWKSP"), ("2", "KILLWKSP")]
    return $ sepBy (ppWsSep pp) . map fmt . sort' $ S.workspaces winset

wrapAction action button s =
    "<action=`"++action++"` button="++button++">"++s++"</action>"
sendEvtAction dir name param =
    dir++"/send_xmonad_evt \"XMONAD_"++name++"\" " ++ param
