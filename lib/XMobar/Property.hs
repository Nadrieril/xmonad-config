module XMobar.Property (Property, ptyTitle, ptyWorkspaces) where

import XMonad

import qualified XMonad.StackSet as S
import XMonad.Hooks.DynamicLog (PP(..))
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook (readUrgents)
import Data.Maybe (isJust)
import Data.List (intercalate, find, elemIndex)
------------------------------------------------------
type Property = ScreenId -> X String


lookupScreen :: ScreenId -> X (Maybe (S.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
lookupScreen id = do
    ws <- gets windowset
    return $ find (\(S.Screen _ sid _) -> sid == id) (S.screens ws)


ptyTitle :: PP -> Property
ptyTitle pp sid = do
    maybe_scr <- lookupScreen sid
    case maybe_scr >>= (S.stack . S.workspace) of
        Nothing -> return ""
        Just stk -> do
            let focus = S.focus stk
            namedwindow <- getName focus
            return $ ppTitle pp $ show namedwindow


ptyWorkspaces :: PP -> Property
ptyWorkspaces pp sid = do
    sort' <- ppSort pp
    winset <- gets windowset
    urgents <- readUrgents
    all_workspaces <- asks (workspaces . config)
    xmonadDir <- getXMonadDir
    let sepBy sep = intercalate sep . filter (not . null)
    let visibles = map (S.tag . S.workspace) (S.visible winset)
    let fmt w = wrapSwitch $ printer pp (S.tag w)
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
                        where action = xmonadDir++"/send_xmonad_evt \"XMONAD_SWITCHWKSP\" "++show i
    return $ sepBy (ppWsSep pp) . map fmt . sort' $ S.workspaces winset
