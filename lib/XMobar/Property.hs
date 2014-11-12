module XMobar.Property (Property(..), ptyTitle, ptyWorkspaces) where

import XMonad

import qualified XMonad.StackSet as S
import XMonad.Hooks.DynamicLog (PP(..))
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook (readUrgents)
import Data.Maybe (isJust)
import Data.List (intercalate, find, elemIndex)
import Data.Map ((!))
import qualified System.Posix.Files as Files

import XMobar.XMobar (XMobar(..), Alias)
------------------------------------------------------

data Property = Property Alias (ScreenId -> X String)

lookupScreen :: ScreenId -> X (Maybe (S.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
lookupScreen id = do
    ws <- gets windowset
    return $ find (\(S.Screen _ sid _) -> sid == id) (S.screens ws)


initProperty :: XMobar -> Property -> X ()
initProperty xmobar (Property alias _) = io $ Files.createNamedPipe (xmPipes xmobar ! alias) pipe_mode
    where pipe_mode = Files.unionFileModes (Files.unionFileModes Files.namedPipeMode Files.ownerModes) (Files.unionFileModes Files.groupReadMode Files.otherReadMode)

writeNamedPipe :: FilePath -> String -> IO ()
writeNamedPipe p s = writeFile p (s++"\n")

writeProperty :: XMobar -> Property -> X ()
writeProperty xmobar (Property alias f) = do
    value <- f (xmScreen xmobar)
    io $ writeNamedPipe (xmPipes xmobar ! alias) value



ptyTitle :: Alias -> PP -> Property
ptyTitle name pp = Property name $ \sid -> do
    maybe_scr <- lookupScreen sid
    case maybe_scr >>= (S.stack . S.workspace) of
        Nothing -> return ""
        Just stk -> do
            let focus = S.focus stk
            namedwindow <- getName focus
            return $ ppTitle pp $ show namedwindow

ptyWorkspaces :: Alias -> PP -> Property
ptyWorkspaces name pp = Property name $ \sid -> do
    sort' <- ppSort pp
    winset <- gets windowset
    urgents <- readUrgents
    all_workspaces <- asks (workspaces . config)
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
                        where action = "/home/nadrieril/.xmonad/send_xmonad_evt \"XMONAD_SWITCHWKSP\" "++show i
    return $ sepBy (ppWsSep pp) . map fmt . sort' $ S.workspaces winset
