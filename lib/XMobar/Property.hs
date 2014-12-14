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
    return $ fromJust $ find ((== id) . S.screen) (S.screens ws)


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
            return $ wrapEvtActions xmonadDir title
                [ ("2", ("KILLFOCUSED", show $ fromIntegral sid))
                , ("4", ("FOCUSUP", show $ fromIntegral sid))
                , ("5", ("FOCUSDOWN", show $ fromIntegral sid)) ]


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
                    (s, Just i) -> wrapEvtActions xmonadDir s
                        [ ("1", ("SWITCHWKSP", show n))
                        , ("2", ("KILLWKSP", show n))
                        , ("3", ("SHIFTWKSP", show n)) ]
                        where n = 10 * fromIntegral i + fromIntegral sid

    let ret = sepBy (ppWsSep pp) . map fmt . sort' $ S.workspaces winset
    return $ wrapEvtActions xmonadDir ret
        [ ("4", ("PREVWKSP", show $ fromIntegral sid))
        , ("5", ("NEXTWKSP", show $ fromIntegral sid)) ]


wrapAction :: String -> String -> String -> String
wrapAction action button s =
    "<action=`"++action++"` button="++button++">"++s++"</action>"

sendEvtAction :: FilePath -> String -> String -> String
sendEvtAction dir name param =
    dir++"/send_xmonad_evt \"XMONAD_"++name++"\" " ++ param

wrapEvtAction :: FilePath -> String -> String -> String -> String -> String
wrapEvtAction dir name param = wrapAction (sendEvtAction dir name param)

wrapEvtActions :: FilePath -> String -> [(String, (String, String))] -> String
wrapEvtActions dir = foldr (\(b,(a,p)) -> wrapEvtAction dir a p b)
