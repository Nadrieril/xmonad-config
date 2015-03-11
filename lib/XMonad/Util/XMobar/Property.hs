module XMonad.Util.XMobar.Property (Property, ptyTitle, ptyWorkspaces) where

import XMonad

import qualified XMonad.StackSet as S
import XMonad.Hooks.DynamicLog (PP(..))
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook (readUrgents)

import Data.Maybe (isJust, fromJust)
import Data.List (intercalate, find, elemIndex, intersect)
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
            let scrNb = fromIntegral sid :: Int
            return $ wrapEvtActions xmonadDir title
                [ ("2", ("KILLFOCUSED", show scrNb))
                , ("4", ("FOCUSUP", show scrNb))
                , ("5", ("FOCUSDOWN", show scrNb)) ]


ptyWorkspaces :: PP -> Property
ptyWorkspaces pp sid = do
    sort' <- ppSort pp
    ws <- gets windowset
    urgents <- readUrgents
    all_workspaces <- asks (workspaces . config)
    let open_workspaces = map S.tag (S.workspaces ws)
    let workspaces = all_workspaces `intersect` open_workspaces  -- keep order
    scr <- lookupScreen sid
    xmonadDir <- getXMonadDir
    let sepBy sep = intercalate sep . filter (not . null)
    let visibles = map (S.tag . S.workspace) (S.current ws : S.visible ws)
    let fmt i w = wrapSwitch $ printer pp (show i ++ "." ++ S.tag w)
         where printer | any (\x -> maybe False (== S.tag w) (S.findTag x ws)) urgents = ppUrgent
                       | S.tag w == S.tag (S.workspace scr) = ppCurrent
                       | S.tag w `elem` visibles = ppVisible
                       | isJust (S.stack w) = ppHidden
                       | otherwise = ppHiddenNoWindows

               wrapSwitch "" = ""
               wrapSwitch s = case elemIndex (S.tag w) workspaces of
                    Nothing -> s
                    Just i -> wrapEvtActions xmonadDir s
                        [ ("1", ("SWITCHWKSP", show n))
                        , ("2", ("KILLWKSP", show n))
                        , ("3", ("SHIFTWKSP", show n)) ]
                        where n = 10 * fromIntegral i + fromIntegral sid :: Int

    let ret = sepBy (ppWsSep pp) . zipWith fmt [(1::Int)..] . sort' $ S.workspaces ws
    let scrNb = fromIntegral sid :: Int
    return $ wrapEvtActions xmonadDir ret
        [ ("4", ("PREVWKSP", show scrNb))
        , ("5", ("NEXTWKSP", show scrNb)) ]


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
