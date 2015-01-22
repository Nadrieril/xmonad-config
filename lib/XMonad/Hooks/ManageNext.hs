{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Hooks.ManageNext
    ( manageNext
    , manageManageNext
    , queryFromClasses
    , nextToWorkspace
    , nextToWorkspaceByClass
    ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
------------------------------------------------------
type NextQuery = (Query Bool, ManageHook)

data QueryStorage = QueryStorage [NextQuery]
    deriving Typeable

instance ExtensionClass QueryStorage where
    initialValue = QueryStorage []

manageNext :: Query Bool -> ManageHook -> X ()
manageNext q mh = XS.modify (\(QueryStorage l) -> QueryStorage $ l ++ [(q, mh)])


manageManageNext :: ManageHook
manageManageNext = ask >>= \w -> liftX $ do
    QueryStorage qs <- XS.get
    (mh, newqs) <- handle w qs
    XS.put $ QueryStorage newqs
    runQuery mh w
    where
        handle :: Window -> [NextQuery] -> X (ManageHook, [NextQuery])
        handle w qs = do
            (ret, newqs) <- extractM (\(q, _) -> runQuery q w) qs
            return (maybe idHook snd ret, newqs)


extractM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a, [a])
extractM f [] = return (Nothing, [])
extractM f (x:q) = do
    match <- f x
    if match
        then return (Just x, q)
        else do
            (mb, q') <- extractM f q
            return (mb, x:q')


queryFromClasses :: [String] -> Query Bool
queryFromClasses classNames = foldl1 (<||>) (map (className =?) classNames)

nextToWorkspace :: WorkspaceId -> Query Bool -> X ()
nextToWorkspace wk q = manageNext q (doF $ W.shift wk)

nextToWorkspaceByClass :: [String] -> WorkspaceId -> X ()
nextToWorkspaceByClass classNames wk = nextToWorkspace wk (queryFromClasses classNames)
