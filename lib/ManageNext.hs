{-# LANGUAGE DeriveDataTypeable #-}
module ManageNext (manageNext, manageManageNext) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

import Data.Monoid (mempty)
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
    QueryStorage nexts <- XS.get
    (mh, newl) <- handle w nexts
    XS.put $ QueryStorage newl
    runQuery mh w
    where
        handle :: Window -> [NextQuery] -> X (ManageHook, [NextQuery])
        handle w ((q, mh):t) = do
            match <- runQuery q w
            if match
                then return (mh, t)
                else do
                    (mh, rest) <- handle w t
                    return (mh, (q, mh):rest)
        handle w [] = return (mempty, [])
