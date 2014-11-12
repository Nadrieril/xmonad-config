{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module DocksFullscreen (docksFullscreenConfig) where

import XMonad

import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Hooks.ManageDocks (SetStruts(..))
import XMonad.Layout.LayoutModifier (LayoutModifier(..), ModifiedLayout(..))
import Data.List (delete, nub)


docksFullscreenConfig conf = conf
    { layoutHook = fullscreenToggleStruts $ FS.fullscreenFocus $ layoutHook conf
    , manageHook = FS.fullscreenManageHook <+> manageHook conf
    , handleEventHook = FS.fullscreenEventHook <+> handleEventHook conf
    }


fullscreenToggleStruts = ModifiedLayout $ FullscreenToggleStruts []
data FullscreenToggleStruts a = FullscreenToggleStruts [a]
     deriving (Read, Show)
instance LayoutModifier FullscreenToggleStruts Window where
    handleMess ff@(FullscreenToggleStruts fulls) m = case fromMessage m of
        Just (FS.AddFullscreen win) -> setStruts $ nub $ win:fulls
        Just (FS.RemoveFullscreen win) -> setStruts $ delete win fulls
        Just FS.FullscreenChanged -> return $ Just ff
        _ -> return Nothing
        where setStruts f = do
                let m = if null f
                        then SetStruts [minBound .. maxBound] []
                        else SetStruts [] [minBound .. maxBound]
                sendMessage m
                return $ Just $ FullscreenToggleStruts f
