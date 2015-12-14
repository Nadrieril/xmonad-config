{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}

module XMonad.Hooks.DocksFullscreen (docksFullscreenConfig, avoidStrutsUnlessFullscreen, ifFullscreen) where

import XMonad
import qualified XMonad.StackSet as W (focus, Workspace(..))

import qualified XMonad.Layout.Fullscreen as FS

import XMonad.Hooks.ManageHelpers (isFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts)

import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Arrow (second)
------------------------------------------------------
docksFullscreenConfig conf = conf
    { layoutHook = avoidStrutsUnlessFullscreen $ FS.fullscreenFocus $ layoutHook conf
    , manageHook = FS.fullscreenManageHook <+> manageHook conf
    , handleEventHook = FS.fullscreenEventHook <+> handleEventHook conf
    }


avoidStrutsUnlessFullscreen l = ifFullscreen l (avoidStruts l)

ifFullscreen :: (LayoutClass l a, LayoutClass r a) => l a -> r a -> IfFullscreen l r a
ifFullscreen = IfFullscreen

data IfFullscreen l r a = IfFullscreen (l a) (r a) deriving (Read, Show)
instance (LayoutClass l Window, LayoutClass r Window) => LayoutClass (IfFullscreen l r) Window where
    runLayout (W.Workspace i (IfFullscreen l r) stk) rect = do
        isFull <- case stk of
            Just s -> runQuery isFullscreen (W.focus s)
            Nothing -> return False

        if isFull
        then mapLayout (flip IfFullscreen r) <$> runLayout (W.Workspace i l stk) rect
        else mapLayout (IfFullscreen l)      <$> runLayout (W.Workspace i r stk) rect

      where mapLayout = second . fmap

    handleMessage (IfFullscreen l r) m = do
      newl <- fromMaybe l <$> handleMessage l m
      newr <- fromMaybe r <$> handleMessage r m
      return $ Just $ IfFullscreen newl newr
