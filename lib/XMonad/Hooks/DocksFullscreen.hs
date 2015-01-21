{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module XMonad.Hooks.DocksFullscreen (docksFullscreenConfig, avoidStrutsUnlessFullscreen) where

import XMonad

import qualified XMonad.Layout.Fullscreen as FS
import qualified XMonad.StackSet as W (focus, stack)
import Control.Monad (when)
import XMonad.Hooks.ManageHelpers (isFullscreen)
import XMonad.Hooks.ManageDocks (SetStruts(..), ToggleStruts(..), calcGap)
import XMonad.Layout.Gaps (Direction2D(..))
import XMonad.Layout.LayoutModifier (LayoutModifier(..), ModifiedLayout(..))
import qualified Data.Set as S


docksFullscreenConfig conf = conf
    { layoutHook = avoidStrutsUnlessFullscreen $ FS.fullscreenFocus $ layoutHook conf
    , manageHook = FS.fullscreenManageHook <+> manageHook conf
    , handleEventHook = FS.fullscreenEventHook <+> handleEventHook conf
    }


avoidStrutsUnlessFullscreen = ModifiedLayout $ AvoidStrutsUnlessFullscreen $ S.fromList [U,D,L,R]
data AvoidStrutsUnlessFullscreen a = AvoidStrutsUnlessFullscreen (S.Set Direction2D)
     deriving (Read, Show)
instance LayoutModifier AvoidStrutsUnlessFullscreen Window where
    modifyLayout (AvoidStrutsUnlessFullscreen ss) w r = do
        let maybestack = W.stack w
        isFull <- case maybestack of
            Just s -> runQuery isFullscreen (W.focus s)
            Nothing -> return False

        if isFull
            then runLayout w r
            else do
                nr <- fmap ($ r) (calcGap ss)
                runLayout w nr

    pureMess (AvoidStrutsUnlessFullscreen ss) m
        | Just ToggleStruts    <- fromMessage m = Just $ AvoidStrutsUnlessFullscreen (toggleAll ss)
        | Just (ToggleStrut s) <- fromMessage m = Just $ AvoidStrutsUnlessFullscreen (toggleOne s ss)
        | Just (SetStruts n k) <- fromMessage m
        , let newSS = S.fromList n `S.union` (ss S.\\ S.fromList k)
        , newSS /= ss = Just $ AvoidStrutsUnlessFullscreen newSS
        | otherwise = Nothing
      where toggleAll x | S.null x = S.fromList [minBound .. maxBound]
                        | otherwise = S.empty
            toggleOne x xs | x `S.member` xs = S.delete x xs
                           | otherwise   = x `S.insert` xs
