{-# LANGUAGE DeriveDataTypeable #-}
module XMobar.XMobar (XMobar(..), Alias) where

import XMonad

import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as State
import System.IO (Handle, writeFile, readFile)
import System.Posix.Types (ProcessID)
------------------------------------------------------

data XMobarStorage = XMobarStorage (M.Map ScreenId XMobar) deriving Typeable
instance ExtensionClass XMobarStorage where
    initialValue = XMobarStorage M.empty

type Alias = String

data XMobar = XMobar {
      xmPid :: ProcessID
    , xmPipes :: M.Map Alias FilePath
    , xmScreen :: ScreenId
    , xmDir :: FilePath
    }
