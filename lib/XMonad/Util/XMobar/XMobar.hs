{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Util.XMobar.XMobar (XMobar(..), killXMobar, spawnXMobar, XMobarConfig(..)) where

import XMonad

import qualified XMonad.Util.ExtensibleState as State
import System.IO (writeFile, readFile)
-- import System.Posix.Types (ProcessID)
import qualified System.Posix.Files as Files
import qualified System.Posix.Signals as Signals
import qualified System.Directory
import Control.Monad (when, forM, forM_)
import Data.List (intercalate)

import qualified XMonad.Util.XMobar.Property as Property (Property)
------------------------------------------------------

data XMobarConfig = XMobarConfig {
      xmProperties :: [(Alias, Property.Property)]
    -- , xmConfigFile :: FilePath
    }

data XMobars = XMobars [XMobar] deriving Typeable
instance ExtensionClass XMobars where
    initialValue = XMobars []

data XMobar = XMobar {
      screen :: ScreenId
    , properties :: [Property]
    }

data Property = Property
    { property :: Property.Property
    , pipe :: FilePath
    , alias :: Alias
    }

type Alias = String


killXMobar i = do
    let dir = "/tmp/xmobar/"++i

    direxists <- System.Directory.doesDirectoryExist dir
    when direxists $ do
        pidexists <- System.Directory.doesFileExist (dir++"/pid")
        when pidexists $ do
            pid <- readFile (dir++"/pid")
            catchIO $ Signals.signalProcess Signals.sigTERM (read pid)
        System.Directory.removeDirectoryRecursive dir


spawnXMobar :: XMobarConfig -> String -> X ()
spawnXMobar xmconf i = do
    let dir = "/tmp/xmobar/"++i
    xmonad_dir <- getXMonadDir

    io $ do
        killXMobar i
        System.Directory.createDirectoryIfMissing True dir

    properties <- forM (xmProperties xmconf) (createProperty dir)

    let pipereaders = intercalate "," $ map pipereader properties
    spawn $ "xmobar "++xmonad_dir++"/xmobarrc -x "++i
            ++" -C \"["++pipereaders++"]\""
            ++" & echo $! > "++dir++"/pid;"

    -- State.put xmmobars

    where
        createProperty :: FilePath -> (Alias, Property.Property) -> X Property
        createProperty dir (alias, pty) = do
            let pipe = dir++"/"++alias
            io $ Files.createNamedPipe pipe pipe_mode
            return $ Property pty pipe alias

        pipereader (Property _ pipe alias) = "Run PipeReader \\\":"++pipe++"\\\" \\\""++alias++"\\\""
        pipe_mode = Files.unionFileModes Files.namedPipeMode Files.ownerModes



writeNamedPipe :: FilePath -> String -> IO ()
writeNamedPipe p s = writeFile p (s++"\n")

writeProperty :: XMobar -> Property -> X ()
writeProperty xmobar (Property p pipe _) = do
    value <- p (screen xmobar)
    io $ writeNamedPipe pipe value

writeProperties :: XMobar -> X ()
writeProperties xmobar = forM_ (properties xmobar) (writeProperty xmobar)

updateXmobars :: X()
updateXmobars = do
    XMobars xm <- State.get
    mapM_ writeProperties xm
