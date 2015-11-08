{-# LANGUAGE OverloadedStrings #-}
module XMonad.Hooks.LogApplet (AppletPipe, initApplet, outputApplet) where
import qualified DBus
import qualified DBus.Client as DBus
import qualified Codec.Binary.UTF8.String as UTF8
-----------------------------------------------------------------------
-- For use with https://github.com/alexkay/xmonad-log-applet

type AppletPipe = DBus.Client

initApplet :: IO AppletPipe
initApplet = do
  dbus <- DBus.connectSession
  getWellKnownName dbus
  return dbus

getWellKnownName :: AppletPipe -> IO ()
getWellKnownName dbus = do
  _ <- DBus.requestName dbus (DBus.busName_ "org.xmonad.Log")
                [DBus.nameAllowReplacement, DBus.nameReplaceExisting, DBus.nameDoNotQueue]
  return ()

outputApplet :: AppletPipe -> String -> IO ()
outputApplet dbus str = do
    let signal = (DBus.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            DBus.signalBody = [DBus.toVariant $ UTF8.decodeString str]
        }
    DBus.emit dbus signal
