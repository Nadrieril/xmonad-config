import System.Environment (getArgs)
import XMonad

main = do
    [wksp, scr] <- getArgs
    sendEvt wksp scr

sendEvt name dt = do
    dpy <- openDisplay ""
    rw <- rootWindow dpy $ defaultScreen dpy
    atom <- internAtom dpy name False
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw atom 32 (read dt) currentTime
        sendEvent dpy rw False structureNotifyMask e
    sync dpy False
