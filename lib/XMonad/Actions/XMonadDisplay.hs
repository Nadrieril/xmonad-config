{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, OverlappingInstances, ParallelListComp #-}
module XMonad.Actions.XMonadDisplay where

import Data.Maybe
import Data.Bits
import Data.Char
import Data.Ord (comparing)
import Control.Monad.State
import Control.Arrow
import Data.List as L
import qualified Data.Map as M
import XMonad hiding (liftX)
import XMonad.Util.Font
import XMonad.Prompt (mkUnmanagedWindow)
import XMonad.StackSet as W
import XMonad.Layout.Decoration
import XMonad.Util.NamedWindows
import XMonad.Actions.WindowBringer (bringWindow)
import Text.Printf
import Data.Word (Word8)
import XMonad.Util.WorkspaceCompare (getSortByIndex)


myGridSelect :: X ()
myGridSelect = do
  let cfg = (buildDefaultGSConfig colorizer)
                { gsNavigate = navNSelect
                , gsCellheight = 30 }

  ws_sort <- getSortByIndex
  workspaces <- gets (map W.tag . ws_sort . W.workspaces . windowset)
  let workspaces_map = [(show i ++ "." ++ w, w) | i <- [1::Integer ..] | w <- workspaces]
  -- wks <- asks (XMonad.workspaces . config)
  -- _ <- gridselect cfg (map (\a -> (a,a)) wks)
  _ <- gridselect cfg workspaces_map
  return ()
    where colorizer _ active = return $
            if active
            then ("#B0B0B0", "#000000")
            else ("#808080", "#000000")



numpadKeys = [xK_KP_1, xK_KP_2, xK_KP_3, xK_KP_4, xK_KP_5, xK_KP_6, xK_KP_7, xK_KP_8, xK_KP_9, xK_KP_0]
azertyKeys = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0] :: [KeySym]

navNSelect :: TwoD a (Maybe a)
navNSelect = makeXEventhandler $ shadowWithKeymap navNSearchKeyMap navNSearchDefaultHandler
  where navNSearchKeyMap = M.fromList $ [
           ((0,xK_Escape)     , cancel)
          ,((0,xK_Return)     , select)
          -- ,((0,xK_Left)       , move (-1,0) >> navNSelect)
          -- ,((0,xK_Right)      , move (1,0) >> navNSelect)
          -- ,((0,xK_Down)       , move (0,1) >> navNSelect)
          -- ,((0,xK_Up)         , move (0,-1) >> navNSelect)
          ,((0,xK_Down)       , moveNext >> navNSelect)
          ,((0,xK_Up)         , movePrev >> navNSelect)
          ,((0,xK_Tab)        , moveNext >> navNSelect)
          ,((shiftMask,xK_Tab), movePrev >> navNSelect)
          ,((0,xK_BackSpace), transformSearchString (\s -> if s == "" then "" else init s) >> navNSelect)
          ]
          ++ [((0,k), setPos (0, i) >> navNSelect) | k <- numpadKeys | i <- [(0::Integer)..]]
          ++ [((0,k), setPos (0, i) >> navNSelect) | k <- azertyKeys | i <- [(0::Integer)..]]
        -- The navigation handler ignores unknown key symbols, therefore we const
        navNSearchDefaultHandler (_,s,_) = do
          transformSearchString (++ s)
          navNSelect



data GSConfig a = GSConfig {
      gsCellheight :: Integer,
      gsCellwidth :: Integer,
      gsCellpadding :: Integer,
      gsColorizer :: a -> Bool -> X (String, String),
      gsFont :: String,
      gsNavigate :: TwoD a (Maybe a),
      gsOriginFractX :: Double,
      gsOriginFractY :: Double
}

-- | That is 'fromClassName' if you are selecting a 'Window', or
-- 'defaultColorizer' if you are selecting a 'String'. The catch-all instance
-- @HasColorizer a@ uses the 'focusedBorderColor' and 'normalBorderColor'
-- colors.
class HasColorizer a where
    defaultColorizer :: a -> Bool -> X (String, String)

instance HasColorizer Window where
    defaultColorizer = fromClassName

instance HasColorizer String where
    defaultColorizer = stringColorizer

instance HasColorizer a where
    defaultColorizer _ isFg =
        let getColor = if isFg then focusedBorderColor else normalBorderColor
        in asks $ flip (,) "black" . getColor . config

-- | A basic configuration for 'gridselect', with the colorizer chosen based on the type.
--
-- If you want to replace the 'gsColorizer' field, use 'buildDefaultGSConfig'
-- instead, to avoid ambiguous type variables.
defaultGSConfig :: HasColorizer a => GSConfig a
defaultGSConfig = buildDefaultGSConfig defaultColorizer

type TwoDPosition = (Integer, Integer)

type TwoDElementMap a = [(TwoDPosition,(String,a))]

data TwoDState a = TwoDState { tdCurpos :: TwoDPosition
                             , tdAvailSlots :: [TwoDPosition]
                             , tdElements :: [(String,a)]
                             , tdGsconfig :: GSConfig a
                             , tdFont :: XMonadFont
                             , tdPaneX :: Integer
                             , tdPaneY :: Integer
                             , tdDrawingWin :: Window
                             , tdSearchString :: String
                             }

tdElementmap :: TwoDState a -> TwoDElementMap a
tdElementmap s = zip positions sortedElements
  where
    TwoDState {tdAvailSlots = positions,
               tdSearchString = searchString} = s
    -- Filter out any elements that don't contain the searchString (case insensitive)
    filteredElements = L.filter ((searchString `isInfixOfI`) . fst) (tdElements s)
    -- Sorts the elementmap
    sortedElements = orderElementmap searchString filteredElements
    -- Case Insensitive version of isInfixOf
    needle `isInfixOfI` haystack = upper needle `isInfixOf` upper haystack
    upper = map toUpper


-- | We enforce an ordering such that we will always get the same result. If the
-- elements position changes from call to call of gridselect, then the shown
-- positions will also change when you search for the same string. This is
-- especially the case when using gridselect for showing and switching between
-- workspaces, as workspaces are usually shown in order of last visited.  The
-- chosen ordering is "how deep in the haystack the needle is" (number of
-- characters from the beginning of the string and the needle).
orderElementmap :: String  -> [(String,a)] -> [(String,a)]
orderElementmap searchString elements = if not $ null searchString then sortedElements else elements
  where
    upper = map toUpper
    -- Calculates a (score, element) tuple where the score is the depth of the (case insensitive) needle.
    calcScore element = ( length $ takeWhile (not . isPrefixOf (upper searchString)) (tails . upper . fst $ element)
                        , element)
    -- Use the score and then the string as the parameters for comparing, making
    -- it consistent even when two strings that score the same, as it will then be
    -- sorted by the strings, making it consistent.
    compareScore = comparing (\(score, (str,_)) -> (score, str))
    sortedElements = map snd . sortBy compareScore $ map calcScore elements


newtype TwoD a b = TwoD { unTwoD :: StateT (TwoDState a) X b }
    deriving (Monad,Functor,MonadState (TwoDState a))

instance Applicative (TwoD a) where
    (<*>) = ap
    pure = return

liftX ::  X a1 -> TwoD a a1
liftX = TwoD . lift

evalTwoD ::  TwoD a1 a -> TwoDState a1 -> X a
evalTwoD m s = flip evalStateT s $ unTwoD m

diamondLayer :: (Enum a, Num a, Eq a) => a -> [(a, a)]
diamondLayer 0 = [(0,0)]
diamondLayer n =
  -- tr = top right
  --  r = ur ++ 90 degree clock-wise rotation of ur
  let tr = [ (x,n-x) | x <- [0..n-1] ]
      r  = tr ++ map (\(x,y) -> (y,-x)) tr
  in r ++ map (negate *** negate) r

diamond :: (Enum a, Num a, Eq a) => [(a, a)]
diamond = concatMap diamondLayer [0..]

diamondRestrict :: Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
diamondRestrict x y originX originY =
  L.filter (\(x',y') -> abs x' <= x && abs y' <= y) .
  map (\(x', y') -> (x' + fromInteger originX, y' + fromInteger originY)) .
  take 1000 $ diamond

findInElementMap :: (Eq a) => a -> [(a, b)] -> Maybe (a, b)
findInElementMap pos = find ((== pos) . fst)

drawWinBox :: Window -> XMonadFont -> (String, String) -> Integer -> Integer -> String -> Integer -> Integer -> Integer -> X ()
drawWinBox win font (fg,bg) ch cw text x y cp =
  withDisplay $ \dpy -> do
  gc <- liftIO $ createGC dpy win
  bordergc <- liftIO $ createGC dpy win
  liftIO $ do
    Just fgcolor <- initColor dpy fg
    Just bgcolor <- initColor dpy bg
    -- Just bordercolor <- initColor dpy borderColor
    setForeground dpy gc fgcolor
    setBackground dpy gc bgcolor
    -- setForeground dpy bordergc bordercolor
    fillRectangle dpy win gc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
    -- drawRectangle dpy win bordergc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
  stext <- shrinkWhile (shrinkIt shrinkText)
           (\n -> do size <- liftIO $ textWidthXMF dpy font n
                     return $ size > fromInteger (cw-(2*cp)))
           text
  printStringXMF dpy win font gc bg fg (fromInteger (x+cp)) (fromInteger (y+div ch 2)) stext
  liftIO $ freeGC dpy gc
  liftIO $ freeGC dpy bordergc

updateAllElements :: TwoD a ()
updateAllElements =
    do
      s <- get
      updateElements (tdElementmap s)

grayoutAllElements :: TwoD a ()
grayoutAllElements =
    do
      s <- get
      updateElementsWithColorizer grayOnly (tdElementmap s)
    where grayOnly _ _ = return ("#808080", "#808080")

updateElements :: TwoDElementMap a -> TwoD a ()
updateElements elementmap = do
      s <- get
      updateElementsWithColorizer (gsColorizer (tdGsconfig s)) elementmap

updateElementsWithColorizer :: (a -> Bool -> X (String, String)) -> TwoDElementMap a -> TwoD a ()
updateElementsWithColorizer colorizer elementmap = do
    TwoDState { tdCurpos = curpos,
                tdDrawingWin = win,
                tdGsconfig = gsconfig,
                tdFont = font,
                tdPaneX = paneX,
                tdPaneY = paneY} <- get
    let cellwidth = gsCellwidth gsconfig
        cellheight = gsCellheight gsconfig
        minmax (x:xs) = foldr (\x -> min x *** max x) (x,x) xs
        (minY, maxY) = minmax [y | ((_, y), _) <- elementmap]
        (minX, maxX) = minmax [x | ((x, _), _) <- elementmap]
        -- paneX' = div (paneX-cellwidth*(maxX-minX+1)) 2
        -- paneY' = div (paneY-cellheight*(maxY-minY+1)) 2
        paneX' = div (paneX-cellwidth) 2
        paneY' = div (paneY-cellheight) 2
        updateElement (pos@(x,y),(text, element)) = liftX $ do
            colors <- colorizer element (pos == curpos)
            drawWinBox win font
                       colors
                       cellheight
                       cellwidth
                       text
                       (paneX'+x*cellwidth)
                       (paneY'+y*cellheight)
                       (gsCellpadding gsconfig)
    mapM_ updateElement elementmap

stdHandle :: Event -> TwoD a (Maybe a) -> TwoD a (Maybe a)
stdHandle ButtonEvent { ev_event_type = t, ev_x = x, ev_y = y } contEventloop
    | t == buttonRelease = do
        s @  TwoDState { tdPaneX = px, tdPaneY = py,
                         tdGsconfig = (GSConfig ch cw _ _ _ _ _ _) } <- get
        let gridX = (fi x - (px - cw) `div` 2) `div` cw
            gridY = (fi y - (py - ch) `div` 2) `div` ch
        case lookup (gridX,gridY) (tdElementmap s) of
             Just (_,el) -> return (Just el)
             Nothing -> contEventloop
    | otherwise = contEventloop

stdHandle ExposeEvent{} contEventloop = updateAllElements >> contEventloop

stdHandle _ contEventloop = contEventloop

-- | Embeds a key handler into the X event handler that dispatches key
-- events to the key handler, while non-key event go to the standard
-- handler.
makeXEventhandler :: ((KeySym, String, KeyMask) -> TwoD a (Maybe a)) -> TwoD a (Maybe a)
makeXEventhandler keyhandler = fix $ \me -> join $ liftX $ withDisplay $ \d -> liftIO $ allocaXEvent $ \e -> do
                             maskEvent d (exposureMask .|. keyPressMask .|. buttonReleaseMask) e
                             ev <- getEvent e
                             if ev_event_type ev == keyPress
                               then do
                                  (ks,s) <- lookupString $ asKeyEvent e
                                  return $ do
                                      mask <- liftX $ cleanMask (ev_state ev)
                                      keyhandler (fromMaybe xK_VoidSymbol ks, s, mask)
                               else
                                  return $ stdHandle ev me

-- | When the map contains (KeySym,KeyMask) tuple for the given event,
-- the associated action in the map associated shadows the default key
-- handler
shadowWithKeymap :: M.Map (KeyMask, KeySym) a -> ((KeySym, String, KeyMask) -> a) -> (KeySym, String, KeyMask) -> a
shadowWithKeymap keymap dflt keyEvent@(ks,_,m') = fromMaybe (dflt keyEvent) (M.lookup (m',ks) keymap)

-- Helper functions to use for key handler functions

-- | Closes gridselect returning the element under the cursor
select :: TwoD a (Maybe a)
select = do
  s <- get
  return $ (snd . snd) <$> findInElementMap (tdCurpos s) (tdElementmap s)

-- | Closes gridselect returning no element.
cancel :: TwoD a (Maybe a)
cancel = return Nothing

-- | Sets the absolute position of the cursor.
setPos :: (Integer, Integer) -> TwoD a ()
setPos newPos = do
  s <- get
  let elmap = tdElementmap s
      newSelectedEl = findInElementMap newPos (tdElementmap s)
      oldPos = tdCurpos s
  when (isJust newSelectedEl && newPos /= oldPos) $ do
    put s { tdCurpos = newPos }
    updateElements (catMaybes [findInElementMap oldPos elmap, newSelectedEl])

-- | Moves the cursor by the offsets specified
move :: (Integer, Integer) -> TwoD a ()
move (dx,dy) = do
  s <- get
  let (x,y) = tdCurpos s
      newPos = (x+dx,y+dy)
  setPos newPos

moveNext :: TwoD a ()
moveNext = do
  position <- gets tdCurpos
  elems <- gets tdElementmap
  let n = length elems
      m = case findIndex (\p -> fst p == position) elems of
               Nothing -> Nothing
               Just k | k == n-1 -> Just 0
                      | otherwise -> Just (k+1)
  whenJust m $ \i ->
      setPos (fst $ elems !! i)

movePrev :: TwoD a ()
movePrev = do
  position <- gets tdCurpos
  elems <- gets tdElementmap
  let n = length elems
      m = case findIndex (\p -> fst p == position) elems of
               Nothing -> Nothing
               Just 0  -> Just (n-1)
               Just k  -> Just (k-1)
  whenJust m $ \i ->
      setPos (fst $ elems !! i)

-- | Apply a transformation function the current search string
transformSearchString :: (String -> String) -> TwoD a ()
transformSearchString f = do
          s <- get
          let oldSearchString = tdSearchString s
              newSearchString = f oldSearchString
          when (newSearchString /= oldSearchString) $ do
            -- FIXME: grayoutAllElements + updateAllElements paint some fields twice causing flickering
            --        we would need a much smarter update strategy to fix that
            when (length newSearchString > length oldSearchString) grayoutAllElements
            -- FIXME curpos might end up outside new bounds
            put s { tdSearchString = newSearchString }
            updateAllElements

-- | By default gridselect used the defaultNavigation action, which
-- binds left,right,up,down and vi-style h,l,j,k navigation. Return
-- quits gridselect, returning the selected element, while Escape
-- cancels the selection. Slash enters the substring search mode. In
-- substring search mode, every string-associated keystroke is
-- added to a search string, which narrows down the object
-- selection. Substring search mode comes back to regular navigation
-- via Return, while Escape cancels the search. If you want that
-- navigation style, add 'defaultNavigation' as 'gsNavigate' to your
-- 'GSConfig' object. This is done by 'buildDefaultGSConfig' automatically.
defaultNavigation :: TwoD a (Maybe a)
defaultNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList [
           ((0,xK_Escape)     , cancel)
          ,((0,xK_Return)     , select)
          ,((0,xK_slash)      , substringSearch defaultNavigation)
          ,((0,xK_Left)       , move (-1,0) >> defaultNavigation)
          ,((0,xK_h)          , move (-1,0) >> defaultNavigation)
          ,((0,xK_Right)      , move (1,0) >> defaultNavigation)
          ,((0,xK_l)          , move (1,0) >> defaultNavigation)
          ,((0,xK_Down)       , move (0,1) >> defaultNavigation)
          ,((0,xK_j)          , move (0,1) >> defaultNavigation)
          ,((0,xK_Up)         , move (0,-1) >> defaultNavigation)
          ,((0,xK_k)          , move (0,-1) >> defaultNavigation)
          ,((0,xK_Tab)        , moveNext >> defaultNavigation)
          ,((0,xK_n)          , moveNext >> defaultNavigation)
          ,((shiftMask,xK_Tab), movePrev >> defaultNavigation)
          ,((0,xK_p)          , movePrev >> defaultNavigation)
          ]
        -- The navigation handler ignores unknown key symbols, therefore we const
        navDefaultHandler = const defaultNavigation

-- | This navigation style combines navigation and search into one mode at the cost of losing vi style
-- navigation. With this style, there is no substring search submode,
-- but every typed character is added to the substring search.
navNSearch :: TwoD a (Maybe a)
navNSearch = makeXEventhandler $ shadowWithKeymap navNSearchKeyMap navNSearchDefaultHandler
  where navNSearchKeyMap = M.fromList [
           ((0,xK_Escape)     , cancel)
          ,((0,xK_Return)     , select)
          ,((0,xK_Left)       , move (-1,0) >> navNSearch)
          ,((0,xK_Right)      , move (1,0) >> navNSearch)
          ,((0,xK_Down)       , move (0,1) >> navNSearch)
          ,((0,xK_Up)         , move (0,-1) >> navNSearch)
          ,((0,xK_Tab)        , moveNext >> navNSearch)
          ,((shiftMask,xK_Tab), movePrev >> navNSearch)
          ,((0,xK_BackSpace), transformSearchString (\s -> if s == "" then "" else init s) >> navNSearch)
          ]
        -- The navigation handler ignores unknown key symbols, therefore we const
        navNSearchDefaultHandler (_,s,_) = do
          transformSearchString (++ s)
          navNSearch

-- | Navigation submode used for substring search. It returns to the
-- first argument navigation style when the user hits Return.
substringSearch :: TwoD a (Maybe a) -> TwoD a (Maybe a)
substringSearch returnNavigation = fix $ \me ->
  let searchKeyMap = M.fromList [
           ((0,xK_Escape)   , transformSearchString (const "") >> returnNavigation)
          ,((0,xK_Return)   , returnNavigation)
          ,((0,xK_BackSpace), transformSearchString (\s -> if s == "" then "" else init s) >> me)
          ]
      searchDefaultHandler (_,s,_) = do
          transformSearchString (++ s)
          me
  in makeXEventhandler $ shadowWithKeymap searchKeyMap searchDefaultHandler


-- FIXME probably move that into Utils?
-- Conversion scheme as in http://en.wikipedia.org/wiki/HSV_color_space
hsv2rgb :: Fractional a => (Integer,a,a) -> (a,a,a)
hsv2rgb (h,s,v) =
    let hi = div h 60 `mod` 6 :: Integer
        f = ((fromInteger h/60) - fromInteger hi) :: Fractional a => a
        q = v * (1-f)
        p = v * (1-s)
        t = v * (1-(1-f)*s)
    in case hi of
         0 -> (v,t,p)
         1 -> (q,v,p)
         2 -> (p,v,t)
         3 -> (p,q,v)
         4 -> (t,p,v)
         5 -> (v,p,q)
         _ -> error "The world is ending. x mod a >= a."

-- | Default colorizer for Strings
stringColorizer :: String -> Bool -> X (String, String)
stringColorizer s active =
    let seed x = toInteger (sum $ map ((*x).fromEnum) s) :: Integer
        (r,g,b) = hsv2rgb (seed 83 `mod` 360,
                           fromInteger (seed 191 `mod` 1000)/2500+0.4,
                           fromInteger (seed 121 `mod` 1000)/2500+0.4)
    in if active
         then return ("#faff69", "black")
         else return ("#" ++ concatMap (twodigitHex.(round :: Double -> Word8).(*256)) [r, g, b], "white")

-- | Colorize a window depending on it's className.
fromClassName :: Window -> Bool -> X (String, String)
fromClassName w active = runQuery className w >>= flip defaultColorizer active

twodigitHex :: Word8 -> String
twodigitHex = printf "%02x"

-- | A colorizer that picks a color inside a range,
-- and depending on the window's class.
colorRangeFromClassName :: (Word8, Word8, Word8) -- ^ Beginning of the color range
                        -> (Word8, Word8, Word8) -- ^ End of the color range
                        -> (Word8, Word8, Word8) -- ^ Background of the active window
                        -> (Word8, Word8, Word8) -- ^ Inactive text color
                        -> (Word8, Word8, Word8) -- ^ Active text color
                        -> Window -> Bool -> X (String, String)
colorRangeFromClassName startC endC activeC inactiveT activeT w active =
    do classname <- runQuery className w
       if active
         then return (rgbToHex activeC, rgbToHex activeT)
         else return (rgbToHex $ mix startC endC
                  $ stringToRatio classname, rgbToHex inactiveT)
    where rgbToHex :: (Word8, Word8, Word8) -> String
          rgbToHex (r, g, b) = '#':twodigitHex r
                               ++twodigitHex g++twodigitHex b

-- | Creates a mix of two colors according to a ratio
-- (1 -> first color, 0 -> second color).
mix :: (Word8, Word8, Word8) -> (Word8, Word8, Word8)
        -> Double -> (Word8, Word8, Word8)
mix (r1, g1, b1) (r2, g2, b2) r = (mix' r1 r2, mix' g1 g2, mix' b1 b2)
    where  mix' a b = truncate $ (fi a * r) + (fi b * (1 - r))

-- | Generates a Double from a string, trying to
-- achieve a random distribution.
-- We create a random seed from the sum of all characters
-- in the string, and use it to generate a ratio between 0 and 1
stringToRatio :: String -> Double
stringToRatio "" = 0
stringToRatio _ = 1/2

-- | Brings up a 2D grid of elements in the center of the screen, and one can
-- select an element with cursors keys. The selected element is returned.
gridselect :: GSConfig a -> [(String,a)] -> X (Maybe a)
gridselect _ [] = return Nothing
gridselect gsconfig elements =
 withDisplay $ \dpy -> do
    rootw <- asks theRoot
    s <- gets $ screenRect . W.screenDetail . W.current . windowset
    win <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
                    (rect_x s) (rect_y s) (rect_width s) (rect_height s)
    liftIO $ mapWindow dpy win
    liftIO $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
    status <- io $ grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
    io $ grabButton dpy button1 anyModifier win True buttonReleaseMask grabModeAsync grabModeAsync none none
    font <- initXMF (gsFont gsconfig)
    let screenWidth = toInteger $ rect_width s;
        screenHeight = toInteger $ rect_height s;
    selectedElement <- if status == grabSuccess then do
                            let
                                -- restriction ss cs = (fromInteger ss/fromInteger (cs gsconfig)-1)/2 :: Double
                                -- restrictX = floor $ restriction screenWidth gsCellwidth
                                -- restrictY = floor $ restriction screenHeight gsCellheight
                                -- originPosX = floor $ ((gsOriginFractX gsconfig) - (1/2)) * 2 * fromIntegral restrictX
                                -- originPosY = floor $ ((gsOriginFractY gsconfig) - (1/2)) * 2 * fromIntegral restrictY
                                -- coords = diamondRestrict restrictX restrictY originPosX originPosY
                                coords = map ((,) 0) [0..]

                            evalTwoD (updateAllElements >> gsNavigate gsconfig) TwoDState { tdCurpos = head coords,
                                                                                  tdAvailSlots = coords,
                                                                                  tdElements = elements,
                                                                                  tdGsconfig = gsconfig,
                                                                                  tdFont = font,
                                                                                  tdPaneX = screenWidth,
                                                                                  tdPaneY = screenHeight,
                                                                                  tdDrawingWin = win,
                                                                                  tdSearchString = "" }
                      else
                          return Nothing
    liftIO $ do
      unmapWindow dpy win
      destroyWindow dpy win
      sync dpy False
    releaseXMF font
    return selectedElement

-- | Like `gridSelect' but with the current windows and their titles as elements
gridselectWindow :: GSConfig Window -> X (Maybe Window)
gridselectWindow gsconf = windowMap >>= gridselect gsconf

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is then passed to
-- a callback function.
withSelectedWindow :: (Window -> X ()) -> GSConfig Window -> X ()
withSelectedWindow callback conf = do
    mbWindow <- gridselectWindow conf
    case mbWindow of
        Just w -> callback w
        Nothing -> return ()

windowMap :: X [(String,Window)]
windowMap = do
    ws <- gets windowset
    mapM keyValuePair (W.allWindows ws)
 where keyValuePair w = flip (,) w `fmap` decorateName' w

decorateName' :: Window -> X String
decorateName' w = show <$> getName w

-- | Builds a default gs config from a colorizer function.
buildDefaultGSConfig :: (a -> Bool -> X (String,String)) -> GSConfig a
buildDefaultGSConfig col = GSConfig 50 130 10 col "xft:Sans-8" defaultNavigation (1/2) (1/2)

borderColor :: String
borderColor = "white"

-- | Brings selected window to the current workspace.
bringSelected :: GSConfig Window -> X ()
bringSelected = withSelectedWindow $ \w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster

-- | Switches to selected window's workspace and focuses that window.
goToSelected :: GSConfig Window -> X ()
goToSelected = withSelectedWindow $ windows . W.focusWindow

-- | Select an application to spawn from a given list
spawnSelected :: GSConfig String -> [String] -> X ()
spawnSelected conf lst = gridselect conf (zip lst lst) >>= flip whenJust spawn

-- | Select an action and run it in the X monad
runSelectedAction :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction conf actions = do
    selectedActionM <- gridselect conf actions
    fromMaybe (return ()) selectedActionM

-- | Select a workspace and view it using the given function
-- (normally 'W.view' or 'W.greedyView')
--
-- Another option is to shift the current window to the selected workspace:
--
-- > gridselectWorkspace (\ws -> W.greedyView ws . W.shift ws)
gridselectWorkspace :: GSConfig WorkspaceId ->
                          (WorkspaceId -> WindowSet -> WindowSet) -> X ()
gridselectWorkspace conf viewFunc = withWindowSet $ \ws -> do
    let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    gridselect conf (zip wss wss) >>= flip whenJust (windows . viewFunc)
