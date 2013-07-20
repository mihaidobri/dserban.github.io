{-# LANGUAGE TupleSections #-}
module Graphics.UI.GLFW.Events(GLFWEvent(..), KeyEvent(..), IsPress(..), eventLoop) where

import Control.Monad(forever, (<=<))
import Data.IORef
import Data.Traversable (traverse)
import Graphics.UI.GLFW.ModState(ModState, isModifierKey, asModkey, modStateFromModkeySet)
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW

data IsPress = Press | Release
  deriving (Show, Read, Eq, Ord)

isPressFromBool :: Bool -> IsPress
isPressFromBool True = Press
isPressFromBool False = Release

-- this is the reification of the callback information:
data GLFWRawEvent = RawCharEvent IsPress Char
                  | RawKeyEvent IsPress GLFW.Key
                  | RawWindowRefresh
                  | RawWindowClose
  deriving (Show, Eq)

data KeyEvent = KeyEvent {
  kePress :: IsPress,
  keModState :: ModState,
  keChar :: Maybe Char,
  keKey :: GLFW.Key }
  deriving (Show, Eq)

-- This is the final representation we expose of events:
data GLFWEvent = GLFWKeyEvent KeyEvent
               | GLFWWindowClose
               | GLFWWindowRefresh
  deriving (Show, Eq)

translate :: [(ModState, GLFWRawEvent)] -> [GLFWEvent]
translate [] = []
translate ((_, RawWindowClose) : xs) = GLFWWindowClose : translate xs
translate ((_, RawWindowRefresh) : xs) = GLFWWindowRefresh : translate xs
translate ((modState1, rk@(RawKeyEvent isPress1 key)) :
           (modState2, rc@(RawCharEvent isPress2 char)) : xs)
  | isModifierKey key =
     -- This happens when you press shift while a key is pressed,
     -- ignore
     GLFWKeyEvent (KeyEvent isPress1 modState1 Nothing key) : translate xs
  | isPress1 == isPress2 =
    GLFWKeyEvent (KeyEvent isPress2 modState2 (Just char) key) : translate xs
  | otherwise =
    error $
      "RawCharEvent " ++ show rc ++
      " mismatches the RawKeyEvent: " ++ show rk
translate ((modState, RawKeyEvent isPress key) : xs) =
  GLFWKeyEvent (KeyEvent isPress modState Nothing key) : translate xs
-- This happens when you press shift while a key is pressed, ignore
translate ((_, RawCharEvent _ _) : xs) = translate xs

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ var f = atomicModifyIORef var ((, ()) . f)

rawEventLoop :: ([GLFWRawEvent] -> IO ()) -> IO a
rawEventLoop eventsHandler = do
  eventsVar <- newIORef []

  let
    addEvent event = atomicModifyIORef_ eventsVar (event:)
    addKeyEvent key isPress = addEvent $ RawKeyEvent (isPressFromBool isPress) key
    charEventHandler char isPress
      | '\57344' <= char && char <= '\63743' = return () -- Range for "key" characters (keys for left key, right key, etc.)
      | otherwise = addEvent $ RawCharEvent (isPressFromBool isPress) char

  GLFW.setCharCallback charEventHandler
  GLFW.setKeyCallback addKeyEvent
  GLFW.setWindowRefreshCallback $ addEvent RawWindowRefresh
  GLFW.setWindowSizeCallback . const . const $ addEvent RawWindowRefresh
  GLFW.setWindowCloseCallback $ addEvent RawWindowClose >> return True

  forever $ do
    GLFW.pollEvents
    let handleReversedEvents rEvents = ([], reverse rEvents)
    events <- atomicModifyIORef eventsVar handleReversedEvents
    eventsHandler events
    GLFW.swapBuffers

modKeyHandlerWrap ::
  ([(ModState, GLFWRawEvent)] -> IO ()) ->
  IO ([GLFWRawEvent] -> IO ())
modKeyHandlerWrap handler = do
  keySetVar <- newIORef Set.empty
  let
    modState r = do
      keySet <- readIORef keySetVar
      return (modStateFromModkeySet keySet, r)
    handleEvent e@(RawKeyEvent Press key) = do
      maybe (return ()) (modifyIORef keySetVar . Set.insert) $
        asModkey key
      modState e
    handleEvent e@(RawKeyEvent Release key) = do
      result <- modState e
      maybe (return ()) (modifyIORef keySetVar . Set.delete) $
        asModkey key
      return result
    handleEvent e = modState e
  return $ handler <=< traverse handleEvent

eventLoop :: ([GLFWEvent] -> IO ()) -> IO a
eventLoop handler = do
  mRawHandler <- modKeyHandlerWrap (handler . translate)
  rawEventLoop mRawHandler
