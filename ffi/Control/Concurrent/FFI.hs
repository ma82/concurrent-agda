module Control.Concurrent.FFI where

import Control.Concurrent (ThreadId,forkIO,myThreadId,killThread,
                           MVar,putMVar,takeMVar,newEmptyMVar)
import qualified Control.Concurrent.Chan as A
import Control.Concurrent.Chan.Synchronous
import Control.Monad

type AgdaUnit     a   = ()
type AgdaChan     a b = A.Chan b
type AgdaSyncChan a b = Chan b

-- Beware! The following code has never been tested.

rival :: Chan a -> Chan a -> MVar ThreadId -> MVar ThreadId -> MVar a -> IO ()
rival cI cO me you mom = do
  mine <- myThreadId
  putMVar me mine
  yours <- takeMVar you
  x <- readChan cI
  killThread yours
  -- TODO. Can I make sure `yours` is dead now?
  --       The process whose TID is `yours` must not be allowed to steal x!
  writeChan cO x
  putMVar mom x

-- `move` won't tell you *who* moved the data: you will not know in
-- which direction it was moved.
race :: Chan a -> Chan a -> MVar ThreadId -> MVar ThreadId -> MVar a -> IO a
race c1 c2 v1 v2 m = do
  forkIO (rival c1 c2 v1 v2 m)
  forkIO (rival c2 c1 v2 v1 m)
  takeMVar m

-- TODO. Will fuse's forever make channels forever referenced, hence
-- uncollectable?
move :: Chan a -> Chan a -> IO a
move c1 c2 = do
  v1 <- newEmptyMVar ; v2 <- newEmptyMVar ; m <- newEmptyMVar
  race c1 c2 v1 v2 m

fuse :: Chan a -> Chan a -> IO ()
fuse c1 c2 = do
  v1 <- newEmptyMVar ; v2 <- newEmptyMVar ; m <- newEmptyMVar
  forever $ race c1 c2 v1 v2 m
