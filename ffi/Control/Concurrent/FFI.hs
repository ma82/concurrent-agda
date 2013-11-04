module Control.Concurrent.FFI where

import Control.Concurrent
import Control.Concurrent.Chan
import qualified Control.Concurrent.Chan.Synchronous as SC

type AgdaUnit     a   = ()

type AgdaChan     a b = Chan b
type AgdaSyncChan a b = SC.Chan b
