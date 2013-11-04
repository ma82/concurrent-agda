
\begin{code}
module Control.Concurrent where

open import Unit public
open import IO.Primitive

postulate ThreadId : Set

{-# IMPORT Control.Concurrent #-}
{-# COMPILED_TYPE ThreadId Control.Concurrent.ThreadId #-}

open import Data.Nat

postulate
  forkIO      : ∀ {l} → IO (Unit l) → IO ThreadId
  Int         : Set
  onesec      : Int
  threadDelay : Int → ∀ {l} → IO (Unit l)

{-# COMPILED_TYPE Int         Int                                         #-}
{-# COMPILED      onesec      1000000                                     #-}
{-# COMPILED      forkIO      (\ _   -> Control.Concurrent.forkIO       ) #-}
{-# COMPILED      threadDelay (\ n _ -> Control.Concurrent.threadDelay n) #-}
\end{code}

