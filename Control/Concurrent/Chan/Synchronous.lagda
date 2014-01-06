\begin{code}
module Control.Concurrent.Chan.Synchronous where

open import IO.Primitive
open import Control.Concurrent

postulate Chan : ∀ {l} → Set l → Set l

{-# IMPORT Control.Concurrent.FFI                          #-}
{-# COMPILED_TYPE Chan Control.Concurrent.FFI.AgdaSyncChan #-}

postulate
  newChan   : ∀ {l}{A : Set l} → IO (Chan A)
  writeChan : ∀ {l}{A : Set l} → Chan A → A → IO <>
  readChan  : ∀ {l}{A : Set l} → Chan A → IO A
  move      : ∀ {l}{A : Set l} → Chan A → Chan A → IO A
  fuse      : ∀ {l}{A : Set l} → Chan A → Chan A → IO <>

{-# IMPORT Control.Concurrent.Chan.Synchronous                                  #-}
{-# COMPILED newChan   (\ _ _ -> Control.Concurrent.Chan.Synchronous.newChan  ) #-}
{-# COMPILED readChan  (\ _ _ -> Control.Concurrent.Chan.Synchronous.readChan ) #-}
{-# COMPILED writeChan (\ _ _ -> Control.Concurrent.Chan.Synchronous.writeChan) #-}
{-# COMPILED move      (\ _ _ -> Control.Concurrent.FFI.move                  ) #-}
{-# COMPILED fuse      (\ _ _ -> Control.Concurrent.FFI.fuse                  ) #-}
\end{code}

Ping-pong example.

\begin{code}
private
  open import Data.String

  [_] = toCostring

  infixl 1 _>>_

  _>>_ : ∀ {lA lB}{A : Set lA}{B : Set lB} → IO A → IO B → IO B
  m >> n = m >>= λ _ → n

  {-# NO_TERMINATION_CHECK #-}
  proc : String → Chan Costring → IO <>
  proc s c = readChan c        >>= λ m →
             putStr [ "Got " ] >>
             putStrLn m        >>
             writeChan c [ s ] >>
             proc s c

  ping pong : Chan Costring → IO <>
  ping = proc ">"
  pong = proc "<"
\end{code}

{-# NO_TERMINATION_CHECK #-}
main : IO <>
main = newChan {A = Costring}  >>= λ c →
       forkIO (ping c)         >> 
       writeChan c [ "start" ] >>
       pong c       
