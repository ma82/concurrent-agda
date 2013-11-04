TODO. Modify stdlib's Foreign.Haskell instead?

\begin{code}
module Unit where

open import Level
data Unit l : Set l where tt : Unit l

<> = Unit zero

⟨⟩ : <>
⟨⟩ = tt

{-# IMPORT Control.Concurrent.FFI #-}
{-# COMPILED_DATA Unit Control.Concurrent.FFI.AgdaUnit () #-}
\end{code}

