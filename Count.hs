module Count() where
import qualified Prelude()
import Circuit.Circuit (Circuit(..), reify)
import Data.Typeable

data Nat = Z | S Nat deriving Typeable
data Bool = F | T deriving Typeable
data Either a b = Left a | Right b deriving Typeable

-- less-than-or-equal on Peano
le :: Nat -> Nat -> Bool
le Z _ = T
le (S _) Z = F
le (S m) (S n) = le m n

-- step: if n <= 10, continue with n+1; else stop
step :: Either Nat Nat -> Either Nat Nat
step (Left n) = case le n (S (S (S (S (S (S (S (S (S (S Z)))))))))) of
  T -> Left (S n)
  F -> Right n
step (Right n) = Left Z

-- Knot wraps the step function into a feedback loop
counter :: Circuit (->) Either Nat Nat
counter = Knot (Lift step)
