module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero = "O"
    show (Succ n)= 'S':show n


instance Eq Nat where

    (==) a b = show a == show b

instance Ord Nat where

    (<=) = undefined

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min (Succ n) (Succ m) = Succ (min n m)
    min _ _ = Zero

    max (Succ n) (Succ m) = Succ (max n m)
    max n _ = n
    max _ n = n

isZero :: Nat -> Bool
isZero Zero = True
isZero (Succ _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

even :: Nat -> Bool
even Zero = True
even (Succ n) = odd n

odd :: Nat -> Bool
odd Zero = False
odd (Succ n) = even n

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) n Zero = n
(<+>) n (Succ m) = Succ (n <+> m)
-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero n = Zero
(<->) n Zero = n
(<->) (Succ n) (Succ m) = n <-> m
 
-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) n Zero = Zero
(<*>) n (Succ m) = n + (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) n Zero = (Succ Zero)
(<^>) n (Succ m) = n <^> (m) <*> n

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) = undefined

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = undefined
        | x == 0    = undefined
        | otherwise = undefined

