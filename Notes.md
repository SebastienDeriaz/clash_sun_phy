# Clash notes

```haskell
-- Unbundle a Signal Tuple (for example the topEntity input)
key :: Signal dom (Unsigned 4, Unsigned 10)

unbundle key :: (Signal dom Unsigned 4, Signal dom Unsigned 10)

fst $ unbundle key :: Signal dom Unsigned 4


-- Add "Signal System" or "Signal dom" to something
A = (0, 0) :: (a0, b0)
B = (0 :: Unsigned 10, 0 :: Unsigned 7) :: (Unsigned 10, Unsigned 7)

pure B :: Signal System(Unsigned 10, Unsigned 7)


-- Bundle
bundle :: (Signal dom Bool, Signal dom Int) -> Signal dom (Bool,Int)
-- Unbundle
unbundle :: Signal dom (Int,Bool) -> (Signal dom Int, Signal dom Bool)



-- Concatenate an input tuple (two values) into one output
output = uncurry (++#) <$> input


-- Concatenate a signal (key) value with 0
uncurry (++#) <$> bundle (0, key)

```
