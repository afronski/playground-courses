-- Raising a number to power of 2.
square x = x * x

-- Returning description of passed number
-- in terms of signess.
positiveOrNegative x =
  if x >= 0
    then "Positive"
    else "Negative"

sqrt3 = sqrt 3
square2 = square 2

-- Recursion samples.

pow2 n =
  if n == 0
    then 1
    else 2 * (pow2 (n - 1))

repeatString str n =
  if n == 0
    then ""
    else str ++ (repeatString str (n - 1))