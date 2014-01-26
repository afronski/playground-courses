-- Defining a list (list are homogeneous).
x = [ 1, 2, 3 ]

-- Con operator (inserting element at the beginning of the list).
y = 0 : x

-- Other representation of string (list of characters).
str = 'a' : 'b' : 'c' : []

-- Concatenation operator applies also to the lists.
z = x ++ y

-- Lists deconstruction.
headX = head x
tailX = tail x

-- Testing lists emptiness.
empty = null []
nonEmpty = null x

-- Sample usage.
double nums =
  if null nums
    then []
    else (2 * (head nums)) : (double (tail nums))