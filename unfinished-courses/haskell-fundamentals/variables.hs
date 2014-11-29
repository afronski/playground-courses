-- Samples of using 'let' binding.
-- You can combine multiple variables in single 'let' expression.

fancySeven =
  let a = 3
  in 2 * a + 1

fancyNine =
  let x = 4
      y = 5
  in x + y

-- There is a second way for providing variables in Haskell.
-- You can use 'where' binding, but it has to be associated
-- with function definition.

fancySeven' = 2 * a + 1
  where a = 3

fancyNine' = x + y
  where x = 4
        y = 5

-- Remember about whitespace arrangement for 'let' and 'where' expressions.
-- Do not use tabs ever.