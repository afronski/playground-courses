% Initialization.
A = [ 1 2; 3 4; 5 6 ]
B = [ 11 12; 13 14; 15 16 ]
C = [ 1 1; 2 2 ]

% Matrix multiplication.
A * C

% Multiplication of elements from matrices.
A .* B

% Operation on vectors.
v = [ 1; 2; 3]

1 ./ v
1 ./ A

log(v)
exp(v)

abs([ -1; -2; -3 ])
-v

% Incrementing vector by one.
v + ones(length(v), 1)
v + 1

% Transposing matrix.
A'
(A')'

% New operations on vectors.

v = [ 0 2.1 15 4.3 ]

max(v)
[ value, index ] = max(v)

v < 4
find(v < 4)

% Playing with 'magic' matrices ;).
A = magic(3)
[ rows, columns ] = find(A >= 7)

% Returns maximum from each column.
max(A, [], 1)

% Returns maximum from each row.
max(A, [], 2)

% Maximum value from whole matrix.
max(max(A))

% Converting matrix to vector.
A(:)

% Get maximum from matrix converted to vector.
max(A(:))

% Another vector operations.
v = [ 1 2 3 4 ]

sum(v)
prod(v)
floor(v)
ceil(v)

% Get only diagonals from matrix.

magic(9) .* eye(9)
magic(9) .* flipud(eye(9))

% Inverting matrix.

pinv(magic(3))
inv(magic(3))