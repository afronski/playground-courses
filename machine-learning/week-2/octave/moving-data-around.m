% Checking size of matrix.
A = ones(3, 2) * 4

size(A)
size(A, 1)
size(A, 2)

v = 1 : 4

length(v)

% Moving in filesystem.
pwd
cd ..
ls

% Reading data files.
load('featuresX.dat')
load('priceY.dat')

% List of variables in workspace.
who
whos             % Detailed view.

% Removing variables.
clear A
clear

% Slice - get first ten elements from priceY.
v = priceY(1:10)

% Saving variables
save 'filename.dat' v
save 'filename.txt' v -ascii

% Indexing Matrix.

A(3, 2)   % Get cell from 3 row and 2 column.
A(2, :)   % Get second row.
A(:, 2)   % Get second column.

A([ 1 3 ], :)   % Get whole 1 and 3 row.
A(:, [ 1 2 ])   % Get whole 1 and 2 column.

% Replacing slices.

% Replace 2 column by passed vector.
A(:, 2) = [ 10; 11; 12 ]

% Append new column to the right.
A = [ A, [ 100; 101; 102 ] ]

% Append new column to the left.
A = [ [ 100; 101; 102 ], A ]

% Concatenating matrices.

A = [ 1 2; 3 4; 5 6 ]
B = [ 11 12; 13 14; 15 16]

% Put B on the right.
C = [ A B ]
C = [ A, B ]

% Put B on the bottom.
D = [ A; B ]