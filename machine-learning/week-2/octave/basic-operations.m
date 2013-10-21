% Comments are starting with percent sign.

(5+6-2*8) / 2
2^6

1 ~= 2
1 == 2

1 && 0
1 || 0

xor(1, 0)

% Changing Octave prompt.
PS1('>> ')

% Variables.
a = 3
b = 'Hello Octave!'
c = (3 >= 1)
d = pi

% Displaying and formatting
disp(d)
disp(sprintf('2 decimals: %0.2f', d))

% Setting up formating.
format long
d

format short
d

% Matrices and vectors.
A = [ 1 2; 3 4; 5 6 ]

v = [ 1 2 3 ]
v = [ 1; 2; 3 ]

v = 1 : 0.1 : 2
v = 1 : 6

ones(2, 3)
zeros(2, 3)

% Identity matrix.
I = eye(4)

C = 2 * ones(2, 3)
W = rand(3, 3)
W = randn(3, 3)

% Semicolon at the end prevent from
% printing out values on standard output.
w = -6 + sqrt(10) * randn(1, 10000);

% Histogram.
hist(w)
hist(w, 50)

% Manual in Octave ;).
help eye