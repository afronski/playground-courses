v = zeros(10, 1);

% For loops.

for i=1:10,
  v(i) = 2 ^ i;
end

indices = 1:10;
for i=indices,
  i;
end;

% While loops.

i = 1;
while i <= 5,
  v(i) = 100;
  i = i + 1;
end;

% You can use break and continue also.
% Also you can use if, elseif, else clauses.

i = 1;
while true,
  v(i) = 999;
  i = i + 1;

  if i == 6,
    break;
  elseif i == 7
    break;
  else
    continue;
  end;
end;

% Modyfing Octave search path.
addpath('/home/user/scripts/octave');

% Functions.

function y = square(x)
  y = x ^ 2;
end;

square(4)

function [ s, c ] = squareAndCube(x)
  s = x ^ 2;
  c = x ^ 3;
end;

[ a, b ] = squareAndCube(3)

% Sample use case of created function.

X = [ 1 1; 1 2; 1 3 ]
y = [ 1; 2; 3 ]
theta = [ 0; 1 ]

function J = cost(X, y, theta)
  m = size(X, 1);

  predictions = X * theta;
  squareErrors = (predictions - y) .^ 2;

  J = 1 / (2 * m) * sum(squareErrors);
end

j = cost(X, y, theta)