function [J, gradients] = cost(theta)
  J = (theta(1) - 5)^2 + (theta(2) - 5)^2;

  gradients = zeros(2, 1);
  gradients(1) = 2 * (theta(1) - 5);
  gradients(2) = 2 * (theta(2) - 5);
end

options = optimset('GradObj', 'on', 'MaxIter', '100');
initialTheta = zeros(2, 1);

[ optimalTheta, functionVal, exitFlag ] = fminunc(@costFunctiom, initialTheta, options)