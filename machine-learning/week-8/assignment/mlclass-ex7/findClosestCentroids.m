function indexes = findClosestCentroids(X, centroids)
  % FINDCLOSESTCENTROIDS computes the centroid memberships for every example
  %   indexes = FINDCLOSESTCENTROIDS (X, centroids) returns the closest centroids
  %   in indexes for a dataset X where each row is a single example. indexes = m x 1
  %   vector of centroid assignments (i.e. each entry in range [1..K])

  % Set K.
  K = size(centroids, 1);

  % You need to return the following variables correctly.
  indexes = zeros(size(X, 1), 1);

  % Instructions: Go over every example, find its closest centroid, and store
  %               the index inside indexes at the appropriate location.
  %               Concretely, indexes(i) should contain the index of the centroid
  %               closest to example i. Hence, it should be a value in the
  %               range 1..K
  %
  % Note: You can use a for-loop over the examples to compute this.

  for i = 1 : size(X, 1)
    distances = zeros(K, 1);

    for k = 1 : K
      d = X(i, :) - centroids(k, :);
      distances(k) = d * d';
    end

    [ v, minimal_index ] = min(distances);
    indexes(i) = minimal_index;
  end
end