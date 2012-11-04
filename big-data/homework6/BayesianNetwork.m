N = 8; 
dag = zeros(N, N);

% Nodes names.
alfa = 1; 
tau = 2;
sigma = 3;
lambda = 4;
beta = 5;
epsilon = 6;
dzeta = 7;
delta = 8;

% Encoding graph structure in DAG matrix.
dag(alfa, tau) = 1;
dag(tau, epsilon) = 1;
dag(sigma, [lambda, beta]) = 1;
dag(beta, delta) = 1;
dag(lambda, epsilon) = 1;
dag(epsilon, [dzeta, delta]) = 1;

% Bayes network shell.
discrete_nodes = 1:N;
node_sizes = 2 * ones(1, N); 
bnet = mk_bnet(dag, ...
               node_sizes, ...
               'discrete', ...
               discrete_nodes);
           
% Assigning parameters.
bnet.CPD{alfa} = tabular_CPD(bnet, alfa, [0.01, 0.99]);
bnet.CPD{sigma} = tabular_CPD(bnet, sigma, [0.5, 0.5]);

bnet.CPD{tau} = tabular_CPD(bnet, tau, ...
                [ 0.95, 0.99, 0.01, 0.05 ]);
bnet.CPD{lambda} = tabular_CPD(bnet, lambda, ...
                   [ 0.9, 0.99, 0.01, 0.1 ]);
bnet.CPD{beta} = tabular_CPD(bnet, beta, ...
                 [ 0.4, 0.7, 0.3, 0.6 ]);
bnet.CPD{dzeta} = tabular_CPD(bnet, dzeta, ...
                  [ 0.02, 0.95, 0.05, 0.98 ]);
                        
bnet.CPD{epsilon} = tabular_CPD(bnet, epsilon, ...
                    [ 1, 0, 0, 0, 0, 1, 1, 1 ]);
bnet.CPD{delta} = tabular_CPD(bnet, delta, ...
                  [ 0.9, 0.2, 0.3, 0.1, 0.1, 0.8, 0.7, 0.9 ]);

% Create engine from Bayesian network.
engine = jtree_inf_engine(bnet);         
evidence = cell(1, N);
evidence{alfa} = 1;
evidence{sigma} = 2;
evidence{dzeta} = 2;
evidence{delta} = 1;
engine = enter_evidence(engine, evidence);

% Calculate a posteriori propabilities for diseases.
N_diseases = 4;
diseases = [ tau, lambda, beta ];
aposteriori = zeros(1, N_diseases);

for i = diseases(:)'
  m = marginal_nodes(engine, i);
  aposteriori(i) = m.T(2);
end

clc;
disp('Tuberculosis:'); aposteriori(1)
disp('Lung cancer:'); aposteriori(2)
disp('Bronchitis:'); aposteriori(3)
           