[rowsAmount, colsAmount] = size(data);

individualM = zeros(colsAmount, 3);
individualS = zeros(colsAmount, 3);

for j = 3:colsAmount
    X = data(:, j);
    M = data(:, 1);
    S = data(:, 2);
    
    Result = polyfit(X, M, 1);
    R = corrcoef(X,M);
    individualM(j, 1:2) = Result;
    individualM(j, 3) = R(1,2);

    Result = polyfit(X, S, 1);
    R = corrcoef(X, S);
    individualS(j, 1:2) = Result;
    individualS(j, 3) = R(1,2);
end
clc;
for i = 1:rowsAmount
    if data(i, 7) > 530000 & data(i, 7) < 531000
        data(i, :)
    end
    
    if data(i, 7) > 1660000 & data(i, 7) < 1661000
        data(i, :)
    end
end