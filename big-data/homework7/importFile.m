function importFile(fileToRead)
newData = importdata(fileToRead);
vars = fieldnames(newData);

for i = 1:length(vars)
    assignin('base', vars{i}, newData.(vars{i}));
end

