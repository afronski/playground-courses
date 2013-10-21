% Plotting data.

t = [ 0 : 0.01 : 0.98 ];
y1 = sin(2 * pi * 4 * t);

plot(t, y1)

y2 = cos(2 * pi * 4 * t);

% Using the same figure again.
hold on;

% And different color.
plot(t, y2, 'r')

% Adding description.

xlabel('time')
ylabel('value')
legend('sin', 'cos')
title('Plot')

% Saving as a PNG file.
print -dpng 'plot.png'

% Using two figures at one time.

figure(1); plot(t, y1, 'b')
figure(2); plot(t, y2, 'r')

% Subplotting.

subplot(1, 2, 1);
plot(t, y1);

subplot(1, 2, 2);
plot(t, y2);

% Setting up an axis.
axis([ 0.5 1 -1 1 ])

% Clearing figures.
clf

% Visualizing matrices.
A = magic(15);

imagesc(A);
imagesc(A), colorbar, colormap gray;