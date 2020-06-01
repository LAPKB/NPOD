function [y, t, sigma, theta_0,true_theta] = initial_data(K)

rand('seed', 0)
theta_0 = zeros(1,K);

[t,y,true_theta] = simulation
sigma = 0.005;

a = 1;
b = 4;
c = 0.1;
d = 1;

%theta_0=true_theta;
for l = 1:K
%     if dim == 1
        theta_0(1,l) = a + (l-rand)*((b - a)/K);
        theta_0(2,l) = c + (l-rand)*((d - c)/K);
%     end
%     if dim == 2
 %       theta_0(1,l) = a + (l-rand)*((b - a)/K);
 
 %       theta_0(2,l) = c + (l-rand)*((d - c)/K);
%     end
end

% w_0 =(1/K) * ones(1,K);


       