clear all

rand('seed',0)
%---------INPUT-----------
[y, t, sigma, theta_0,true_theta] = initial_data(10);



%--------Run DNPAG clone-------

tic

theta_F = 10^(-2);
theta_d = 10^(-4);

% a = [-1 1];
%b = [2 2];

[count, theta, w, LogLikelihood] = Dopt(y, t, theta_0, theta_F, theta_d, sigma)

% [count, theta, w, LogLikelihood] = Dopt(y,t, theta_0, eps, sigma,a,b)

toc

%PYL = p_y_l(y, t, theta, w, sigma);
P = PSI_2(y,t,theta,sigma);
PYL = P*(w)';

Dfun = @(theta_parameter)D(theta_parameter, y, t, sigma, PYL);
    K = (1.5:0.1:3.5);
     V = (0.3:0.05:0.5);
     Z = zeros(length(K),length(V));
      for i = 1:length(K)
            for j = 1:length(V)
                 Z(i,j) = Dfun([K(i),V(j)]);
            end
      end
    D_max_val = max(max(Z))
%D_max_val = max(Z)
    surf(K, V, Z')

stem(theta(1,:),w)   

%true_theta =[2.8388    2.6503    2.6587    3.0654   2.8359    2.9784    2.9492    3.0391   2.9501    2.5147;0.4058    0.4031    0.4062    0.3968  0.4029    0.4085    0.3993    0.3933   0.4013    0.4044];
hold on
plot(true_theta(1,:),zeros(1,length(true_theta(1,:))),'*')
hold off

figure(2)
stem(theta(2,:),w)    
hold on
plot(true_theta(2,:),zeros(1,length(true_theta(2,:))),'*')
xlim([0.2 0.7])
hold off

figure(3)
plot(t,y,t,y,'o')
