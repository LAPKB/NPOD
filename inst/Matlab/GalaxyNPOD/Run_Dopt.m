clear all

rand('seed',0)
%---------INPUT-----------
y = [ 9.1720 9.3500 9.4830 9.5580 9.7750 10.2270 10.4060 16.0840 16.1700 18.4190 18.5520 18.6000 18.9270 19.0520 19.0700 19.3300 19.3430 19.3490 19.4400 19.4730 19.5290 19.5410 19.5470 19.6630 19.8460 19.8560 19.8630 19.9140 19.9180 19.9730 19.9890 20.1660 20.1750 20.1790 20.1960 20.2150 20.2210 20.4150 20.6290 20.7950 20.8210 20.8460 20.8750 20.9860 21.1370 21.4920 21.7010 21.8140 21.9210 21.9600 22.1850 22.2090 22.2420 22.2490 22.3140 22.3740 22.4950 22.7460 22.7470 22.8880 22.9140 23.2060 23.2410 23.2630 23.4840 23.5380 23.5420 23.6660 23.7060 23.7110 24.1290 24.2850 24.2890 24.3660 24.7170 24.9900 25.6330 26.9600 26.9950 32.0650 32.7890 34.2790]';


sigma = 2.08;
% theta_0 = y';
theta_0 = 8+30*rand(1,3);

% theta_0  = faure1;

%eps = 10^(-4);
a = 8;
b = 38;

t = 0;

tic

theta_F = 10^(-2);
theta_d = 10^(-4);

%--------Run DNPAG clone-------


[count, theta, w, LogLikelihood] = Dopt(y, t, theta_0, theta_F, theta_d,sigma,a,b)

%pyl = p_y_l(y, theta, w, sigma);

toc

%PYL = p_y_l(y, t, theta, w, sigma);
P = PSI_2(y,t,theta,sigma);
PYL = P*(w)';



Dtheta =@(theta_parameter) D(theta_parameter, y, t, sigma, PYL);

nDtheta = @(theta_parameter)((-1)*(Dtheta(theta_parameter)));

x = fminbnd(nDtheta, 8,38);

max_value = Dtheta(x)

figure(1); clf;

fplot(Dtheta, [0,40])
grid on

stem(theta,w) 
