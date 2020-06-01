function P = PSI_2(y,t,theta, sigma)

N = length(y(:,1));
K = length(theta(1,:));
Psi = zeros(N,K);
%sigma = 0.005;

for i = 1:N
    for l=1:K
        Psi(i,l) = prob(y(i,:), t, theta(:,l), sigma);
    end
end
P = Psi;
end

%%----------------------------------------------------------------
% function p = prob(y, t, theta, sigma)
% 
% % y-dim(1,J)
% % t-dim(1,J)
% % theta -dim(1,1)
% 
% J = length(y);
% dim = length(theta);
% z = zeros(1,J);
% 
% for j = 1:J
%     
%        m = (100/theta(2))*exp((-1)*theta(1)*t(j));
%        z(j) = (y(j)-m)^2 ;
%    
% end
% p = (1/(sqrt(2*pi)*sigma))^(J)*exp(-(1/(2*(sigma)^(2)))*sum(z));
% end
