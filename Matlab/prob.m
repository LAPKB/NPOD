function p = prob(y, t, theta, sigma)

% y-dim(1,J)
% t-dim(1,J)
% theta -dim(1,1)

J = length(y);
dim = length(theta);
z = zeros(1,J);

for j = 1:J
    
       m = mu(theta,t(j));
       z(j) = (y(j)-m)^2 ;
   
end
p = (1/(sqrt(2*pi)*sigma))^(J)*exp(-(1/(2*(sigma)^(2)))*sum(z));
end

