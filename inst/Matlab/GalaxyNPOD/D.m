function D = D(theta_parameter, y,t, sigma, PYL)

N = length(y(:,1));
D_components = -N;

for i=1:N
    
    D_components = D_components + (prob(y(i,:), t, theta_parameter, sigma))/PYL(i);
    
end
D = D_components;

end