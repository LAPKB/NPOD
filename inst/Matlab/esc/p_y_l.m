function p = p_y_l(y, t, theta_0, w_0, sigma)

N = length(y);
K = length(theta_0(1,:));
f_lamda = zeros(N,K);

for i = 1:N
    for l = 1:K
    f_lamda(i,l) = w_0(l) * prob(y(i,:), t, theta_0(:,l), sigma); 
    end
end
p_y_lamda = sum(f_lamda,2);

p = p_y_lamda;
