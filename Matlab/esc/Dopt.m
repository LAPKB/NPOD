function [count, theta, w, LogLikelihood] = Dopt(y, t, theta_0, theta_F, theta_d,sigma)

%---matrix: y(NxJ), t(NxJ), w_0(1xK), theta_0(dim x K)
%---scalar: eps,sigma,linar

old_theta = theta_0;
count = 1;

F0 = -10^(30);
F1 = 2*F0;
options = optimset('MaxFunEvals', 2000000000,'TolX',1e-14,'MaxIter', 5,'TolFun',1e-14);

%  main loop-----------------------------------------------------------------


while (abs(F1-F0) > theta_F) 
    
    F0 = F1;
 
  % w_prime calculation----------------------------------------------------------------------------------
  
    P = PSI_2(y,t,old_theta,sigma); 
    [~,lam]=burke(P);
  
    ind = (lam>0.00000001) & (lam > (max(lam)/1000));    
    inb_theta = old_theta(:,ind);
%     sortrows(new_theta',[1 2]);
%     size_new_theta = size(new_theta);

    P = PSI_2(y,t,inb_theta,sigma); 
    [F1,lam]=burke(P);
    ind2 = (lam > (max(lam)/1000));
    new_w = (lam(ind2))'/sum(lam(ind2));
    new_theta = inb_theta(:,ind2);
%       pause
      
        if abs(F1 - F0) <=theta_F
            break 
        end
        
% theta_prime calculation----------------------------------------------------------------------------------
  
       
           K = length(new_theta(1,:));
           pyl = P*(new_w)';
           parfor l = 1:K 
%                l
               Dtheta = @(theta)D(theta, y,t, sigma, pyl);
               fun = @(theta_parameter)((-1)*(Dtheta(theta_parameter)));
               % search for candidate point to be added--------------------------               
               cand_theta = fminsearch(fun,new_theta(:,l),options);
                     
               % prune a new set of grid points (i.e. make sure that a new point is far enought from the old points)     
                nc_theta = prune(new_theta,cand_theta,theta_d);

            end
          
old_theta = nc_theta;  



%       pause

% dif = F1 - F0;   %% progress bar

count = count+1;
end


LogLikelihood = F1;
% P = PSI_2(y,t,old_theta); 
% [~,lam]=burke(P);  
% new_w = lam';
w = new_w;
theta = new_theta;
