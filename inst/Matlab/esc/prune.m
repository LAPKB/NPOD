function new_theta = prune(theta,theta_plus,theta_d)

% a = 0.1;
% b = 0.7;

a = [1,0.1];
b = [4,1];

new_theta =theta;
dist = inf;

            for ink = 1:length(new_theta(1,:))
                new_dist = sum(abs(theta_plus-new_theta(:,ink))./(b-a)');
                dist = min(dist,new_dist);
            end
            up = sign(min(theta_plus-a'));
            down  = sign(min(b'-theta_plus));
             if (dist>theta_d)&&(up>-1)&&(down>-1)
                new_theta = [new_theta,theta_plus];   
            end
           
%             if (dist>theta_d)&&(a(1)<=theta_plus(1))&&(theta_plus(1)<=b(1))&&(a(2)<=theta_plus(2))&&(theta_plus(2)<=b(2))
%                 new_theta = [new_theta,theta_plus];   
%             end         

