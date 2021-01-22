function new_theta = prune(theta,theta_plus,theta_d,a,b)

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

