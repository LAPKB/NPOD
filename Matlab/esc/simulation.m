function [t,y,theta] = simulation

N = 10;
J = 7;
% t = zeros(1,J);
sigma = 0.005;
rand('seed',0)
randn('seed',0)

%rng( 0)

t = [ 10, 40, 80, 120, 240, 480, 1100];
%% Initialization
% name of xml file
%appPath=[fileparts(which('example_timecourses_upon_parameter_variation.m')) filesep];
xml='C:\Users\alona.kryshchenko\Dropbox (CSUCI)\For Alan\SummerGrant\ESC\IV 10mg.xml';

% Create the structure for the variable parameters
initStruct=[];

%initStruct=initParameter(initStruct,'IV 10mg|Escitalopram-CYP2D6-von Moltke|CLspec/[Enzyme]','withWarning');
initStruct=initParameter(initStruct,'IV 10mg|CYP2D6|Reference concentration','withWarning');
initStruct=initParameter(initStruct,'IV 10mg|Escitalopram|Lipophilicity','withWarning');
% Initialize the simulation
initSimulation(xml,initStruct,'report','none');

s2=.05; 
s3=.05;

K1= 0.4 + s2 *randn(1,N); 
K2= 0.4 + s3*randn(1,N); 
K=zeros(1,N); 
%lip =2.85+.25*randn(1,N);

s4=.15; 
s5=.15;

Lip1= 2 + s4 *randn(1,N); 
Lip2= 3 + s5*randn(1,N); 
lip=zeros(1,N); 

u=rand(1,N);

for k=1:N
if u(k) < .5
K(k)=K1(k);
lip(k) = Lip1(k);
else
K(k)=K2(k); 
lip(k) = Lip2(k);
end 
end 

% main loop, simulation of y 
%lip = [-0.3217    0.0233    0.2627    0.3417    0.0252];
%theta = K;
theta = [lip;K];
%theta = [2.5864    2.9968    2.2376    2.5622    2.7785    2.9110    3.1006    3.1936    2.6903    2.6396    3.2036; 0.3958    0.4872    0.3275    0.4156    0.4424    0.4698    0.5118    0.5178    0.4156    0.4424    0.5235];
%theta = K
    for i = 1:N
       for j = 1:J
            y(i,j) = mu(theta(:,i),t(j)) + randn * sigma;  
        end
    end
