function [y] = mu(theta,t)
% %% Initialization
% % name of xml file
% appPath=[fileparts(which('example_timecourses_upon_parameter_variation.m')) filesep];
% xml=[appPath 'models' filesep 'PopSim.xml'];
% 
% % Create the structure for the variable parameters
% initStruct=[];
% 
% initStruct=initParameter(initStruct,'PopSim|MoleculeProperties|MyCompound|Lipophilicity','withWarning');
% % Initialize the simulation
% initSimulation(xml,initStruct,'report','none');
% 

% change numerics to get a better time resolution
setSimulationTime(t);

%% Set aim and preferences
% set factors for lipophilicity variation to be applied later
lipophilicity = theta(1); 
value = theta(2);

% % figure initialisation
% ax_handle=getNormFigure(1,1);
% set(gcf, 'CurrentAxes', ax_handle(1));
% 
% lgtxt=cell(length(lipophilicity_offsets),1);
% % set colors for plotting
% colors=[1 0 0; 0 1 0; 0 0 1];
% determine the default value 
%lipophilicity=getParameter('PopSim|MoleculeProperties|MyCompound|Lipophilicity',1);


%% Loop to execute simulations with a varied lipophilicity

%for iOffset=1:length(lipophilicity_offsets)
    % vary parameter of interest
    setParameter(lipophilicity,'IV 10mg|Escitalopram|Lipophilicity',1);
    setParameter(value,'IV 10mg|CYP2D6|Reference concentration',1);
%    setParameter(CLspec,'IV 10mg|Escitalopram-CYP2D6-von Moltke|CLspec/[Enzyme]',1);
    % simulate model with current parameter values
    success=processSimulation;
    if success
        % retrieve spefic timecourse information for your species of interest
        [time,drug]=getSimulationResult('IV 10mg|Organism|PeripheralVenousBlood|Escitalopram|Plasma (Peripheral Venous Blood)',1);
%         % plot timecourse
%         plot(time/60,drug,'color',colors(iOffset,:),'linewidth',2);
%         hold on;
%         % prepare a legend
%         lgtxt{iOffset}=['Lipophilicty: ' num2str(lipophilicity+lipophilicity_offsets(iOffset))];
    else
        error('solver failed')
    end
    y = drug(2);
end
%setAxesScaling(ax_handle(1),'timeUnit','h');
     
% %% Add labels to your figure
% xlabel('time [h]')
% unit=getObserverFormula('PopSim|Organism|PeripheralVenousBlood|MoleculeProperties|MyCompound|OBSPlasma',1,'Property','Unit');
% ylabel(['Plasma concentration [' unit ']'])
% title('Influence of lipophilicity');
% legend(lgtxt,'location','northeast');
% shg;