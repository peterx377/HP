% Variable
StartDate = datetime(2020,3,23);
EndDate = datetime(2021,08,31);

% retrieve Covid data & state names
covid = csvread('../../data/I/state_covid_confirmed-7ma.csv',1,0);
DateRange = StartDate:EndDate;
stateData = fileread('../../data/stateName.txt');
% N1 number of states(numOfrows)
% N2 number of days(numOfCol)
[N1 N2]=size(covid);
bins=fix(max(size(DateRange))/10);
T=N2;
output1 = [1:bins];
output2 = ["scale", "shape", "mu"];
output3 = [];
% initialize mu, alpha, beta (3*1 for each state)
mus=zeros(N1,1);
alphas=zeros(N1,1);
betas=zeros(N1,1);
K1s=zeros(N1,bins);  
% run algo on each each state
for i=1:N1
    if i == 12 || i == 27
        y=covid(i,:);
        Nt=y(1:N2);
        % a1 is scale parameter of weibull
        % b1 is shape parameter
        [K1 a1 b1 mu1 p lam]=EM_covid(Nt,2000,T,bins,200);   
        output1=[output1; K1';];
        output2=[output2;[a1 b1 mu1];];
        output3=[output3; lam'];
        output3=[output3; Nt];
    end


    
end
csvwrite("../../data/I/param/R0_statez.csv", output1);  %/complete
csvwrite("../../data/I/param/simz.csv", output3);  %/complete
writematrix(output2,'../../data/I/param/weibull_paramz.csv'); %/complete


