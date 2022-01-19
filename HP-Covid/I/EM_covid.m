%% main func of EM
function [K0 alpha beta mu p lam]=EM_covid(t,emiter,T,bins,Nconf)
% obtain the length of days
N=max(size(t));

% the end time boundary biases estimates (because we can't observe future
% offspring.  so only calculate p_ij when j is extra days before the end
% time T or earlier
extra=7; % temp und: length of predicted days

% p is a matrix storing branching probabilities
p=zeros(N,N);

%initial guesses for parameters
% what is k0
K0=.5*ones(bins,1);

alpha=6;
beta=3;
mu=10;


% number of EM iterations
for k=1:emiter

% E-Step
[p lam]=updatep(t,p,K0,alpha,beta,mu,T,bins,extra);   

% M-Step
[K0 alpha beta mu]=updatepar(K0,t,p,T,bins,extra);

%stabilize weibull; force it under 4 

bup=6;
if(beta>bup)
    beta=bup;
end

% if the beta is higher than bup, then we move back to the parameters obtained 200 iterations ago and stop learning
if(mod(k,200)==0)
    %[k/emiter mu alpha beta K0(end)]
    if (beta < bup)
        last_mu = mu;
        last_alpha = alpha;
        last_beta = beta;
        last_K0 = K0;
    else
        mu = last_mu;
        alpha = last_alpha;
        beta = last_beta;
        K0 = last_K0;
        break
    end
   
end

end


end
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function to estimate branching probabilities

function [p lam]=updatep(t,p,K0,alpha,beta,mu,T,bins,extra)

N=max(size(t));

lam=zeros(N,1);

for i=1:N
    for j=1:(i-1)
        
            
            % probability i triggered by j is proportional to triggering
            % kernel evaluated at inter-point times and distances
            p(i,j)=K0(ceil(bins*j/T))*wblpdf((i-j),alpha,beta)*t(j);
      
    end
    
    %probablity i is background event proportional to mu background rate
    p(i,i)=mu;
    
    % save intensity at each event for analysis 
    lam(i)=sum(p(i,1:i));
    
    %normalize probabilities to sum to 1
    p(i,1:i)=p(i,1:i)/sum(p(i,1:i));
    

end


end
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function to estimate parameters from branching probabilities

function [K0 alpha beta mu]=updatepar(K0,t,p,T,bins,extra)

N=max(size(t));
mu=0;

K0=zeros(size(K0));
Nc=zeros(size(K0));

time_sample=[];
p_sample=[];

for i=1:N

    for j=1:min((i-1),N-extra)

        % parameters are determined by weighted sample mean
        % of inter-point times and square distances        
        
        time_sample=[time_sample; (i-j);]; 
        
        p_sample=[p_sample; p(i,j)*t(i);];
        
        
      
    end
    
    for j=1:(i-1)
        
        K0(ceil(bins*j/T))=K0(ceil(bins*j/T))+p(i,j)*t(i);
        
        
    end
    mu=mu+p(i,i)*t(i);
  
end

[coef,~] = wblfit(time_sample,[],[],p_sample);
alpha=coef(1);

beta=coef(2);


for i=1:N
  Nc(ceil(bins*i/T))=Nc(ceil(bins*i/T))+t(i);
end

K0=K0./(Nc+.000001);

% we don't have a good estimate of K0 at the boundary so just set it
% equal to K0 at previous bin
K0(end)=K0(end-1);

mu=mu/T;

end
