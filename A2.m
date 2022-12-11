%Q3(a
out = est_gumbel(atlantic); %MLE to estimate parameter
beta = out(1);
mu = out(2);

%Q3(b
u = rand(582,1); % sample from uniform distribution
x = mu - beta*log(-log(u)); %simulated draws with inverse method
qqplot(x, atlantic) % compare the result
title('approximately agree')
xlabel('simulated data')
ylabel('atlantic data');

%Q3(c
B = 10000;  % the number of bootstrap simulations
boots_beta = zeros(B,1); % replace = 0 for sampling without replacement
boots_mu = zeros(B,1);
n = 582; %the number of data
rng(123);
for ii=1:B
    % simulate bootstrapped data
    u = rand(n,1);
    x = mu - beta*log(-log(u));
    out = est_gumbel(x);
    boots_beta(ii) = out(1);
    boots_mu(ii) = out(2);
end
a = 0.05; % set significant level
beta_lower = prctile(boots_beta,a/2); % calculate upper and lower bounder with prcentile method
beta_upper = prctile(boots_beta,(1-a)/2);
mu_lower = prctile(boots_mu,a/2);
mu_upper = prctile(boots_mu,(1-a)/2);


%Q3(d
T = 3*14*100; % T value
year_return = boots_mu - boots_beta*log(-log(1-(1/T))); %put the estiamted parameter in (c) with T
year_return_lower = prctile(year_return,a/2); % calculate upper and lower bounder with prcentile method
year_return_upper = prctile(year_return,(1-a)/2);

%Q3(e


%Q3(f
non_boots_beta = zeros(B,1); % replace = 0 for sampling without replacement
non_boots_mu = zeros(B,1);
replace = 1; % replace = 0 for sampling without replacement
rng(123);
for ii=1:B
    % simulate bootstrapped data
    x = randsample(atlantic,n,replace);
    out = est_gumbel(x);
    non_boots_beta(ii) = out(1);
    non_boots_mu(ii) = out(2);
end

non_beta_lower = prctile(non_boots_beta,a/2); % calculate upper and lower bounder with prcentile method
non_beta_upper = prctile(non_boots_beta,(1-a)/2);
non_mu_lower = prctile(non_boots_mu,a/2);
non_mu_upper = prctile(non_boots_mu,(1-a)/2);