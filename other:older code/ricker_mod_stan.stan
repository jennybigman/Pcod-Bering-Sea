

data {
int N;
vector[N] r; // recruits
vector[N] ssb; // spawning stock biomass
}

parameters {
real a; 
real b; 
real sigma;
}

transformed parameters {
vector[N] logmu_r;
vector[N] resid;


for(i in 1:N) {
log(mu_r[i]) = log(a) * log(ssb[i]) * -b * ssb[i];
resid[i] = r[i] - mu_r[i];
}}

model {

for(i in 1:N) {
r[i] ~ normal(mu_r[i], sigma);
}
}

generated quantities {

vector[N] log_lik;

for(i in 1:N) {
log_lik[i] =
normal_lpdf(r[i] | mu_r[i], sigma);
}
}
