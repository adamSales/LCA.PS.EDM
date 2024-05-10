library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = 6)

load('dataProcessed/sdatAs.RData')

## oops
sdat$nstud=length(sdat$Z)

fit=stan('code-AS/lca2classPSnoSch.stan',data=sdat,iter=4000,
    warmup=1500,chains=6)

save(fit,sdat,file='results/psFitAs.RData')
