// adapted from https://discourse.mc-stan.org/t/latent-class-model-estimation-with-long-format-data/1799
// "Stan code for wide format"

data {
  int<lower=1> nworked;	//number of worked items--rows of data
  int<lower=1> nprob;                                     // number of items
  int<lower=1> nstud;                                     // number of respondents
  int<lower=1> ncov;					  // number of person-level covariates
  int<lower=0> hint[nworked];                                     // score for obs n
  int<lower=0> err[nworked];
  real ltime[nworked];
  int<lower=1,upper=nprob> prob[nworked];
  int<lower=1,upper=nstud> stud[nworked];

  matrix[nstud,ncov] X;

  real Y[nstud];
  vector[nstud] Z;

  int<lower=1> nschool;
  int<lower=1,upper=nschool> school[nstud];

  vector[3] zeros;

}

parameters {

  real meanTime[2];
  real<lower=0> sigTime[2];
  real effHint[2];
  real effErr[2];

  vector[3] probEff[nprob]; // hint, err, time

  corr_matrix[3] OmegaProb;
  vector<lower=0>[3] sigProb;

  real alpha;

  vector[nstud] studEff;
  real<lower=0> sigStud;

  vector[ncov] beta;

  ordered[2] cHint;
  ordered[2] cErr;

  real gamma0;
  real tzero;
  real lambda;
  real tone;
  vector[ncov] gamma;
  real schoolEff[nschool];
  real<lower=0> sigY;
  real<lower=0> sigSchool;

}
transformed parameters {
//model{
  
  cov_matrix[3] SigmaProb=quad_form_diag(OmegaProb, sigProb);

  
}

model{
 real yhat[nstud];
 vector[nstud] nu=inv_logit(alpha+X*beta+studEff);

  for(i in 1:nstud)
  	yhat[i]=gamma0+tzero*Z[i]+lambda*nu[i]+tone*Z[i]*nu[i]+X[i,]*gamma+schoolEff[school[i]];

// priors
 meanTime~normal(0,5);
 sigTime~normal(0,5);
 effHint~normal(0,5);
 effErr~normal(0,5);
 sigProb~normal(0,1);
 //sigStud~normal(0,1);
 to_vector(beta)~normal(0,1);
schoolEff~normal(0,sigSchool);
tzero~std_normal();
tone~std_normal();
lambda~std_normal();
gamma~normal(0,5);

sigProb~normal(0,3);
sigStud~normal(0,3);
sigY~normal(0,5);
sigSchool~normal(0,5);


 studEff~normal(0,sigStud);

 probEff~multi_normal(zeros,SigmaProb);

 for(w in 1:nworked)
   target += log_sum_exp(
    log(nu[stud[w]])+
    ordered_logistic_lpmf(hint[w]|probEff[prob[w]][1]+effHint[1],cHint)+
    ordered_logistic_lpmf(err[w]|probEff[prob[w]][2]+effErr[1],cErr)+
    normal_lpdf(ltime[w]| probEff[prob[w]][3]+meanTime[1],sigTime[1]),
    log(1-nu[stud[w]])+
    ordered_logistic_lpmf(hint[w]|probEff[prob[w]][1]+effHint[2],cHint)+
    ordered_logistic_lpmf(err[w]|probEff[prob[w]][2]+effErr[2],cErr)+
    normal_lpdf(ltime[w]| probEff[prob[w]][3]+meanTime[2],sigTime[2])
   );


 Y~normal(yhat,sigY);

}
//last line