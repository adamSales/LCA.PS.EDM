library(stargazer)
library(rstan)
library(dplyr)
library(ggplot2)
library(xtable)

print(load('psFitAs.RData'))

mod <- fit
rm(fit)

rhat <- function(mat){
    B <- var(colMeans(mat))
    W <- mean(apply(mat,2,var))
    sqrt(((nrow(mat)-1)/nrow(mat)*W+B)/W)
}

asDrawMat=function(ar){
  iter=dim(ar)[1]
  chains=dim(ar)[2]
  npars=dim(ar)[3]
  out=matrix(0,nrow=iter*chains,ncol=dim(ar)[3])

  colnames(out)=dimnames(ar)[[3]]
  
  for(i in 1:chains)
    out[((i-1)*iter+1):(iter*i),] <- ar[,i,]
  
  out
}

stars=function(pval)
  ifelse(pval<0.001,"***",
  ifelse(pval<0.01,"**",
  ifelse(pval<0.05,"*",
  ifelse(pval<0.1,".",""))))


disp <- function(x){ 
  pval=2*pnorm(-abs(mean(x)/sd(x)))
  paste0(
                        sprintf("%.3f", round(mean(x),3)),stars(pval),
                        ' (',sprintf("%.3f", round(sd(x),3)),')'
                    )
}



traceplot(mod,par='lp__')
traceplot(mod,par='beta')

lp=extract(mod,par='lp__',permute=FALSE)

rhat(lp)

apply(lp,c(2,3),mean)
### chain 4 is weird

rhat(lp[,-4,])

## measurement parameters
meas0 <- rstan::extract(mod,par=c('meanTime','sigTime','effErr','effHint','cHint','cErr'),permute=FALSE)

measd0 <- tibble(
  draw=as.vector(meas0),
  iter=rep(1:dim(meas0)[1],prod(dim(meas0)[2:3])),
  chain=rep(rep(dimnames(meas0)$chains,each=dim(meas0)[1]),dim(meas0)[3]),
  parameter=rep(dimnames(meas0)$parameters,each=prod(dim(meas0)[1:2]))
)

ggplot(measd0,aes(iter,draw,color=chain,group=chain))+
  geom_line()+
  facet_wrap(~parameter,scales="free_y")



## some reverse coding necessary....
(means <- apply(meas0[,-4,],c(2,3),mean)%>%
  as.data.frame()%>%select(-starts_with('c')))

## for Appendix A
xtable(means)

## want sigTime1 \approx .88
## chains where it's .53 should get reversed
revCode <- rownames(means)[which(trunc(means[,'sigTime[1]']*10)==5)]

meas1 <- meas0[,-4,]
meas <- meas1
meas[,revCode,] <- meas[,revCode,c(2,1,4,3,6,5,8,7,9:12)]

apply(meas,c(2,3),mean)

## rhats


    
xtable(
    rbind(
        Original=apply(meas0,3,rhat),
        No4=apply(meas1,3,rhat),
        Relabeled=apply(meas,3,rhat)
        ))


## new traceplot 


measd <- tibble(
    draw=as.vector(meas),
    iter=rep(1:dim(meas)[1],prod(dim(meas)[2:3])),
    chain=rep(rep(dimnames(meas)$chains,each=dim(meas)[1]),dim(meas)[3]),
    parameter=rep(dimnames(meas)$parameters,each=prod(dim(meas)[1:2]))
)

ggplot(measd,aes(iter,draw,color=chain,group=chain))+
    geom_line()+
    facet_wrap(~parameter,scales="free_y")
ggsave('newtpAsPs.jpg')

effs0=extract(mod,par=c("gamma0","lambda","tzero","tone"),permute=FALSE)

apply(effs0,c(2,3),mean)

##  g0+t0z+lam(1-nu)+t1*Z*(1-nu)
## =g0+t0z+lam-lam*nu+t1*Z-t1*Z*nu
## =g0+lam-lam*nu+(t0+t1)Z-t1*Z*nu

effs1=effs0[,-4,]
effs=effs1
effs[,revCode,'gamma0']=effs[,revCode,'gamma0']+effs[,revCode,'lambda']
effs[,revCode,'lambda']=-effs[,revCode,'lambda']
effs[,revCode,'tzero']=effs[,revCode,'tzero']+effs[,revCode,'tone']
effs[,revCode,'tone']=-effs[,revCode,'tone']

apply(effs,3,rhat)

apply(effs,c(2,3),mean)

effs=asDrawMat(effs)


gamma=extract(mod,c('gamma'),permute=FALSE)
gamma=gamma[,-4,]

apply(gamma,3,rhat)

gamma=asDrawMat(gamma)

colnames(gamma)=colnames(sdat$X)

gammaShort=gamma[,-grep('SchIDPre',colnames(gamma))]

outregCoef=cbind(effs,gammaShort)

outregStats=cbind(
  Est=colMeans(outregCoef),
  SE=apply(outregCoef,2,sd))

outregStats=cbind(
  outregStats,
  p=2*pnorm(-abs(outregStats[,1]/outregStats[,2])))

tab2 <- measd%>%
  filter(!startsWith(parameter,'c'))%>%
    mutate(par=substr(parameter,1,nchar(parameter)-3),
           par=factor(par,levels=unique(par)),
           class=substr(parameter,nchar(parameter)-1,nchar(parameter)-1)
           )%>%          
    group_by(par)%>%
    summarize(
        `State 1`=disp(draw[class=='1']),
        `State 2`=disp(draw[class=='2']),
        `Difference`=disp(draw[class=='1']-draw[class=='2'])
    )

cTab <- measd%>%
  filter(startsWith(parameter,'c'))%>%
  group_by(parameter)%>%
  summarize(`State 1`=disp(draw))
  

### probabilities
nuParams=extract(mod,c('alpha','beta','studEff'),permute=FALSE)
nuParams=nuParams[,-4,]

X<- sdat$X
nu=lapply(1:dim(nuParams)[2],function(cc){
  PP=nuParams[,cc,]
  beta=PP[,paste0('beta[',1:sdat$ncov,']')]
  alpha=PP[,'alpha']
  studEff=PP[,grep('studEff',colnames(PP))]
  plogis(sweep(beta%*%t(X)+studEff,1,alpha,"+"))})

nuAr=array(0,c(nrow(nu[[1]]),length(nu),ncol(nu[[1]])),dimnames=
list(iter=NULL,chains=dimnames(nuParams)$chains,parameters=paste0('studEff[',1:747,']')))
for(cc in 1:length(nu)) nuAr[,cc,]=nu[[cc]]

nuAr[,revCode,]=1-nuAr[,revCode,]

### probabilities
#nu <- rstan::extract(mod,par='nu',permute=FALSE)
#nu[,revCode,] <- 1-nu[,revCode,]
#quantile(apply(nu,3,rhat))

meanNu <- #apply(nu,c(1,2),function(x) mean(x[sdat$stud]))
  colMeans(asDrawMat(nuAr))

#tibble(draw=as.vector(meanNu),
#       iter=rep(1:nrow(meanNu),ncol(meanNu)),
#       chain=rep(colnames(meanNu),each=nrow(meanNu)))%>%
#    ggplot(aes(iter,draw,color=chain,group=chain))+geom_line()+
#    ggtitle(expression(paste('E',nu)))


tab2 <- bind_rows(
    tab2,
    tibble(par='Probability',`State 1`=disp(meanNu),`State 2`=disp(1-meanNu))
)

tab2$par[1:4] <- c('Time (mean)','Time (SD)','Error','Hint')
tab2$Notation <- c('$\\mu_{1m}$','$\\sigma_{m}$','$\\mu_{2m}$','$\\mu_{3m}$',' ')

tab2 <- tab2[,c(1,5,2:4)]

print(xtable(tab2),include.rownames=FALSE,
      sanitize.text.function = function(x) x,
      floating=FALSE)

## coef table
beta <- rstan::extract(mod,par=c('alpha','beta'),permute=FALSE)

beta <- beta[,-4,]

beta[,revCode,] <- -beta[,revCode,]
beta <- -beta ## parameterized model opposite like

apply(beta,3,rhat)

dimnames(beta)$parameters[-1] <- colnames(sdat$X)
dimnames(beta)$parameters[1] <- "(Intercept)"


betaShort <- beta[,,-grep('SchIDPre',dimnames(beta)$parameters)]


tab3 <- tibble(
    Covariate=dimnames(betaShort)$parameters,
    Est=sprintf("%.3f", round(apply(betaShort,3,mean),3)),
    SE=sprintf("%.3f", round(apply(betaShort,3,sd),3)),
    pval=sprintf("%.3f", round(apply(betaShort,3,function(x) 2*min(mean(x<0),mean(x>0))),3))
     )
    

ddd=matrix(rnorm(nrow(tab3)*100),nrow=100)
`logit(\\pi)`=rnorm(100)

colnames(ddd)=tab3$Covariate
fakeMod=lm(`logit(\\pi)`~ddd-1)

ddd=matrix(rnorm(nrow(outregStats)*100),nrow=100)
colnames(ddd)=rownames(outregStats)
colnames(ddd)[1] <- "(Intercept)"
Posttest=rnorm(100)


fakeModOut=lm(Posttest~ddd-1)


stargazer(
  fakeMod,fakeModOut,
  coef=list(unname(as.numeric(tab3$Est)),unname(outregStats[,'Est'])),
  se=list(as.numeric(tab3$SE),unname(outregStats[,'SE'])),
  t=list(as.numeric(tab3$Est)/as.numeric(tab3$SE),unname(outregStats[,'Est']/outregStats[,'SE'])),
  omit.stat='all',star.cutoffs=c(0.05,0.01,0.001) )


### plot effects
range(meanNu)

plotEffDat <- tibble(
  pp=seq(0.01,.99,0.01),
  tau=mean(effs[,'tzero'])+pp*mean(effs[,'tone']),
  se=sqrt(var(effs[,'tzero'])+pp^2*var(effs[,'tone'])+2*pp*cov(effs[,'tzero'],effs[,'tone'])),
  RCT="ASSISTments"
) 


load('plotEffcta1.RData')

plotEffDatCT$RCT="CTA1"

ped=rbind(plotEffDat,plotEffDatCT)

line=tibble(
  slope=c(
    mean(effs[,'tone']),
    mean(effsCT[,'tone'])),
  intercept=c(
    mean(effs[,'tzero']),
    mean(effsCT[,'tzero'])),
  RCT=c('ASSISTments','CTA1'))


ggplot(ped,aes(pp,tau,ymin=tau-2*se,ymax=tau+2*se,color=RCT,fill=RCT))+
  geom_ribbon(alpha=0.2)+
  geom_line(size=2)+
geom_hline(yintercept=0,
          linetype='dotted')+
xlab(expression(pi))+
ylab("Treatment Effect")
ggsave('effectPlot.pdf',height=4,width=5)


varComp <- summary(mod,par=c('sigProb','OmegaProb','sigStud'),probs=c(0.025,0.975))$summary%>%round(3)


## variance of fitted values
yh <- sd(sdat$X%*%apply(beta,3,mean))



