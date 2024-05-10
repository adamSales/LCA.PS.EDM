library(rstan)
library(dplyr)
library(ggplot2)
library(xtable)
library(stargazer)

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

load('dataProcessed/smallSdatPS.RData')

load('results/psFit.RData')

mod <- fit
rm(fit)

traceplot(mod,par='lp__')
traceplot(mod,par='beta')

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
#means <- apply(meas0[,-4,],c(2,3),mean)%>%
#  as.data.frame()%>%select(-starts_with('c'))

## for Appendix A
#xtable(means)

## want sigTime \approx .76
## chains where it's .84 should get reversed
#revCode <- which(trunc(means[,'sigTime[1]']*10)==8)

meas <- meas0
#meas[,revCode,] <- meas[,revCode,c(2,1,4,3,6,5,8,7,9:12)]

#apply(meas,c(2,3),mean)

## rhats

rhat <- function(mat){
    B <- var(colMeans(mat))
    W <- mean(apply(mat,2,var))
    sqrt(((nrow(mat)-1)/nrow(mat)*W+B)/W)
}

xtable(
    rbind(
        Original=apply(meas0,3,rhat),
        Relabeled=apply(meas,3,rhat)
        ))

meas=meas0
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


disp <- function(x){
  pval=2*pnorm(-abs(mean(x)/sd(x)))
  paste0(
                        sprintf("%.3f", round(mean(x),3)),stars(pval),
                        ' (',sprintf("%.3f", round(sd(x),3)),')'
                    )
}



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


## calculate nu
nuParams=extract(mod,c('alpha','beta','studEff'))
X<- sdat$X
nu=with(nuParams,
  plogis(sweep(beta%*%t(X)+studEff,1,nuParams$alpha,"+")))


### probabilities
#nu <- rstan::extract(mod,par='nu',permute=FALSE)
#nu[,revCode,] <- 1-nu[,revCode,]
#quantile(apply(nu,3,rhat))

meanNu <- #apply(nu,c(1,2),function(x) mean(x[sdat$stud]))
  colMeans(nu)
rhat(meanNu)



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
## coef table
beta <- rstan::extract(mod,par=c('alpha','beta'),permute=FALSE)

beta <- -beta ## parameterized model opposite like

apply(beta,3,rhat)

dimnames(beta)$parameters[-1] <- colnames(sdat$X)
dimnames(beta)$parameters[1] <- "(Intercept)"

betaShort <- beta[,,-grep('pair',dimnames(beta)$parameters)]


tab3 <- tibble(
    Covariate=dimnames(betaShort)$parameters,
    Est=sprintf("%.3f", round(apply(betaShort,3,mean),3)),
    SE=sprintf("%.3f", round(apply(betaShort,3,sd),3)),
    pval=sprintf("%.3f", round(apply(betaShort,3,function(x) 2*min(mean(x<0),mean(x>0))),3))
     )


effs=extract(mod,par=c("gamma0","lambda","tzero","tone"),permute=FALSE)
effs=asDrawMat(effs)

gamma=extract(mod,c('gamma'),permute=FALSE)
gamma=asDrawMat(gamma)

colnames(gamma)=colnames(sdat$X)

gammaShort=gamma[,-grep('pair',colnames(gamma))]

outregCoef=cbind(effs,gammaShort)

outregStats=cbind(
  Est=colMeans(outregCoef),
  SE=apply(outregCoef,2,sd))

outregStats=cbind(
  outregStats,
  p=2*pnorm(-abs(outregStats[,1]/outregStats[,2])))


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

varComp <- summary(mod,par=c('sigProb','OmegaProb','sigStud'),probs=c(0.025,0.975))$summary%>%round(3)


## variance of fitted values
yh <- sd(sdat$X%*%apply(beta,3,mean))


range(meanNu)

plotEffDatCT <- tibble(
  pp=seq(0.04,.75,0.01),
  tau=mean(effs[,'tzero'])+pp*mean(effs[,'tone']),
  se=sqrt(var(effs[,'tzero'])+pp^2*var(effs[,'tone'])+2*pp*cov(effs[,'tzero'],effs[,'tone']))
)

effsCT=effs

save(plotEffDatCT,effsCT,file='results/plotEffcta1.RData')

ggplot(plotEffDat,aes(pp,tau,ymin=tau-2*se,ymax=tau+2*se))+
  geom_ribbon(fill='grey')+
  geom_abline(slope=mean(effs[,'tone']),intercept=mean(effs[,'tzero']))



#### spline plot
