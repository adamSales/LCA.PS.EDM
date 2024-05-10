library(tidyverse)

#print(load("../../../../CT/data/problemLevelUsageData/probLevelData.RData"))
print(load("~/OneDrive - Worcester Polytechnic Institute (wpi.edu)/CT/data/RANDstudyData/HSdata.RData"))

## probDat <- filter(x,field_id%in%dat$field_id[dat$year==1])
## es1Prob <- unique(x$Prob1[x$unit=='es_01'])
## probDat <- filter(probDat,Prob1%in%es1Prob)

dfs <- load('../CTA2007C_ES1.RData')
probDat <- bind_rows(data_es1)#oneDay #bind_rows(data_prob_es01_sday)


mean(dat$field_id[dat$treatment==1&dat$year==1]%in%probDat$field_id)

### remove schools and matched pairs with little to no ES1 data
dat%>%
  filter(treatment==1,year==1)%>%
  group_by(schoolid2)%>%
  summarize(es1Dat=mean(field_id%in%probDat$field_id),nMiss=sum(!field_id%in%probDat$field_id))%>%
  arrange(es1Dat)%>%
  print(n=Inf)

dat <- dat%>%
  filter(year==1)%>%
  group_by(pair)%>%
  mutate(es1Dat=mean(field_id[treatment==1]%in%probDat$field_id))%>%
  filter(es1Dat>0.25)%>%
  ungroup()%>%
  droplevels()

probDat <- filter(probDat,field_id%in%dat$field_id)

mean(dat$field_id[dat$treatment==1]%in%probDat$field_id)

dat$field_id <- as.factor(dat$field_id)
probDat$field_id <- factor(probDat$field_id,levels=levels(dat$field_id))

dat$studID <- as.numeric(dat$field_id)
probDat$studID <- as.numeric(probDat$field_id)

### test to see if "stud" matches between datasets
left_join(select(probDat, slctudID,field_id),select(dat,studID,field_id),by="studID")%>%
  summarize(all(field_id.x==field_id.y))


probDat <- probDat%>%
#    filter(nerrs1<103,nhints1<50)%>%
    mutate(across(c(nhints1,nerrs1),~cut(.,c(-1,0,1,Inf),labels=FALSE)))


dat <- dat%>%arrange(studID)

X <- model.matrix(~grade+race+sex+frl+splines::ns(xirt,3)+esl+pair+xirtMIS+frlMIS,data=dat)[,-1]

Y <- dat$Y

Z <- dat$treatment

sdat <- with(probDat,
             list(
               prob=as.numeric(as.factor(kcs)),
               err=nerrs1,#as.numeric(err>0),
               hint=nhints1,#as.numeric(hint>0),
               ltime=scale(ltime)[,1],
               stud=studID,
               nprob=n_distinct(kcs),
               nstud=n_distinct(field_id),
               nworked=length(kcs),
               zeros=c(0,0,0),
               X=X,
               ncov=ncol(X),
               Y=Y,
               Z=Z,
               school=as.numeric(dat$schoolid2),
               nschool=max(school)
             )
           )

#mod <- stan_model('lcaPoisCov.stan')

## if(!small) save(sdat,file='lcaDatMod.RData')
## if(small)
save(sdat,file='smallSdatPS.RData')
