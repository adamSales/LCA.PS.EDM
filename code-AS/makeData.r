library(tidyverse)

probPartDat <- read_csv('ASSISTments_ProblemLevelData_Adam.csv')%>%
  mutate(
    prob=paste0(problem_id,problem_part),
    time=log(total_time),
    nerrs1=cut(errors,c(-1,0,1,Inf),labels=FALSE),
    nhints1=cut(hint_count,c(-1,0,2,Inf),labels=FALSE))

##                                         #pretest, post test, race, sex, free-lunch, ESL


## probDat <- probPartDat%>%
##   group_by(problem_id)%>%
##   mutate(npart=n_distinct(problem_part))%>%
##   group_by(problem_id,StuID,npart)%>%
##   summarize(across(c(total_time,hint_count,errors),sum))

## probErrs <- probDat%>%
##   group_by(problem_id,npart)%>%
##   summarize(n=n(),err=mean(errors))

## min(probErrs$err)
## probErrs%>%
##   ggplot(aes(n,err))+geom_point()+geom_smooth()

## probErrs%>%
##   group_by(npart)%>%summarize(err=mean(err),nprob=n())%>%
##   ggplot(aes(npart,err,size=nprob))+geom_point()+geom_smooth()

## probErrs%>%ggplot(aes(err))+geom_histogram()



load('~/OneDrive - Worcester Polytechnic Institute (wpi.edu)/hintsAndFeedback/hintsFeedback/data/feedbackData.RData')

dat$esol <- ifelse(rowSums(dat[,c("EL_PARENT_DENIED","ESOL_C","ESOL_FORMER","ESOL")],na.rm=TRUE)>0,1,0)

dat <- dat%>%
  filter(hasPosttest)


X <- model.matrix(StuID~
                    pre_total_math_score+
                    Scale_Score5imp+pre_MA_total_scoreimp + pre_MSE_total_scoreimp +
                    ScaleScore5miss+logTime+race+esol+MALE+
                    as.factor(SchIDPre),data=dat)[,-1]


dat$StudID <- as.numeric(as.factor(dat$StuID))

probPartDat <- inner_join(select(dat,StuID,StudID),probPartDat,by="StuID")


sdat <- with(probPartDat,
             list(
               prob=as.numeric(as.factor(prob)),
               err=nerrs1,#as.numeric(err>0),
               hint=nhints1,#as.numeric(hint>0),
               ltime=scale(time)[,1],
               stud=StudID,
               nprob=n_distinct(prob),
               nworked=length(prob),
               zeros=c(0,0,0),
               X=X,
               Z=dat$Z,
               Y=dat$post_total_math_score,
               ncov=ncol(X)
             )
           )

save(sdat,file='sdatAs.RData')
