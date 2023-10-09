library(knitr)
library(rvest)
library(tidyverse)
library(stringr)
library(ggplot2)
library(e1071)
library(rpart)
library(randomForest)
library(caret)
library(gbm)
library(neuralnet)
simNBAPlayoff<- function(modtype){
source("/Users/seanfloersch/FloStrength/FloStrengthApp/NBAFunctions.R")
NBAResults<-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/ComNBAResults")
NBATeams <- getNBATeam(2023)
NBAPlayers <- getNBAplayers(2023)
NBASched <- getNBAsched(NBATeams,NBAResults)
NBAStreaks <- getStreaks(NBATeams, NBAResults)
PowerRanking <- getNBArank(NBAPlayers, NBATeams, NBASched, NBAPredTeam,NBAStreaks)
moddf<-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBAModel")

############################################################################
############################################################################
############################################################################
#Set MODEL TYPE (Log, RF, SVM, GBM, NN, ENS)
model = modtype
############################################################################
############################################################################
############################################################################
############################################################################
#Set up models and df
Team<- c("MIL","BOS","PHI","CLE","NYK","BRK","ATL","MIA","DEN","MEM","SAC","PHO","LAC","GSW","LAL","MIN")
Seed <- c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8)
Match <- c("E1","E2","E3","E4","E4","E3","E2","E1","W1","W2","W3","W4","W4","W3","W2","W1")
Conference<- c(rep("E",8),rep("W",8))
podf<- data.frame(Team, Seed, Conference,Match)
modeldf<-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBAModel")%>%
  mutate(Awaylast3games= Awaylast3games/AwaySched)%>%
  mutate(Awaylast5games= Awaylast5games/AwaySched)%>%
  mutate(Awaylast7games= Awaylast7games/AwaySched)%>%
  mutate(Awaylast10games= Awaylast10games/AwaySched)%>%
  mutate(Homelast3games= Homelast3games/HomeSched)%>%
  mutate(Homelast5games= Homelast5games/HomeSched)%>%
  mutate(Homelast7games= Homelast7games/HomeSched)%>%
  mutate(Homelast10games= Homelast10games/HomeSched)%>%
  mutate(last3Diff=Awaylast3games-Homelast3games)%>%
  mutate(last5Diff=Awaylast5games-Homelast5games)%>%
  mutate(last7Diff=Awaylast7games-Homelast7games)%>%
  mutate(last10Diff=Awaylast10games-Homelast10games)

logmod <- glm(AWin~last3Diff+last5Diff+last7Diff+last10Diff+SchedDiff+TeamDiff+PlayerDiff, data = modeldf,family = binomial)
svmmod = svm(formula = AWin~AwayPlayer+HomePlayer+AwayTeamStr+HomeTeamStr+Awaylast3games+Awaylast5games+Awaylast7games+Awaylast10games+AwaySched+HomeSched+Homelast3games+Homelast5games+Homelast7games+Homelast10games, data = modeldf, type = 'C-classification', kernel = 'linear')
rfmod <- randomForest(AWin~AwayPlayer+HomePlayer+AwayTeamStr+HomeTeamStr+Awaylast3games+Awaylast5games+Awaylast7games+Awaylast10games+AwaySched+HomeSched+Homelast3games+Homelast5games+Homelast7games+Homelast10games, data = modeldf)
nnmod  <- neuralnet(AWin~AwayPlayer+HomePlayer+AwayTeamStr+HomeTeamStr+Awaylast3games+Awaylast5games+Awaylast7games+Awaylast10games+AwaySched+HomeSched+Homelast3games+Homelast5games+Homelast7games+Homelast10games, data = modeldf, hidden=1,act.fct = "logistic",linear.output = FALSE)
gbmmod <- gbm(AWin~AwayPlayer+HomePlayer+AwayTeamStr+HomeTeamStr+Awaylast3games+Awaylast5games+Awaylast7games+Awaylast10games+AwaySched+HomeSched+Homelast3games+Homelast5games+Homelast7games+Homelast10games, data = modeldf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 10000) 
#R1
AwayTeams <- podf %>%
  group_by(Match)%>%
  arrange(-Seed)%>%
  slice(1)%>%
  ungroup()
HomeTeams <- podf %>%
  group_by(Match)%>%
  arrange(Seed)%>%
  slice(1)%>%
  ungroup()
AwayTeams<- left_join(AwayTeams, PowerRanking, by = "Team")%>%
  rename("Sched"="SchedRating","TeamStr"="TeamStrength","Player"="PlayerStrength")%>%
  mutate(last3games= last3games/Sched)%>%
  mutate(last5games= last5games/Sched)%>%
  mutate(last7games= last7games/Sched)%>%
  mutate(last10games= last10games/Sched)
HomeTeams<- left_join(HomeTeams, PowerRanking, by = "Team")%>%
  rename("Sched"="SchedRating","TeamStr"="TeamStrength","Player"="PlayerStrength")%>%
  mutate(last3games= last3games/Sched)%>%
  mutate(last5games= last5games/Sched)%>%
  mutate(last7games= last7games/Sched)%>%
  mutate(last10games= last10games/Sched)
for (i in c(1:3,5:13)){
  colnames(AwayTeams)[i]= str_c("Away",colnames(AwayTeams)[i])
}
for (i in c(1:3,5:13)){
  colnames(HomeTeams)[i]= str_c("Home",colnames(HomeTeams)[i])
}
predAct = left_join(AwayTeams, HomeTeams, by = "Match")%>%
  mutate(last3Diff=Awaylast3games-Homelast3games)%>%
  mutate(last5Diff=Awaylast5games-Homelast5games)%>%
  mutate(last7Diff=Awaylast7games-Homelast7games)%>%
  mutate(last10Diff=Awaylast10games-Homelast10games)%>%
  mutate(TeamDiff=AwayTeamStr-HomeTeamStr)%>%
  mutate(SchedDiff=AwaySched-HomeSched)%>%
  mutate(PlayerDiff=AwayPlayer-HomePlayer)
set.seed(1212)
logpred1 <- predict(logmod, newdata = predAct, type = "response")
logpred <- ifelse(logpred1>.5, 1,0)
svmpred <- as.vector(predict(svmmod, newdata = predAct, type = "response"))
rfpred<-as.vector(predict(rfmod, newdata = predAct))
rfpred=ifelse(rfpred>0.5,1,0)
nnpred<-(compute(nnmod,predAct))$net.result
nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, predAct, type = "response")
gbmpred=ifelse(gbmpred>0.5,1,0)

predAct <- predAct %>%
  mutate(logpred = logpred) %>%
  mutate(svmpred = as.numeric(svmpred)) %>%
  mutate(nnpred = nnpred) %>%
  mutate(rfpred = rfpred)%>%
  mutate(gbmpred = gbmpred) %>%
  mutate(Ensemble = ifelse((logpred+svmpred+nnpred+rfpred+gbmpred)>2.5,1,0))
if (model=="Log"){
  predAct <- predAct%>%
    mutate(Team = ifelse(logpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(logpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(logpred==1,AwaySeed,HomeSeed))
}
if (model=="SVM"){
  predAct <- predAct%>%
    mutate(Team = ifelse(svmpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(svmpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(svmpred==1,AwaySeed,HomeSeed))
    
}
if (model=="NN"){
  predAct <- predAct%>%
    mutate(Team = ifelse(nnpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(nnpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(nnpred==1,AwaySeed,HomeSeed))
    
}
if (model=="RF"){
  predAct <- predAct%>%
    mutate(Team = ifelse(rfpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(rfpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(rfpred==1,AwaySeed,HomeSeed))
    
}
if (model=="GBM"){
  predAct <- predAct%>%
    mutate(Team = ifelse(gbmpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(gbmpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(gbmpred==1,AwaySeed,HomeSeed))
    
}
if (model=="ENS"){
  predAct <- predAct%>%
    mutate(Team = ifelse(Ensemble==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(Ensemble==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(Ensemble==1,AwaySeed,HomeSeed))
    
}
predAct <- predAct %>%
  select(Team, Seed, Conference,Match)%>%
  mutate(Match = ifelse(Match %in% c("E1","E4"),"E1",Match))%>%
  mutate(Match = ifelse(Match %in% c("E2","E3"),"E2",Match))%>%
  mutate(Match = ifelse(Match %in% c("W1","W4"),"W1",Match))%>%
  mutate(Match = ifelse(Match %in% c("W2","W3"),"W2",Match))

r1win<- predAct %>%
  select(Team, Seed, Conference,Match)
AwayTeams <- predAct %>%
  group_by(Match)%>%
  arrange(-Seed)%>%
  slice(1)%>%
  ungroup()
HomeTeams <- predAct %>%
  group_by(Match)%>%
  arrange(Seed)%>%
  slice(1)%>%
  ungroup()
AwayTeams<- left_join(AwayTeams, PowerRanking, by = "Team")%>%
  rename("Sched"="SchedRating","TeamStr"="TeamStrength","Player"="PlayerStrength")%>%
  mutate(last3games= last3games/Sched)%>%
  mutate(last5games= last5games/Sched)%>%
  mutate(last7games= last7games/Sched)%>%
  mutate(last10games= last10games/Sched)
HomeTeams<- left_join(HomeTeams, PowerRanking, by = "Team")%>%
  rename("Sched"="SchedRating","TeamStr"="TeamStrength","Player"="PlayerStrength")%>%
  mutate(last3games= last3games/Sched)%>%
  mutate(last5games= last5games/Sched)%>%
  mutate(last7games= last7games/Sched)%>%
  mutate(last10games= last10games/Sched)
for (i in c(1:3,5:13)){
  colnames(AwayTeams)[i]= str_c("Away",colnames(AwayTeams)[i])
}
for (i in c(1:3,5:13)){
  colnames(HomeTeams)[i]= str_c("Home",colnames(HomeTeams)[i])
}
predAct = left_join(AwayTeams, HomeTeams, by = "Match")%>%
  mutate(last3Diff=Awaylast3games-Homelast3games)%>%
  mutate(last5Diff=Awaylast5games-Homelast5games)%>%
  mutate(last7Diff=Awaylast7games-Homelast7games)%>%
  mutate(last10Diff=Awaylast10games-Homelast10games)%>%
  mutate(TeamDiff=AwayTeamStr-HomeTeamStr)%>%
  mutate(SchedDiff=AwaySched-HomeSched)%>%
  mutate(PlayerDiff=AwayPlayer-HomePlayer)
set.seed(1212)
logpred1 <- predict(logmod, newdata = predAct, type = "response")
logpred <- ifelse(logpred1>.5, 1,0)
svmpred <- as.vector(predict(svmmod, newdata = predAct, type = "response"))
rfpred<-as.vector(predict(rfmod, newdata = predAct))
rfpred=ifelse(rfpred>0.5,1,0)
nnpred<-(compute(nnmod,predAct))$net.result
nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, predAct, type = "response")
gbmpred=ifelse(gbmpred>0.5,1,0)

predAct <- predAct %>%
  mutate(logpred = logpred) %>%
  mutate(svmpred = as.numeric(svmpred)) %>%
  mutate(nnpred = nnpred) %>%
  mutate(rfpred = rfpred)%>%
  mutate(gbmpred = gbmpred) %>%
  mutate(Ensemble = ifelse((logpred+svmpred+nnpred+rfpred+gbmpred)>2.5,1,0))
if (model=="Log"){
  predAct <- predAct%>%
    mutate(Team = ifelse(logpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(logpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(logpred==1,AwaySeed,HomeSeed))
}
if (model=="SVM"){
  predAct <- predAct%>%
    mutate(Team = ifelse(svmpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(svmpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(svmpred==1,AwaySeed,HomeSeed))
    
}
if (model=="NN"){
  predAct <- predAct%>%
    mutate(Team = ifelse(nnpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(nnpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(nnpred==1,AwaySeed,HomeSeed))
    
}
if (model=="RF"){
  predAct <- predAct%>%
    mutate(Team = ifelse(rfpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(rfpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(rfpred==1,AwaySeed,HomeSeed))
    
}
if (model=="GBM"){
  predAct <- predAct%>%
    mutate(Team = ifelse(gbmpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(gbmpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(gbmpred==1,AwaySeed,HomeSeed))
    
}
if (model=="ENS"){
  predAct <- predAct%>%
    mutate(Team = ifelse(Ensemble==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(Ensemble==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(Ensemble==1,AwaySeed,HomeSeed))
    
}
predAct <- predAct %>%
  select(Team, Seed, Conference,Match)%>%
  mutate(Match = ifelse(Match %in% c("E1","E2"),"E1",Match))%>%
  mutate(Match = ifelse(Match %in% c("W1","W2"),"W1",Match))
r2win<- predAct %>%
  select(Team, Seed, Conference,Match)
AwayTeams <- predAct %>%
  group_by(Match)%>%
  arrange(-Seed)%>%
  slice(1)%>%
  ungroup()
HomeTeams <- predAct %>%
  group_by(Match)%>%
  arrange(Seed)%>%
  slice(1)%>%
  ungroup()
AwayTeams<- left_join(AwayTeams, PowerRanking, by = "Team")%>%
  rename("Sched"="SchedRating","TeamStr"="TeamStrength","Player"="PlayerStrength")%>%
  mutate(last3games= last3games/Sched)%>%
  mutate(last5games= last5games/Sched)%>%
  mutate(last7games= last7games/Sched)%>%
  mutate(last10games= last10games/Sched)
HomeTeams<- left_join(HomeTeams, PowerRanking, by = "Team")%>%
  rename("Sched"="SchedRating","TeamStr"="TeamStrength","Player"="PlayerStrength")%>%
  mutate(last3games= last3games/Sched)%>%
  mutate(last5games= last5games/Sched)%>%
  mutate(last7games= last7games/Sched)%>%
  mutate(last10games= last10games/Sched)
for (i in c(1:3,5:13)){
  colnames(AwayTeams)[i]= str_c("Away",colnames(AwayTeams)[i])
}
for (i in c(1:3,5:13)){
  colnames(HomeTeams)[i]= str_c("Home",colnames(HomeTeams)[i])
}
predAct = left_join(AwayTeams, HomeTeams, by = "Match")%>%
  mutate(last3Diff=Awaylast3games-Homelast3games)%>%
  mutate(last5Diff=Awaylast5games-Homelast5games)%>%
  mutate(last7Diff=Awaylast7games-Homelast7games)%>%
  mutate(last10Diff=Awaylast10games-Homelast10games)%>%
  mutate(TeamDiff=AwayTeamStr-HomeTeamStr)%>%
  mutate(SchedDiff=AwaySched-HomeSched)%>%
  mutate(PlayerDiff=AwayPlayer-HomePlayer)
set.seed(1212)
logpred1 <- predict(logmod, newdata = predAct, type = "response")
logpred <- ifelse(logpred1>.5, 1,0)
svmpred <- as.vector(predict(svmmod, newdata = predAct, type = "response"))
rfpred<-as.vector(predict(rfmod, newdata = predAct))
rfpred=ifelse(rfpred>0.5,1,0)
nnpred<-(compute(nnmod,predAct))$net.result
nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, predAct, type = "response")
gbmpred=ifelse(gbmpred>0.5,1,0)

predAct <- predAct %>%
  mutate(logpred = logpred) %>%
  mutate(svmpred = as.numeric(svmpred)) %>%
  mutate(nnpred = nnpred) %>%
  mutate(rfpred = rfpred)%>%
  mutate(gbmpred = gbmpred) %>%
  mutate(Ensemble = ifelse((logpred+svmpred+nnpred+rfpred+gbmpred)>2.5,1,0))
if (model=="Log"){
  predAct <- predAct%>%
    mutate(Team = ifelse(logpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(logpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(logpred==1,AwaySeed,HomeSeed))
}
if (model=="SVM"){
  predAct <- predAct%>%
    mutate(Team = ifelse(svmpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(svmpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(svmpred==1,AwaySeed,HomeSeed))
    
}
if (model=="NN"){
  predAct <- predAct%>%
    mutate(Team = ifelse(nnpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(nnpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(nnpred==1,AwaySeed,HomeSeed))
    
}
if (model=="RF"){
  predAct <- predAct%>%
    mutate(Team = ifelse(rfpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(rfpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(rfpred==1,AwaySeed,HomeSeed))
    
}
if (model=="GBM"){
  predAct <- predAct%>%
    mutate(Team = ifelse(gbmpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(gbmpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(gbmpred==1,AwaySeed,HomeSeed))
    
}
if (model=="ENS"){
  predAct <- predAct%>%
    mutate(Team = ifelse(Ensemble==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(Ensemble==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(Ensemble==1,AwaySeed,HomeSeed))
    
}
predAct <- predAct %>%
  select(Team, Seed, Conference,Match)%>%
  mutate(Match = ifelse(Match %in% c("E1","W1"),"F",Match))
r3win<- predAct %>%
  select(Team, Seed, Conference,Match)
AwayTeams <- predAct %>%
  group_by(Match)%>%
  arrange(-Seed)%>%
  slice(1)%>%
  ungroup()
HomeTeams <- predAct %>%
  group_by(Match)%>%
  arrange(Seed)%>%
  slice(1)%>%
  ungroup()
AwayTeams<- left_join(AwayTeams, PowerRanking, by = "Team")%>%
  rename("Sched"="SchedRating","TeamStr"="TeamStrength","Player"="PlayerStrength")%>%
  mutate(last3games= last3games/Sched)%>%
  mutate(last5games= last5games/Sched)%>%
  mutate(last7games= last7games/Sched)%>%
  mutate(last10games= last10games/Sched)
HomeTeams<- left_join(HomeTeams, PowerRanking, by = "Team")%>%
  rename("Sched"="SchedRating","TeamStr"="TeamStrength","Player"="PlayerStrength")%>%
  mutate(last3games= last3games/Sched)%>%
  mutate(last5games= last5games/Sched)%>%
  mutate(last7games= last7games/Sched)%>%
  mutate(last10games= last10games/Sched)
for (i in c(1:3,5:13)){
  colnames(AwayTeams)[i]= str_c("Away",colnames(AwayTeams)[i])
}
for (i in c(1:3,5:13)){
  colnames(HomeTeams)[i]= str_c("Home",colnames(HomeTeams)[i])
}
predAct = left_join(AwayTeams, HomeTeams, by = "Match")%>%
  mutate(last3Diff=Awaylast3games-Homelast3games)%>%
  mutate(last5Diff=Awaylast5games-Homelast5games)%>%
  mutate(last7Diff=Awaylast7games-Homelast7games)%>%
  mutate(last10Diff=Awaylast10games-Homelast10games)%>%
  mutate(TeamDiff=AwayTeamStr-HomeTeamStr)%>%
  mutate(SchedDiff=AwaySched-HomeSched)%>%
  mutate(PlayerDiff=AwayPlayer-HomePlayer)
set.seed(1212)
logpred1 <- predict(logmod, newdata = predAct, type = "response")
logpred <- ifelse(logpred1>.5, 1,0)
svmpred <- as.vector(predict(svmmod, newdata = predAct, type = "response"))
rfpred<-as.vector(predict(rfmod, newdata = predAct))
rfpred=ifelse(rfpred>0.5,1,0)
nnpred<-(compute(nnmod,predAct))$net.result
nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, predAct, type = "response")
gbmpred=ifelse(gbmpred>0.5,1,0)

predAct <- predAct %>%
  mutate(logpred = logpred) %>%
  mutate(svmpred = as.numeric(svmpred)) %>%
  mutate(nnpred = nnpred) %>%
  mutate(rfpred = rfpred)%>%
  mutate(gbmpred = gbmpred) %>%
  mutate(Ensemble = ifelse((logpred+svmpred+nnpred+rfpred+gbmpred)>2.5,1,0))
if (model=="Log"){
  predAct <- predAct%>%
    mutate(Team = ifelse(logpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(logpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(logpred==1,AwaySeed,HomeSeed))
    
}
if (model=="SVM"){
  predAct <- predAct%>%
    mutate(Team = ifelse(svmpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(svmpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(svmpred==1,AwaySeed,HomeSeed))
    
}
if (model=="NN"){
  predAct <- predAct%>%
    mutate(Team = ifelse(nnpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(nnpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(nnpred==1,AwaySeed,HomeSeed))
    
}
if (model=="RF"){
  predAct <- predAct%>%
    mutate(Team = ifelse(rfpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(rfpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(rfpred==1,AwaySeed,HomeSeed))
    
}
if (model=="GBM"){
  predAct <- predAct%>%
    mutate(Team = ifelse(gbmpred==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(gbmpred==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(gbmpred==1,AwaySeed,HomeSeed))
    
}
if (model=="ENS"){
  predAct <- predAct%>%
    mutate(Team = ifelse(Ensemble==1,AwayTeam,HomeTeam))%>%
    mutate(Conference = ifelse(Ensemble==1,AwayConference,HomeConference))%>%
    mutate(Seed = ifelse(Ensemble==1,AwaySeed,HomeSeed))
    
}
r4win<- predAct %>%
  select(Team, Seed, Conference,Match)
nbaplayoff<- rbind(r1win,r2win,r3win,r4win)%>%
  mutate(Mod = modtype)
return(nbaplayoff)
}
logsim<- simNBAPlayoff("Log")
svmsim<- simNBAPlayoff("SVM")
rfsim<- simNBAPlayoff("RF")
nnsim<- simNBAPlayoff("NN")
gbmsim<- simNBAPlayoff("GBM")
enssim<- simNBAPlayoff("ENS")

