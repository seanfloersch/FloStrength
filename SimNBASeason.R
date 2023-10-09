
### update df
df<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NBA/TeamPred23.csv")%>%
  mutate(UpdPred=FSPredict)%>%
  mutate(ub=UpdPred+.2)%>%
  mutate(lb =UpdPred-.2)
write_csv(df, "/Users/seanfloersch/FloStrength/FloStrengthFuture/NBA/TeamPred23.csv")
###Create Simulations
simNBA<- function(numit){
  nbachamps=c()
  df1<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NBA/TeamPred23.csv")%>%
    mutate(teamID=Team)%>%
    mutate(leagueID=factor(Conf))%>%
    select(teamID,"FloPrediction"=UpdPred, lb,ub,leagueID)%>%
    mutate(FloPrediction = round(FloPrediction,3))%>%
    arrange(-FloPrediction)
  for (i in c(1:numit)){
    res<-runif(30, 0.1, .9)
    if (mean(res)>.6){
      res<-runif(30, 0.15, .85)
    }
    res = res- (mean(res)-.5)
    df<- df1 %>%
      mutate(per = res-.5)%>%
      mutate(se = ub-lb)%>%
      mutate(FloPrediction = FloPrediction + se*per)%>%
      arrange(-FloPrediction)
    seeddf <- df %>%
      group_by(leagueID)%>%
      arrange(-FloPrediction)%>%
      slice(1:10)%>%
      mutate(Seed= 1:10)%>%
      ungroup()
    ##### play-in #####
    #west
    playoff<-seeddf%>%
      filter(leagueID=="West"& Seed %in% c(9,10))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,playoff$home,playoff$away)
    loser=ifelse(res<.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      mutate(Seed=ifelse(teamID==winner,9,Seed))%>%
      filter(teamID!=loser)
    playoff<-seeddf%>%
      filter(leagueID=="West"& Seed %in% c(7,8))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,playoff$home,playoff$away)
    loser=ifelse(res<.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      mutate(Seed=ifelse(teamID==winner,7,Seed))%>%
      mutate(Seed=ifelse(teamID==loser,8,Seed))
    playoff<-seeddf%>%
      filter(leagueID=="West"& Seed %in% c(8,9))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,playoff$home,playoff$away)
    loser=ifelse(res<.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      mutate(Seed=ifelse(teamID==winner,8,Seed))%>%
      filter(teamID!=loser)
    #east
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(9,10))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,playoff$home,playoff$away)
    loser=ifelse(res<.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      mutate(Seed=ifelse(teamID==winner,9,Seed))%>%
      filter(teamID!=loser)
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(7,8))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,playoff$home,playoff$away)
    loser=ifelse(res<.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      mutate(Seed=ifelse(teamID==winner,7,Seed))%>%
      mutate(Seed=ifelse(teamID==loser,8,Seed))
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(8,9))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,playoff$home,playoff$away)
    loser=ifelse(res<.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      mutate(Seed=ifelse(teamID==winner,8,Seed))%>%
      filter(teamID!=loser)
    #####Round 1######
    #West
    playoff<-seeddf%>%
      filter(leagueID=="West"& Seed %in% c(1,8))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,1,NA))
    playoff<-seeddf%>%
      filter(leagueID=="West"& Seed %in% c(2,7))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,2,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="West"& Seed %in% c(3,6))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,3,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="West"& Seed %in% c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,4,MatchSeed))
    #East
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(1,8))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,1,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(2,7))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,2,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(3,6))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,3,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,4,MatchSeed))
    #####Second Round ######
    #West
    playoff<-seeddf%>%
      filter(leagueID=="West"& MatchSeed %in% c(1,4))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,1,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="West"& MatchSeed %in% c(2,3))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,2,MatchSeed))
    #East
    playoff<-seeddf%>%
      filter(leagueID=="East"& MatchSeed %in% c(1,4))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,1,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="East"& MatchSeed %in% c(2,3))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,2,MatchSeed))
    ##### Conference Finals#####
    playoff<-seeddf%>%
      filter(leagueID=="East"& MatchSeed %in% c(1,2))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,1,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="West"& MatchSeed %in% c(1,2))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,1,MatchSeed))
    ##### NBA Finals#####
    playoff<-seeddf%>%
      arrange(-FloPrediction)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,1,MatchSeed))
    nbachamps[i]<-seeddf$teamID
  }
  nbachamps<- data.frame(nbachamps)%>%
    group_by(nbachamps)%>%
    mutate(Probability = length(nbachamps)/numit)%>%
    slice(1)%>%
    ungroup()%>%
    arrange(-Probability)%>%
    select("Team"=nbachamps,Probability)
  return(nbachamps)
}
nbachamps<-simNBA(5000)
nbachamps<- nbachamps
general<-read.csv("~/FloStrength/NBAFloStrength/NBATeamGeneral.csv")
nbachamps<-full_join(nbachamps,general,by ="Team")%>%
  mutate(Probability=ifelse(is.na(Probability)==TRUE,0,Probability))%>%
  mutate(Date=as.Date("2023-10-07"))
oldnbachamps<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NBA/NBAChampOdds.csv")
nbachamps<-rbind(oldnbachamps,nbachamps)
write_csv(nbachamps, "/Users/seanfloersch/FloStrength/FloStrengthFuture/NBA/NBAChampOdds.csv")
