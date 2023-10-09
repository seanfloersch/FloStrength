
### update df
df<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/TeamPred23.csv")%>%
  mutate(UpdPred=PredFS)%>%
  select(-LYFS,-PlayPred)%>%
  mutate(ub=UpdPred+.2)%>%
  mutate(lb =UpdPred-.2)
write_csv(df, "/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/TeamPred23.csv")
###Create Simulations
simNHL<- function(numit){
  stanleycup=c()
  df1<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/TeamPred23.csv")%>%
    mutate(teamID=Team)%>%
    mutate(leagueID=factor(Conf))%>%
    select(teamID,"FloPrediction"=UpdPred, lb,ub,leagueID,"divisionID"=Div)%>%
    mutate(FloPrediction = round(FloPrediction,3))%>%
    arrange(-FloPrediction)
  for (i in c(1:numit)){
    res<-runif(32, 0.1, .9)
    if (mean(res)>.6){
      res<-runif(32, 0.15, .85)
    }
    res = res- (mean(res)-.5)
    df<- df1 %>%
      mutate(per = res-.5)%>%
      mutate(se = ub-lb)%>%
      mutate(FloPrediction = FloPrediction + se*per)%>%
      arrange(-FloPrediction)
    middf <- df %>%
      group_by(divisionID)%>%
      arrange(-FloPrediction)%>%
      slice(2:3)%>%
      ungroup%>%
      group_by(divisionID)%>%
      mutate(Seed= c(4:5))%>%
      ungroup()
    topdf <- df %>%
      group_by(divisionID)%>%
      arrange(-FloPrediction)%>%
      slice(1)%>%
      ungroup%>%
      group_by(leagueID)%>%
      mutate(Seed= c(1:2))%>%
      ungroup()
    seeddf<-rbind(topdf,middf)
    wcdf<-df%>%
      filter(!teamID %in% seeddf$teamID)%>%
      group_by(leagueID)%>%
      arrange(-FloPrediction)%>%
      slice(1:2)%>%
      mutate(Seed=7:8)%>%
      ungroup()
    seeddf<-rbind(seeddf,wcdf)
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
    matchseed<-seeddf$divisionID[which(seeddf$Seed==1&seeddf$leagueID=="West")]
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,matchseed,NA))
    
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
    matchseed<-seeddf$divisionID[which(seeddf$Seed==2&seeddf$leagueID=="West")]
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,matchseed,MatchSeed))
    
    playoff<-seeddf%>%
      filter(divisionID=="Central"& Seed %in% c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    matchseed="Central"
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,matchseed,MatchSeed))
    
    playoff<-seeddf%>%
      filter(divisionID=="Metropolitan"& Seed %in% c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    matchseed="Metropolitan"
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,matchseed,MatchSeed))
    #East
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(1,8))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    matchseed<-seeddf$divisionID[which(seeddf$Seed==1&seeddf$leagueID=="East")]
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,matchseed,MatchSeed))
    playoff<-seeddf%>%
      filter(leagueID=="East"& Seed %in% c(2,7))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    matchseed<-seeddf$divisionID[which(seeddf$Seed==2&seeddf$leagueID=="East")]
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,matchseed,MatchSeed))
    playoff<-seeddf%>%
      filter(divisionID=="Metro"& Seed %in% c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    matchseed="Metro"
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,matchseed,MatchSeed))
    playoff<-seeddf%>%
      filter(divisionID=="Atlantic"& Seed %in% c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    matchseed="Atlantic"
    prob = playoff$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,playoff$home,playoff$away)
    loser=ifelse(res<3.5,playoff$home,playoff$away)
    seeddf<-seeddf%>%
      filter(teamID!=loser)%>%
      mutate(MatchSeed= ifelse(teamID==winner,matchseed,MatchSeed))
    #####Second Round ######
    #West
    playoff<-seeddf%>%
      filter(MatchSeed=="Central")%>%
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
    playoff<-seeddf%>%
      filter(MatchSeed=="Metropolitan")%>%
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
      mutate(MatchSeed= ifelse(teamID==winner,2,MatchSeed))
    #East
    playoff<-seeddf%>%
      filter(MatchSeed=="Metro")%>%
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
    playoff<-seeddf%>%
      filter(MatchSeed=="Atlantic")%>%
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
      mutate(MatchSeed= ifelse(teamID==winner,2,MatchSeed))
    ##### Conference Finals#####
    playoff<-seeddf%>%
      filter(leagueID=="East")%>%
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
    playoff<-seeddf%>%
      filter(leagueID=="West")%>%
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
    stanleycup[i]<-seeddf$teamID
  }
  stanleycup<- data.frame(stanleycup)%>%
    group_by(stanleycup)%>%
    mutate(Probability = length(stanleycup)/numit)%>%
    slice(1)%>%
    ungroup()%>%
    arrange(-Probability)%>%
    select("Team"=stanleycup,Probability)
  return(stanleycup)
}
stanleycup<-simNHL(5000)
general<-read.csv("~/FloStrength/NHLFloStrength/TeamGeneral.csv")
stanleycup<-full_join(stanleycup,general,by ="Team")%>%
  mutate(Probability=ifelse(is.na(Probability)==TRUE,0,Probability))%>%
  mutate(Date=as.Date("2023-10-08"))
oldstanleycup<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/StanleyCupOdds.csv")
stanleycup<-rbind(oldstanleycup,stanleycup)
write_csv(stanleycup, "/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/StanleyCupOdds.csv")
