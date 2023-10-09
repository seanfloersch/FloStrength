df<- read.csv("/Users/seanfloersch/FloStrength/NFLFloStrength/NFL2023/TeamPred23.csv")%>%
  select(-Division,-League,-GP,-GRem,-Wins,-Losses)
df2<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthCurrent/NFL/TeamMaster.csv")%>%
  mutate(FSOct03=round(.2*FloPlayer+.5*FloTeam+.3*FloSched,3))%>%
  select(Team,GP,Wins,Losses,Division,League,FSOct03)%>%
  mutate(GRem=17-GP)
df<-left_join(df,df2,by ="Team")%>%
  mutate(UpdPred=((GP/17)*FSOct03+((GRem/17)*PredWpct)))%>%
  mutate(UpdPred=(GRem*(FSOct03)+(Wins))/17)
write_csv(df,"/Users/seanfloersch/FloStrength/NFLFloStrength/NFL2023/TeamPred23.csv")

df<-df%>%
  select(Team,Division,League,PredWpct,predWins,FSOct03,UpdPred)%>%
  mutate(lb=UpdPred-.14)%>%
  mutate(ub=UpdPred+.14)

write_csv(df,"/Users/seanfloersch/FloStrength/FloStrengthFuture/NFL/NFLTeamPred2023.csv")

###Create Simulations
simSBWinners<- function(numit){
  sbwinners=c()
  league<-c(1,1,2,2,1,1,2,2,1,2,1,1,2,2,2,2,2,1,2,2,1,2,1,1,2,1,2,1,1,1,2,1)
  division<-c(4,2,1,3,2,1,1,1,3,4,1,1,2,2,2,4,4,4,4,3,1,3,2,3,3,3,1,4,4,2,2,3)
  df1<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NFL/NFLTeamPred2023.csv")%>%
    mutate(teamID=Team)%>%
    select(teamID,"FloPrediction"=UpdPred, lb,ub)%>%
    mutate(FloPrediction = round(FloPrediction,3))%>%
    mutate(leagueID=factor(league))%>%
    mutate(divisionID=division)%>%
    mutate(divisionID=ifelse(leagueID==2,divisionID+4,divisionID))%>%
    mutate(divisionID=factor(divisionID))%>%
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
    divwin <- df %>%
      group_by(divisionID)%>%
      slice(1)%>%
      ungroup()%>%
      group_by(leagueID)%>%
      arrange(-FloPrediction)%>%
      mutate(Seed= 1:4)%>%
      ungroup()
    divwinners<- divwin$teamID
    wc<- df %>%
      filter(!teamID %in% divwinners)%>%
      arrange(-FloPrediction)%>%
      group_by(leagueID)%>%
      slice(1:3)%>%
      mutate(Seed=5:7)%>%
      ungroup()
    playoffs<- rbind(divwin,wc)  
    #wc round
    winners<- playoffs[which(playoffs$Seed==1),c(1)]
    ##6@3
    a63<- playoffs%>%
      filter(leagueID==2&Seed %in%c(3,6))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a63$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,a63$home,a63$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    n63<- playoffs%>%
      filter(leagueID==1&Seed %in%c(3,6))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n63$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,n63$home,n63$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    ##7@2
    a72<- playoffs%>%
      filter(leagueID==2&Seed %in%c(2,7))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a72$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,a72$home,a72$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    n72<- playoffs%>%
      filter(leagueID==1&Seed %in%c(2,7))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n72$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,n72$home,n72$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    
    
    ##5v4
    a54<- playoffs%>%
      filter(leagueID==2&Seed %in%c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a54$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,a54$home,a54$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    n54<- playoffs%>%
      filter(leagueID==1&Seed %in%c(4,5))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n54$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,n54$home,n54$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    
    playoffs<- playoffs %>%
      select(teamID,FloPrediction,Seed,leagueID)
    playoffs <- left_join(winners,playoffs, by = "teamID")%>%
      arrange(FloPrediction)%>%
      group_by(leagueID)%>%
      mutate(newSeed= c(4,3,2,1))%>%
      ungroup()
    #division round
    ## 1 seeds
    a1<- playoffs%>%
      filter(leagueID==2&newSeed %in%c(1,4))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner)
    winners<- winner
    n1<- playoffs%>%
      filter(leagueID==1&newSeed %in%c(1,4))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    ## 2 seeds
    a1<- playoffs%>%
      filter(leagueID==2&newSeed %in%c(2,3))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    n1<- playoffs%>%
      filter(leagueID==1&newSeed %in%c(2,3))%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    playoffs<- playoffs %>%
      select(teamID,FloPrediction,Seed,leagueID)
    playoffs <- left_join(winners,playoffs, by = "teamID")
    # league champ
    a1<- playoffs%>%
      filter(leagueID==2)%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner)
    winners<- winner
    n1<- playoffs%>%
      filter(leagueID==1)%>%
      arrange(Seed)%>%
      mutate(Prob = (first(FloPrediction)+.02)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner)
    winners<- rbind(winner,winners)
    playoffs<- playoffs %>%
      select(teamID,FloPrediction,Seed,leagueID)
    playoffs <- left_join(winners,playoffs, by = "teamID")
    ### WORLD SERIES
    a1<- playoffs%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(1, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>.5,a1$home,a1$away)
    sbwinners[i]<-winner
  }
  sbwinners<- data.frame(sbwinners)%>%
    group_by(sbwinners)%>%
    mutate(Probability = length(sbwinners)/numit)%>%
    slice(1)%>%
    ungroup()%>%
    arrange(-Probability)
  return(sbwinners)
}
sbwinners<-simSBWinners(5000)
sbwinners<- sbwinners%>%
  mutate(Date= as.Date("2023-10-03"))
oldsbwinners<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NFL/NFLMonteCarlo23.csv")%>%
  mutate(Date=as.Date(Date))%>%
  select(sbwinners,Date,Probability)
sbwinners<-rbind(oldsbwinners,sbwinners)%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Team=sbwinners)%>%
  mutate(Team=ifelse(Team=="GB","GNB",Team))%>%
  mutate(Team=ifelse(Team=="KC","KAN",Team))%>%
  mutate(Team=ifelse(Team=="NE","NWE",Team))%>%
  mutate(Team=ifelse(Team=="LV","LVR",Team))%>%
  mutate(Team=ifelse(Team=="NO","NOR",Team))%>%
  mutate(Team=ifelse(Team=="SF","SFO",Team))%>%
  mutate(Team=ifelse(Team=="TB","TAM",Team))%>%
  mutate(Team=ifelse(Team=="WSH","WAS",Team))
df2<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthCurrent/NFL/TeamMaster.csv")%>%
  mutate(FSOct03=round(.2*FloPlayer+.5*FloTeam+.3*FloSched,3))%>%
  select(Team,Division,League)
sbwinners<-left_join(sbwinners,df2,by ="Team")
write_csv(sbwinners, "/Users/seanfloersch/FloStrength/FloStrengthFuture/NFL/NFLMonteCarlo23.csv")
