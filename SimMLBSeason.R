
### update df
df1<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/predappMLBteampred23")
df2<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthCurrent/MLB/TeamMaster.csv")%>%
  select(Team,"FSOct05"="FloStrength")
df<-left_join(df1,df2,by="Team")%>%
  mutate(UpdPred=(1*FSOct05))%>%
  mutate(lb=UpdPred-0)%>%
  mutate(ub=UpdPred+0)
df<-df%>%filter(Team!="CHC"&Team!="CIN")%>%filter(Team!="SEA"&Team!="SF")
write_csv(df, "/Users/seanfloersch/FloStrength/FloStrengthFuture/predappMLBteampred23")
###Create Simulations
simMLB<- function(numit){
  wswinners=c()
  #league <- c(1,1,2,2,1,2,1,2,1,2,2,2,2,1,1,1,2,1,2,2,1,1,1,2,1,1,2,2,2,1)
  league<-c(1,2,1,1,2,1,1,1,2,1,2,1,2,2,2)
  #division<-c(1,3,3,3,2,2,2,2,1,2,1,2,1,1,3,2,2,3,3,1,3,2,1,1,1,2,3,1,3,3)
  division<-c(1,1,2,2,3,3,1,2,2,1,3,3,1,3,1)
  df1<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/predappMLBteampred23")%>%
    mutate(teamID=Team)%>%
    select(teamID,"FloPrediction"=UpdPred, lb,ub)%>%
    mutate(FloPrediction = round(FloPrediction,3))%>%
    mutate(leagueID=factor(league))%>%
    mutate(divisionID=division)%>%
    mutate(divisionID=ifelse(leagueID==2,divisionID+3,divisionID))%>%
    mutate(divisionID=factor(divisionID))%>%
    arrange(-FloPrediction)
  for (i in c(1:numit)){
    res<-runif(15, 0.1, .9)
    if (mean(res)>.6){
      res<-runif(15, 0.15, .85)
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
      mutate(Seed= 1:3)%>%
      ungroup()
    divwinners<- divwin$teamID
    wc<- df %>%
      filter(!teamID %in% divwinners)%>%
      arrange(-FloPrediction)%>%
      group_by(leagueID)%>%
      slice(1:3)%>%
      mutate(Seed=4:6)%>%
      ungroup()
      playoffs<- rbind(divwin,wc)  
      #wc round
      winners<- playoffs[which(playoffs$Seed==1 | playoffs$Seed==2),c(1,9,5)]
      ##6@3
      a63<- playoffs%>%
        filter(leagueID==2&Seed %in%c(3,6))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a63$Prob
      res<-runif(3, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>1.5,a63$home,a63$away)
      winner<- data.frame(teamID=winner, Seed=36, leagueID=2)
      winners<- rbind(winner,winners)
      n63<- playoffs%>%
        filter(leagueID==1&Seed %in%c(3,6))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n63$Prob
      res<-runif(3, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>1.5,n63$home,n63$away)
      winner<- data.frame(teamID=winner, Seed=36, leagueID=1)
      winners<- rbind(winner,winners)
      ##5@4
      a54<- playoffs%>%
        filter(leagueID==2&Seed %in%c(4,5))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a54$Prob
      res<-runif(3, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>1.5,a54$home,a54$away)
      winner<- data.frame(teamID=winner, Seed=45, leagueID=2)
      winners<- rbind(winner,winners)
      n54<- playoffs%>%
        filter(leagueID==1&Seed %in%c(4,5))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n54$Prob
      res<-runif(3, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>1.5,n54$home,n54$away)
      winner<- data.frame(teamID=winner, Seed=45, leagueID=1)
      winners<- rbind(winner,winners)
      playoffs<- playoffs %>%
        select(teamID,FloPrediction)
      playoffs <- left_join(winners,playoffs, by = "teamID")
      #division round
      ## 1 seeds
      a1<- playoffs%>%
        filter(leagueID==2&Seed %in%c(1,45))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a1$Prob
      res<-runif(5, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>2.5,a1$home,a1$away)
      winner<- data.frame(teamID=winner, Seed=1, leagueID=2)
      winners<- winner
      n1<- playoffs%>%
        filter(leagueID==1&Seed %in%c(1,45))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n1$Prob
      res<-runif(5, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>2.5,n1$home,n1$away)
      winner<- data.frame(teamID=winner, Seed=1, leagueID=1)
      winners<- rbind(winner,winners)
      ## 2 seeds
      a1<- playoffs%>%
        filter(leagueID==2&Seed %in%c(2,36))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a1$Prob
      res<-runif(5, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>2.5,a1$home,a1$away)
      winner<- data.frame(teamID=winner, Seed=2, leagueID=2)
      winners<- rbind(winner,winners)
      n1<- playoffs%>%
        filter(leagueID==1&Seed %in%c(2,36))%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n1$Prob
      res<-runif(5, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>2.5,n1$home,n1$away)
      winner<- data.frame(teamID=winner, Seed=2, leagueID=1)
      winners<- rbind(winner,winners)
      playoffs<- playoffs %>%
        select(teamID,FloPrediction)
      playoffs <- left_join(winners,playoffs, by = "teamID")
      # league champ
      a1<- playoffs%>%
        filter(leagueID==2)%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = a1$Prob
      res<-runif(7, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>3.5,a1$home,a1$away)
      winner<- data.frame(teamID=winner, Seed=1, leagueID=2)
      winners<- winner
      n1<- playoffs%>%
        filter(leagueID==1)%>%
        arrange(Seed)%>%
        mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
        mutate(home = first(teamID))%>%
        mutate(away = last(teamID))%>%
        select(away,home,Prob)%>%
        slice(1)
      prob = n1$Prob
      res<-runif(7, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>3.5,n1$home,n1$away)
      winner<- data.frame(teamID=winner, Seed=1, leagueID=1)
      winners<- rbind(winner,winners)
      playoffs<- playoffs %>%
        select(teamID,FloPrediction)
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
      res<-runif(7, 0, 1)
      res<- sum(ifelse(res>prob,0,1))
      winner=ifelse(res>3.5,a1$home,a1$away)
      wswinners[i]<-winner
  }
  wswinners<- data.frame(wswinners)%>%
    group_by(wswinners)%>%
    mutate(Probability = length(wswinners)/numit)%>%
    slice(1)%>%
    ungroup()%>%
    arrange(-Probability)
  return(wswinners)
}
simMLBplayoffs<- function(numit){
  wswinners=c()
  for (i in c(1:numit)){
  #league <- c(1,1,2,2,1,2,1,2,1,2,2,2,2,1,1,1,2,1,2,2,1,1,1,2,1,1,2,2,2,1)
  league<-c(1,2,2,1,1,1,2,1,2,2,2)
  #division<-c(1,3,3,3,2,2,2,2,1,2,1,2,1,1,3,2,2,3,3,1,3,2,1,1,1,2,3,1,3,3)
  division<-c(1,2,2,1,1,1,2,1,2,2,2)
  df1<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/predappMLBteampred23")%>%
    mutate(teamID=Team)%>%
    select(teamID,"FloPrediction"=UpdPred, lb,ub)%>%
    mutate(FloPrediction = round(FloPrediction,3))%>%
    mutate(leagueID=factor(league))%>%
    mutate(divisionID=division)%>%
    mutate(divisionID=ifelse(leagueID==2,divisionID+3,divisionID))%>%
    mutate(divisionID=factor(divisionID))%>%
    arrange(teamID)%>%
    mutate(Seed=c(c(1,1,2,2,9,9,36,45,9,45,9)))%>%
    filter(Seed!=9)
  ari<-data.frame("teamID"="ARI","FloPrediction"=.537,"lb"=.537,"ub"=.537,"leagueID"=1,"divisionID"=5,"Seed"=36)
  playoffs<-rbind(df1,ari)  %>%
    mutate(per=0)%>%
    mutate(se=0)%>%
    select(teamID,FloPrediction,lb,ub,leagueID,divisionID,per,se,Seed)
  #division round
    ## 1 seeds
    a1<- playoffs%>%
      filter(leagueID==2&Seed %in%c(1,45))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(5, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>2.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner, Seed=1, leagueID=2)
    winners<- winner
    n1<- playoffs%>%
      filter(leagueID==1&Seed %in%c(1,45))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(5, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>2.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner, Seed=1, leagueID=1)
    winners<- rbind(winner,winners)
    ## 2 seeds
    a1<- playoffs%>%
      filter(leagueID==2&Seed %in%c(2,36))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(5, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>2.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner, Seed=2, leagueID=2)
    winners<- rbind(winner,winners)
    n1<- playoffs%>%
      filter(leagueID==1&Seed %in%c(2,36))%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(5, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>2.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner, Seed=2, leagueID=1)
    winners<- rbind(winner,winners)
    playoffs<- playoffs %>%
      select(teamID,FloPrediction)
    playoffs <- left_join(winners,playoffs, by = "teamID")
    # league champ
    a1<- playoffs%>%
      filter(leagueID==2)%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = a1$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,a1$home,a1$away)
    winner<- data.frame(teamID=winner, Seed=1, leagueID=2)
    winners<- winner
    n1<- playoffs%>%
      filter(leagueID==1)%>%
      arrange(Seed)%>%
      mutate(Prob = first(FloPrediction)/(first(FloPrediction)+last(FloPrediction)))%>%
      mutate(home = first(teamID))%>%
      mutate(away = last(teamID))%>%
      select(away,home,Prob)%>%
      slice(1)
    prob = n1$Prob
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,n1$home,n1$away)
    winner<- data.frame(teamID=winner, Seed=1, leagueID=1)
    winners<- rbind(winner,winners)
    playoffs<- playoffs %>%
      select(teamID,FloPrediction)
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
    res<-runif(7, 0, 1)
    res<- sum(ifelse(res>prob,0,1))
    winner=ifelse(res>3.5,a1$home,a1$away)
    wswinners[i]<-winner
  }
  wswinners<- data.frame(wswinners)%>%
    group_by(wswinners)%>%
    mutate(Probability = length(wswinners)/numit)%>%
    slice(1)%>%
    ungroup()%>%
    arrange(-Probability)
  return(wswinners)
}

wswinners<-simMLBplayoffs(5000)
wswinners<- wswinners%>%
  mutate(Date= 100523)
oldwswinners<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/MLB/WSOdds.csv")%>%
  select("wswinners"="Team",Probability,Date)
wswinners<-rbind(oldwswinners,wswinners)
wswinners<-wswinners%>%
  mutate(wswinners=ifelse(wswinners=="SDP","SD",wswinners))%>%
  mutate(wswinners=ifelse(wswinners=="TBR","TB",wswinners))%>%
  mutate(wswinners=ifelse(wswinners=="KCR","KC",wswinners))%>%
  mutate(wswinners=ifelse(wswinners=="SFG","SF",wswinners))
df2<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthCurrent/MLB/TeamMaster.csv")%>%
  select("wswinners"=Team,Div,Lg)
test<-left_join(wswinners,df2,by ="wswinners")%>%
  rename("Team"="wswinners")
write_csv(test, "/Users/seanfloersch/FloStrength/FloStrengthFuture/MLB/WSOdds.csv")
