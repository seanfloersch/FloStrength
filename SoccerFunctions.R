getSoccerRanks<- function(x){
  ###############################
  #Standings
  ###############################
  h <- read_html("https://fbref.com/en/comps/Big5/Big-5-European-Leagues-Stats") 
  basictable <- html_nodes(h, "#big5_table td") %>% html_text
  df<- data.frame(matrix(nrow=96,ncol=20))
  for (i in c(1:20)){
    for (j in c(1:96)){
      df[j,i]= basictable[i + (j -1)* 20]
    }
  }
  colnames(df)<- (html_nodes(h, "#big5_table thead .center") %>% html_text)[2:21]
  standings<-df%>%select(Squad,Country,W,D,L,GF,GA,Pts,Attendance)
  ###############################
  #Standard
  ###############################
  h <- read_html("https://fbref.com/en/comps/Big5/stats/squads/Big-5-European-Leagues-Stats") 
  standstats <- html_nodes(h, "#stats_teams_standard_for td , #stats_teams_standard_for .right") %>% html_text
  df<- data.frame(matrix(nrow=96,ncol=34))
  for (i in c(1:34)){
    for (j in c(1:96)){
      df[j,i]= standstats[i + (j -1)* 34]
    }
  }
  colnames(df)<- (html_nodes(h, "#stats_teams_standard_for .poptip") %>% html_text)
  standstats<-df%>% select(Squad, Poss,PK,PrgC,PrgP)
  oppostats <- html_nodes(h, ".modified , #stats_teams_standard_against .right , #stats_teams_standard_against .left") %>% html_text
  df<- data.frame(matrix(nrow=96,ncol=34))
  for (i in c(1:34)){
    for (j in c(1:96)){
      df[j,i]= oppostats[i + (j -1)* 34]
    }
  }
  colnames(df)<- (html_nodes(h, "#stats_teams_standard_for .poptip") %>% html_text)
  oppostats<-df%>% select(Squad, Poss,PK,PrgC,PrgP)
  ###############################
  #Shooting
  ###############################
  h <- read_html("https://fbref.com/en/comps/Big5/shooting/squads/Big-5-European-Leagues-Stats") 
  shooting <- html_nodes(h, "#stats_teams_shooting_for td") %>% html_text
  df<- data.frame(matrix(nrow=96,ncol=21))
  for (i in c(1:21)){
    for (j in c(1:96)){
      df[j,i]= shooting[i + (j -1)* 21]
    }
  }
  colnames(df)<- (html_nodes(h, "#stats_teams_shooting_for .poptip") %>% html_text)[2:22]
  shooting<-df%>% select(Squad, Sh,SoT,Dist,FK)
  stats <- html_nodes(h, "#stats_teams_shooting_against td") %>% html_text
  df<- data.frame(matrix(nrow=96,ncol=21))
  for (i in c(1:21)){
    for (j in c(1:96)){
      df[j,i]= stats[i + (j -1)* 21]
    }
  }
  colnames(df)<- (html_nodes(h, "#stats_teams_shooting_for .poptip") %>% html_text)[2:22]
  oppshooting<-df%>% select(Squad, Sh,SoT,Dist,FK)
  
  ###############################
  #Passing
  ###############################
  h <- read_html("https://fbref.com/en/comps/Big5/passing/squads/Big-5-European-Leagues-Stats") 
  stats <- html_nodes(h, "#stats_teams_passing_for td") %>% html_text
  df<- data.frame(matrix(nrow=96,ncol=27))
  for (i in c(1:27)){
    for (j in c(1:96)){
      df[j,i]= stats[i + (j -1)* 27]
    }
  }
  colnames(df)<- (html_nodes(h, "#stats_teams_passing_for .poptip") %>% html_text)[2:28]
  passing<-df[,c(1,5,6,8,9,10,11,13,14,16,17,19,23:26)]
  stats <- html_nodes(h, "#stats_teams_passing_against td") %>% html_text
  df<- data.frame(matrix(nrow=96,ncol=27))
  for (i in c(1:27)){
    for (j in c(1:96)){
      df[j,i]= stats[i + (j -1)* 27]
    }
  }
  colnames(df)<- (html_nodes(h, "#stats_teams_passing_against .poptip") %>% html_text)[2:28]
  opppassing<-df[,c(1,5,6,8,9,10,11,13,14,16,17,19,23:26)]
  
  ###############################
  #Combine and FloStats
  ###############################
  teamdf<- left_join(standings,standstats, by = "Squad")
  teamdf<- left_join(teamdf,shooting, by = "Squad")
  teamdf<- left_join(teamdf,passing, by = "Squad")%>%
    mutate(Attendance=str_remove(Attendance,","))
  for (i in c(3:32)){
    teamdf[,i]=as.numeric(teamdf[,i])
  }
  flostats<-teamdf%>%
    mutate(MP=W+D+L)%>%
    mutate(Shooting=log((SoT*(SoT/Sh))/(MP*Dist)))%>%
    mutate(Dominance =log(((`1/3`+PrgC+PrgP)*Poss)/MP))%>%
    mutate(Passing=log((Cmp.1*(Cmp.1/Att.1)+2*Cmp.2*(Cmp.2/Att.2)+3*Cmp.3*(Cmp.3/Att.3)+50*(PPA+CrsPA))/MP))%>%
    mutate(PtsPerc=Pts/(MP*3))%>%
    mutate(Shooting=(Shooting-mean(Shooting))/sd(Shooting))%>%
    mutate(Passing=(Passing-mean(Passing))/sd(Passing))%>%
    mutate(Dominance=(Dominance-mean(Dominance))/sd(Dominance))%>%
    mutate(GF=GF/MP)%>%
    mutate(Offense=1.51+.40*Shooting+Passing*.15)%>%
    select(Squad,Country,MP,GF,"Sh"="Sh","SoT"="SoT","PassCmp"="Cmp","PassAtt"="Att","Ast"="Ast","Poss"="Poss","KeyPass"="KP",Shooting,Dominance,Passing,Offense)
  lin<-lm(GF~Shooting+Passing,data = flostats)
  summary(lin)
  
  
  
  oppostats<- oppostats%>%
    mutate(Squad= str_remove(Squad,"vs "))
  oppshooting<- oppshooting%>%
    mutate(Squad= str_remove(Squad,"vs "))
  opppassing<- opppassing%>%
    mutate(Squad= str_remove(Squad,"vs "))
  
  teamdf<- left_join(standings,oppostats, by = "Squad")
  teamdf<- left_join(teamdf,oppshooting, by = "Squad")
  teamdf<- left_join(teamdf,opppassing, by = "Squad")%>%
    mutate(Attendance=str_remove(Attendance,","))
  for (i in c(3:32)){
    teamdf[,i]=as.numeric(teamdf[,i])
  }
  flodef<- teamdf%>%
    mutate(MP=W+D+L)%>%
    mutate(ShotDef=log((SoT*(SoT/Sh))/(MP*Dist)))%>%
    mutate(OppDominance =log(((`1/3`+PrgC+PrgP)*Poss)/MP))%>%
    mutate(PassDef=log((Cmp.1*(Cmp.1/Att.1)+2*Cmp.2*(Cmp.2/Att.2)+3*Cmp.3*(Cmp.3/Att.3)+50*(PPA+CrsPA))/MP))%>%
    mutate(PtsPerc=Pts/(MP*3))%>%
    mutate(ShotDef=(ShotDef-mean(ShotDef))/sd(ShotDef))%>%
    mutate(PassDef=(PassDef-mean(PassDef))/sd(PassDef))%>%
    mutate(OppDominance=-1*((OppDominance-mean(OppDominance))/sd(OppDominance)))%>%
    mutate(GA=GA/MP)%>%
    mutate(Defense=1.5+.34*ShotDef+.15*PassDef )%>%
    select(Squad,W,D,L,GA,Pts, "OppSh"="Sh","OppSoT"="SoT","OppPassCmp"="Cmp","OppPassAtt"="Att","OppAst"="Ast","OppPoss"="Poss","OppKeyPass"="KP",ShotDef,OppDominance,PassDef,PtsPerc,Defense)
  #lin<-lm(GA~ShotDef+PassDef,data = flodef)
  #summary(lin)
  floteam<-left_join(flostats,flodef, by = "Squad")%>%
    mutate(Dominance = (Dominance+OppDominance)/2)
  lin<- lm(PtsPerc~Offense+Defense, data = floteam)
  floteam<-floteam%>%
    mutate(TeamScore=predict(lin))%>%
    mutate(yearID = 2023)%>%
    mutate(League = ifelse(Country=="es ESP","La Liga",NA))%>%
    mutate(League = ifelse(Country=="de GER","Bundesliga",League))%>%
    mutate(League = ifelse(Country=="eng ENG","EPL",League))%>%
    mutate(League = ifelse(Country=="fr FRA","Ligue 1",League))%>%
    mutate(League = ifelse(Country=="it ITA","Serie A",League))
  #####################
  #League Rank
  #####################
  getCL<-function(year){
    yearcode <- str_c(year,"-", year+1)
    url <- str_c("https://fbref.com/en/comps/8/",yearcode,"/schedule/",yearcode,"-Champions-League-Scores-and-Fixtures")
    h <- read_html(url) 
    HTeam <- (html_nodes(h, ".right a") %>% html_text)[1:125]
    ATeam <- (html_nodes(h, ".right+ .left a") %>% html_text)[1:125]
    Score <- (html_nodes(h, ".center a") %>% html_text)[1:125]
    HScore<-str_extract(Score,"\\d{1,2}")%>% as.numeric()
    AScore<-str_extract(Score,"\\d{1,2}$")%>% as.numeric()
    sched<-data.frame(ATeam,HTeam,AScore,HScore)%>%
      mutate(yearID = year)
    return(sched)
  }
  schedCL<-getCL(2023)
  
  getEL<-function(year){
    yearcode <- str_c(year,"-", year+1)
    url <- str_c("https://fbref.com/en/comps/19/",yearcode,"/schedule/",yearcode,"-Europa-League-Scores-and-Fixtures")
    h <- read_html(url) 
    HTeam <- (html_nodes(h, ".right a") %>% html_text)[1:141]
    ATeam <- (html_nodes(h, ".center+ .left a") %>% html_text)[1:141]
    Score <- (html_nodes(h, ".center a") %>% html_text)[1:141]
    HScore<-str_extract(Score,"\\d{1,2}")%>% as.numeric()
    AScore<-str_extract(Score,"\\d{1,2}$")%>% as.numeric()
    numgames<-
      if(year>=2017){
        ATeam <- (html_nodes(h, ".right+ .left a") %>% html_text)[1:141]
      }
    sched<-data.frame(ATeam,HTeam,AScore,HScore)%>%
      mutate(yearID = year)
    return(sched)
  }
  schedEL<-getEL(2023)
  
  schedCL<- rbind(schedCL,schedEL)
  ateam<-floteam%>% select("ATeam"="Squad","ALeague"="Country","AwayFS"="TeamScore",yearID)
  hteam<-floteam%>% select("HTeam"="Squad","HLeague"="Country","HomeFS"="TeamScore",yearID)
  schedCL<-left_join(schedCL,ateam, by =c("ATeam","yearID"))
  schedCL<-left_join(schedCL,hteam, by =c("HTeam","yearID"))%>%
    mutate(PD= AScore-HScore)%>%
    mutate(AFS=ifelse(PD>=0,PD*HomeFS,PD*(1-HomeFS)))%>%
    mutate(HFS=ifelse(PD>=0,-1*PD*(1-AwayFS),-1*PD*AwayFS))%>%
    na.omit()
  dfschedaway= schedCL%>%
    group_by(yearID,ALeague)%>%
    mutate(AFS= sum(AFS))%>%
    mutate(AG=length(AFS))%>%
    select("League"="ALeague",yearID,AFS,AG)%>%
    slice(1)%>%
    ungroup()
  dfschedhome= schedCL%>%
    group_by(yearID,HLeague)%>%
    mutate(HFS= sum(HFS))%>%
    mutate(HG=length(HFS))%>%
    select("League"="HLeague",yearID,HFS,HG)%>%
    slice(1)%>%
    ungroup()
  dfsched<-left_join(dfschedaway,dfschedhome,by=c("yearID","League"))%>%
    mutate(SchedFS= (AFS+HFS)/(AG+HG))%>%
    select(League,yearID,SchedFS)
  leagueRank <- dfsched %>%
    mutate(LeagueRank=(9+SchedFS)/10)%>%
    group_by(yearID)%>%
    mutate(LeagueRank = LeagueRank/max(LeagueRank))%>%
    ungroup()%>%
    select(League,yearID, LeagueRank)%>%
    mutate(League = ifelse(League=="es ESP","La Liga",League))%>%
    mutate(League = ifelse(League=="de GER","Bundesliga",League))%>%
    mutate(League = ifelse(League=="eng ENG","EPL",League))%>%
    mutate(League = ifelse(League=="fr FRA","Ligue 1",League))%>%
    mutate(League = ifelse(League=="it ITA","Serie A",League))
  floteam<-left_join(floteam,leagueRank, by = c("League","yearID"))%>%
    na.omit()%>%
    mutate(FloStrength=LeagueRank*TeamScore)
  getEPLSched<-function(year){
    yearcode <- str_c(year,"-", year+1)
    url <- str_c("https://fbref.com/en/comps/9/",yearcode,"/schedule/",yearcode,"-Premier-League-Scores-and-Fixtures")
    h <- read_html(url) 
    Date <- html_nodes(h, ".left:nth-child(3) a") %>% html_text
    HTeam <- html_nodes(h, ".right a") %>% html_text
    ATeam <- html_nodes(h, ".center+ .left a") %>% html_text
    Score <- html_nodes(h, ".center a") %>% html_text
    HScore<-str_extract(Score,"\\d{1,2}")%>% as.numeric()
    AScore<-str_extract(Score,"\\d{1,2}$")%>% as.numeric()
    if(year>=2017){
      ATeam <- html_nodes(h, ".right+ .left a") %>% html_text
    }
    ATeam=ATeam[1:length(AScore)]
    HTeam=HTeam[1:length(AScore)]
    Date=Date[1:length(AScore)]
    
    sched<-data.frame(ATeam,HTeam,AScore,HScore,Date)%>%
      mutate(yearID = year)
    return(sched)
  }
  schedEPL<- getEPLSched(2023)
  
  getLaLiga<-function(year){
    yearcode <- str_c(year,"-", year+1)
    url <- str_c("https://fbref.com/en/comps/12/",yearcode,"/schedule/",yearcode,"-La-Liga-Scores-and-Fixtures")
    h <- read_html(url) 
    Date <- html_nodes(h, ".left:nth-child(3) a") %>% html_text
    HTeam <- html_nodes(h, ".right a") %>% html_text
    ATeam <- html_nodes(h, ".center+ .left a") %>% html_text
    Score <- html_nodes(h, ".center a") %>% html_text
    HScore<-str_extract(Score,"\\d{1,2}")%>% as.numeric()
    AScore<-str_extract(Score,"\\d{1,2}$")%>% as.numeric()
    if(year>=2017){
      ATeam <- html_nodes(h, ".right+ .left a") %>% html_text
    }
    ATeam=ATeam[1:length(AScore)]
    HTeam=HTeam[1:length(AScore)]
    Date=Date[1:length(AScore)]
    
    sched<-data.frame(ATeam,HTeam,AScore,HScore,Date)%>%
      mutate(yearID = year)
    return(sched)
  }
  schedSpa=getLaLiga(2023)
  
  getItaly<-function(year){
    yearcode <- str_c(year,"-", year+1)
    url <- str_c("https://fbref.com/en/comps/11/",yearcode,"/schedule/",yearcode,"-Serie-A-Scores-and-Fixtures")
    h <- read_html(url) 
    Date <- html_nodes(h, ".left:nth-child(3) a") %>% html_text
    HTeam <- html_nodes(h, ".right a") %>% html_text
    ATeam <- html_nodes(h, ".center+ .left a") %>% html_text
    Score <- html_nodes(h, ".center a") %>% html_text
    HScore<-str_extract(Score,"\\d{1,2}")%>% as.numeric()
    AScore<-str_extract(Score,"\\d{1,2}$")%>% as.numeric()
    if(year>=2017){
      ATeam <- html_nodes(h, ".right+ .left a") %>% html_text
    }
    ATeam=ATeam[1:length(AScore)]
    HTeam=HTeam[1:length(AScore)]
    Date=Date[1:length(AScore)]
    
    sched<-data.frame(ATeam,HTeam,AScore,HScore,Date)%>%
      mutate(yearID = year)
    return(sched)
  }
  itsched<-getItaly(2023)
  
  getLigue1<-function(year){
    yearcode <- str_c(year,"-", year+1)
    url <- str_c("https://fbref.com/en/comps/13/",yearcode,"/schedule/",yearcode,"-Ligue-1-Scores-and-Fixtures")
    h <- read_html(url) 
    Date <- html_nodes(h, ".left:nth-child(3) a") %>% html_text
    HTeam <- html_nodes(h, ".right a") %>% html_text
    ATeam <- html_nodes(h, ".center+ .left a") %>% html_text
    Score <- html_nodes(h, ".center a") %>% html_text
    HScore<-str_extract(Score,"\\d{1,2}")%>% as.numeric()
    AScore<-str_extract(Score,"\\d{1,2}$")%>% as.numeric()
    if(year>=2017){
      ATeam <- html_nodes(h, ".right+ .left a") %>% html_text
    }
    ATeam=ATeam[1:length(AScore)]
    HTeam=HTeam[1:length(AScore)]
    Date=Date[1:length(AScore)]
    
    sched<-data.frame(ATeam,HTeam,AScore,HScore,Date)%>%
      mutate(yearID = year)
    return(sched)
  }
  schedL1<-getLigue1(2023)
  
  getBundasliga<-function(year){
    yearcode <- str_c(year,"-", year+1)
    url <- str_c("https://fbref.com/en/comps/20/",yearcode,"/schedule/",yearcode,"-Bundesliga-Scores-and-Fixtures")
    h <- read_html(url) 
    Date <- html_nodes(h, ".left:nth-child(3) a") %>% html_text
    
    HTeam <- html_nodes(h, ".right a") %>% html_text
    ATeam <- html_nodes(h, ".center+ .left a") %>% html_text
    Score <- html_nodes(h, ".center a") %>% html_text
    HScore<-(str_extract(Score,"\\d{1,2}")%>% as.numeric())[1:length(Date)]
    AScore<-(str_extract(Score,"\\d{1,2}$")%>% as.numeric())[1:length(Date)]
    if(year>=2017){
      ATeam <- html_nodes(h, ".right+ .left a") %>% html_text
    }
    ATeam=ATeam[1:length(AScore)]
    HTeam=HTeam[1:length(AScore)]
    Date=Date[1:length(AScore)]
    
    sched<-data.frame(ATeam,HTeam,AScore,HScore,Date)%>%
      mutate(yearID = year)
    return(sched)
  }
  schedBun=getBundasliga(2023)
  dfsched<-rbind(schedEPL,schedSpa,itsched,schedL1,schedBun)%>%
    na.omit()
  ateam<-floteam%>% select("ATeam"="Squad","AwayFS"="TeamScore",yearID)
  hteam<-floteam%>% select("HTeam"="Squad","HomeFS"="TeamScore",yearID)
  dfsched<-left_join(dfsched,ateam, by =c("ATeam","yearID"))
  dfsched<-left_join(dfsched,hteam, by =c("HTeam","yearID"))%>%
    mutate(PD= AScore-HScore)%>%
    mutate(AFS=ifelse(PD>=0,PD*HomeFS,PD*(1-HomeFS)))%>%
    mutate(HFS=ifelse(PD>=0,-1*PD*(1-AwayFS),-1*PD*AwayFS))
  form<-dfsched
  dfschedaway= dfsched%>%
    group_by(yearID,ATeam)%>%
    mutate(AFS= sum(AFS))%>%
    mutate(AG=length(AFS))%>%
    select("Squad"="ATeam",yearID,AFS,AG)%>%
    slice(1)%>%
    ungroup()
  dfschedhome= dfsched%>%
    group_by(yearID,HTeam)%>%
    mutate(HFS= sum(HFS))%>%
    mutate(HG=length(HFS))%>%
    select("Squad"="HTeam",yearID,HFS,HG)%>%
    slice(1)%>%
    ungroup()
  dfsched<-left_join(dfschedaway,dfschedhome,by=c("yearID","Squad"))%>%
    mutate(SchedFS= (AFS+HFS)/(AG+HG))%>%
    na.omit()%>%
    select(Squad,yearID,SchedFS)
  dftotal=left_join(floteam,dfsched,by=c("yearID","Squad"))%>%
    na.omit()%>%
    mutate(FloStrength = LeagueRank*((TeamScore+2*SchedFS)/3))
  officialFlo<- dftotal%>%
    mutate(GF=GF*MP)%>%
    mutate(GA=GA*MP)%>%
    mutate(ShotDef=-1*ShotDef)%>%
    mutate(PassDef=-1*PassDef)
  formA<- form%>%
    select(Date,"Squad"=ATeam, "FS"="AFS")
  formH<- form%>%
    select(Date,"Squad"=HTeam, "FS"="HFS")
  form<-rbind(formA,formH)%>%
    mutate(Date=as.numeric(str_remove_all(Date,"\\-")))%>%
    arrange(-Date)
  form3<- form %>%
    group_by(Squad)%>%
    slice(1:3)%>%
    mutate(Form3=mean(FS))%>%
    slice(1)%>%
    ungroup()%>%
    select(Squad,Form3)
  form6<- form %>%
    group_by(Squad)%>%
    slice(1:6)%>%
    mutate(Form6=mean(FS))%>%
    slice(1)%>%
    ungroup()%>%
    select(Squad,Form6)
  form10<- form %>%
    group_by(Squad)%>%
    slice(1:10)%>%
    mutate(Form10=mean(FS))%>%
    slice(1)%>%
    ungroup()%>%
    select(Squad,Form10)
  officialFlo<-left_join(officialFlo,form3, by = "Squad")
  officialFlo<-left_join(officialFlo,form6, by = "Squad")
  officialFlo<-left_join(officialFlo,form10, by = "Squad")
  officialFlo<- officialFlo%>%
    mutate(FloStrength= (6+2*(Offense-Defense)+(SchedFS+Form3+Form6+Form10))/12)
  for (i in c(36:41,28:33,12:15)) {
    officialFlo[,i]=round(officialFlo[,i],2)
  }
  officialFlo<- officialFlo%>%
    arrange(-FloStrength)%>%
    mutate(Pts= round(3*MP*PtsPerc))
  return(officialFlo)
}
getSoccerPlayerRanks<- function(teamdf){
  ##############################
  #Basic
  ##############################
  h <- read_html("https://fbref.com/en/comps/Big5/stats/players/Big-5-European-Leagues-Stats") 
  stats <- html_nodes(h, "#stats_standard td") %>% html_text
  players<- length(stats)/37
  df<- data.frame(matrix(nrow=players,ncol=37))
  for (i in c(1:37)){
    for (j in c(1:players)){
      df[j,i]= stats[i + (j -1)* 37]
    }
  }
  colnames(df) <- html_nodes(h, "#stats_standard .center+ .poptip , .left+ .poptip.center , .poptip.sort_default_asc.left") %>% html_text
  playerdf<- df[,c(1:17,24:26)]
  
  ##############################
  #Shooting
  ##############################
  h <- read_html("https://fbref.com/en/comps/Big5/shooting/players/Big-5-European-Leagues-Stats") 
  stats <- html_nodes(h, "#stats_shooting td") %>% html_text
  players<- length(stats)/26
  df<- data.frame(matrix(nrow=players,ncol=26))
  for (i in c(1:26)){
    for (j in c(1:players)){
      df[j,i]= stats[i + (j -1)* 26]
    }
  }
  colnames(df) <- html_nodes(h, "#stats_shooting .center+ .poptip , .left+ .poptip.center , .poptip.sort_default_asc.left") %>% html_text
  df<-df[,c(1,2,4,7,10,11,17)]
  playerdf<-left_join(playerdf,df,by = c("Player","Nation","Born","Squad"))
  
  ##############################
  #Passing
  ##############################
  h <- read_html("https://fbref.com/en/comps/Big5/passing/players/Big-5-European-Leagues-Stats") 
  stats <- html_nodes(h, "#stats_passing td") %>% html_text
  players<- length(stats)/32
  df<- data.frame(matrix(nrow=players,ncol=32))
  for (i in c(1:32)){
    for (j in c(1:players)){
      df[j,i]= stats[i + (j -1)* 32]
    }
  }
  colnames(df) <- html_nodes(h, "#stats_passing .center+ .poptip , .left+ .poptip.center , .poptip.sort_default_asc.left") %>% html_text
  df<-df[,c(1,2,4,7,9,10,12,13,14,15,17,18,20,21,27,28,29,30)]
  playerdf<-left_join(playerdf,df,by = c("Player","Nation","Born","Squad"))
  
  ##############################
  #Defense
  ##############################
  h <- read_html("https://fbref.com/en/comps/Big5/defense/players/Big-5-European-Leagues-Stats") 
  stats <- html_nodes(h, "#stats_defense td") %>% html_text
  players<- length(stats)/25
  df<- data.frame(matrix(nrow=players,ncol=25))
  for (i in c(1:25)){
    for (j in c(1:players)){
      df[j,i]= stats[i + (j -1)* 25]
    }
  }
  colnames(df) <- html_nodes(h, "#stats_defense .center+ .poptip , .left+ .poptip.center , .poptip.sort_default_asc.left") %>% html_text
  df<-df[,c(1,2,4,7,9,10,11,12,13,14,17,18,21,23,24)]
  playerdf<-left_join(playerdf,df,by = c("Player","Nation","Born","Squad"))
  for (i in c(7:48)) {
    playerdf[,i]=as.numeric(playerdf[,i])
  }
  getGA<- teamdf%>%
    select(Squad,Defense)
  df<- left_join(playerdf,getGA,by="Squad")%>%
    mutate(TeamDef= MP*Defense)%>%
    mutate(Min=`90s`*90)%>%
    mutate(Dist=ifelse(is.na(Dist)==TRUE,0,Dist))%>%
    mutate(ShotDist=Dist*Sh)%>%
    group_by(Player,Nation,Born)%>%
    summarise(across(c(5:19,21:45,47,48), sum))
  getSquads<- playerdf%>%
    group_by(Player,Nation,Born)%>%
    mutate(sq=length(Squad))%>%
    mutate(Comp=ifelse(first(Comp)==last(Comp),Comp,"Multiple"))%>%
    mutate(Squad=ifelse(sq==1,Squad,"Multiple"))%>%
    slice(1)%>%
    ungroup()%>%
    select(Player,Nation,Born,Squad,Comp,Age,Pos)
  df<-left_join(getSquads,df,by = c("Player","Nation","Born"))
  newdf<-df%>%
    mutate(Pos=ifelse(Pos %in%c("DF","DF,MF","MF,DF"),"DF",Pos))%>%
    mutate(Pos=ifelse(Pos %in%c("DF,FW","MF"),"MF",Pos))%>%
    mutate(Pos=ifelse(Pos %in%c("FW","FW,DF","FW,MF","MF,FW"),"FW",Pos))%>%
    filter(Min>(20*max(MP))&Pos!="GK")%>%
    mutate(ShotDist=round(ShotDist/(Sh+.000001),2))%>%
    mutate(Gls=Gls-PK)%>%
    mutate(Shooting=round(10*ShotDist*((SoT+4*Gls)*SoT)/((Sh+.00001)*Min),7))%>%
    mutate(Passing=(20000000*`1/3`+150000000*KP+Cmp*Cmp*PrgDist)/(Att*10000*Min))%>%
    mutate(Control= sqrt((PrgR+8*PrgC)/Min))%>%
    mutate(TeamDef=TeamDef/MP)%>%
    mutate(Defense=(6*`Def 3rd`+4*`Mid 3rd`+2*`Att 3rd`+Clr+2*Int+Blocks-20*Err)/(Min))%>%
    mutate(Defense=(Defense-mean(Defense))/sd(Defense))%>%
    mutate(Defense=(Defense+4)/(TeamDef+4))%>%
    mutate(Shooting=log(Shooting+.001))%>%
    mutate(Passing=log(Passing))%>%
    mutate(Shooting=(Shooting-mean(Shooting))/sd(Shooting))%>%
    mutate(Defense=(Defense-mean(Defense))/sd(Defense))%>%
    mutate(Control=(Control-mean(Control))/sd(Control))%>%
    mutate(Passing=(Passing-mean(Passing))/sd(Passing))
  playerdf<-newdf%>% 
    select(Player,Nation,Born,Squad,Age,Comp,Pos, MP,Min,Gls,Ast,Sh,SoT,Tkl,Shooting,Passing,Control,Defense)%>%
    mutate(FloStrength=ifelse(Pos=="FW",(5*Shooting+Passing+Control)/7,NA))%>%
    mutate(FloStrength=ifelse(Pos=="MF",(3*Shooting+3*Passing+2*Control+1*Defense)/7,FloStrength))%>%
    mutate(FloStrength=ifelse(Pos=="DF",(7*Defense+2*Passing+Control+Shooting)/11,FloStrength))%>%
    mutate(yearID=2023)%>%
    group_by(yearID)%>%
    mutate(FloStrength=(FloStrength-mean(FloStrength))/sd(FloStrength))%>%
    ungroup()%>%
    select(-yearID)%>%
    mutate(FloValue=FloStrength*Min/1351.351 )%>%
    mutate(League = ifelse(Comp=="es La Liga","La Liga",NA))%>%
    mutate(League = ifelse(Comp=="de Bundesliga","Bundesliga",League))%>%
    mutate(League = ifelse(Comp=="eng Premier League","EPL",League))%>%
    mutate(League = ifelse(Comp=="fr Ligue 1","Ligue 1",League))%>%
    mutate(League = ifelse(Comp=="it Serie A","Serie A",League))%>%
    mutate(Nation= str_extract(Nation,"[A-Z]{1,3}"))
  for (i in c(15:20)) {
    playerdf[,i]=round(playerdf[,i],2)
  }
  playerdf<-playerdf%>%
    select(Player,Nation,Born,Squad,League,Pos, MP,Min,Gls,Ast,Sh,SoT,Tkl,Shooting,Passing,Control,Defense,FloStrength,FloValue)%>%
    arrange(-FloValue)
  ################################
  #Goalkeepers
  ################################
  h <- read_html("https://fbref.com/en/comps/Big5/keepers/players/Big-5-European-Leagues-Stats") 
  stats <- html_nodes(h, "#stats_keeper td") %>% html_text
  players<- length(stats)/27
  df<- data.frame(matrix(nrow=players,ncol=27))
  for (i in c(1:27)){
    for (j in c(1:players)){
      df[j,i]= stats[i + (j -1)* 27]
    }
  }
  colnames(df) <- html_nodes(h, "#stats_keeper .center+ .poptip , .left+ .poptip.center , .poptip.sort_default_asc.left") %>% html_text
  df<-df[,1:16]%>%
    mutate(Min=str_remove(Min,"\\,"))
  for (i in c(8:16)) {
    df[,i]=as.numeric(df[,i])
  }
  df1<-df%>%
    filter(Min>300)%>%
    group_by(Player,Nation,Born,Age)%>%
    summarise(across(c(4:11), sum))%>%
    mutate(SavePer=Saves/SoTA)%>%
    mutate(GA90=GA/`90s`)%>%
    mutate(Goalkeeping=log(SavePer*GA90))%>%
    ungroup()%>%
    mutate(Goalkeeping=-1*(Goalkeeping-mean(Goalkeeping))/sd(Goalkeeping))
  getSquads<- df%>%
    filter(Min>300)%>%
    group_by(Player,Nation,Born)%>%
    mutate(sq=length(Squad))%>%
    mutate(Comp=ifelse(first(Comp)==last(Comp),Comp,"Multiple"))%>%
    mutate(Squad=ifelse(sq==1,Squad,"Multiple"))%>%
    slice(1)%>%
    ungroup()%>%
    select(Player,Nation,Born,Squad,Comp,Pos)
  df<-left_join(getSquads,df1,by = c("Player","Nation","Born"))
  gkdf<-df%>%select(Player,Nation,Pos,Squad,Comp,Age,Born,MP,Min,Saves,SoTA,Goalkeeping)%>%
    mutate(FloValue = Goalkeeping*(Min/1620))%>%
    mutate(League = ifelse(Comp=="es La Liga","La Liga",NA))%>%
    mutate(League = ifelse(Comp=="de Bundesliga","Bundesliga",League))%>%
    mutate(League = ifelse(Comp=="eng Premier League","EPL",League))%>%
    mutate(League = ifelse(Comp=="fr Ligue 1","Ligue 1",League))%>%
    mutate(League = ifelse(Comp=="it Serie A","Serie A",League))%>%
    mutate(Nation= str_extract(Nation,"[A-Z]{1,3}"))
  #################################
  write_csv(gkdf,"/Users/seanfloersch/FloStrength/FloStrengthCurrent/Soccer/GK.csv")
  write_csv(playerdf,"/Users/seanfloersch/FloStrength/FloStrengthCurrent/Soccer/Players.csv")
}










