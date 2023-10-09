#basic functions
getDate <- function(x){
  x <- Sys.Date()
  x <- str_remove_all(x, "-")
  return(x)
}
getyestDate <- function(x){
  x <- Sys.Date() -1
  x <- str_remove_all(x, "-")
  return(x)
}
#read in data
NBAResults <- read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/ComNBAResults")
NBAModel <- read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBAModel")
NBABets <-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/nbayestbetdf")
NBAPredTeam<- read.csv("~/FloStrength/NBAFloStrength/predteam23")
NBAPredPlay<- read.csv("~/FloStrength/NBAFloStrength/playpred23")
#get data
getNBAplayers <- function(year) {
  url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,"_totals.html")
  h <- read_html(url) 
  len <- length(html_nodes(h, "td") %>% html_text %>% as.numeric)/29
  stats <- html_nodes(h, "td") %>% html_text
  df<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:29)){
      marker = j + (i -1)* 29
      df[i,j]<- stats[marker]
    }
  }
  
  for (i in c(3, 5:29)){
    df[i] <- as.numeric(unlist(df[i]))
  }
  df <- df
  col <-html_nodes(h, ".center+ .poptip") %>% html_text
  colnames(df) <- col
  url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,"_advanced.html")
  h <- read_html(url) 
  stats <- html_nodes(h, "td") %>% html_text
  dfadv<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:28)){
      marker = j + (i -1)* 28
      dfadv[i,j]<- stats[marker]
    }
  }
  for (i in c(3, 5:28)){
    dfadv[i] <- as.numeric(unlist(dfadv[i]))
  }
  col <-html_nodes(h, ".center+ .poptip") %>% html_text
  colnames(dfadv) <- col
  dfadv <- dfadv[, c(1,4,7:18, 20:23,25:28)]
  dfcomp <- left_join(df, dfadv, by = c("Player", "Tm"))%>%
    mutate(yearID = year)
  dfcomp <- dfcomp  %>%
    group_by(Player)%>%
    mutate(Tm = last(Tm))%>%
    slice(1)%>%
    ungroup()%>%
    mutate(MPG = MP / G) %>%
    filter(MPG > 12.9 & G > 5) %>%
    mutate(DWS = DWS / MP) %>%
    mutate(Pos = str_extract(Pos, "[A-Z]{1,2}")) %>%
    rename("FG2"="2P","FG2A"="2PA","FG3"="3P","FG3A"="3PA") %>%
    select(-`eFG%`, -VORP, -`AST%`,-`TOV%`, -`WS/48`, -FTr, -`3PAr`, -DWS, -OWS, -WS)%>%
    mutate(Scoring = ((FG2*FG2 / (FG2A +.00001)) + 1.5 *(FG3 * FG3 /(.00001 + FG3A)) + .5 * (FT * FT / (FTA + .00001))) / MP) %>%
    mutate(Passing = (AST/MP) - 2 * (TOV/MP)) %>%
    mutate(Rebounding =  sqrt((ORB/MP) + (DRB/MP))) %>%
    mutate(defedit = (MPG-24)/6) %>%
    mutate(Defense = (3*(STL) + (BLK) + .75*G*(DBPM+defedit))/MP) %>%
    group_by(yearID) %>%
    mutate(Scoring = (Scoring - mean(Scoring)) / sd(Scoring))%>%
    mutate(Passing = (Passing - mean(Passing)) / sd(Passing))%>%
    mutate(Rebounding = (Rebounding - mean(Rebounding)) / sd(Rebounding)) %>%
    ungroup() %>%  
    group_by(yearID, Pos) %>%
    mutate(Defense = (Defense - mean(Defense)) / sd(Defense))%>%
    ungroup() %>%  
    group_by(yearID) %>%
    mutate(Defense = (Defense - mean(Defense)) / sd(Defense)) %>%
    ungroup %>%
    mutate(FloStrength = .59* Scoring + .64*Defense + .11 * Passing + .077 * Rebounding) %>%
    mutate(Value = FloStrength * MP / 400) %>%
    arrange(-Value)
  return(dfcomp)
}
getNBATeam <- function(year){
  url <- str_c("https://www.basketball-reference.com/leagues/NBA_", year,".html")
  h <- read_html(url) 
  stats <- html_nodes(h, "#per_game-team tbody td") %>% html_text
  df<- data.frame()
  for (i in c(1:30)) {
    for (j in c(1:24)){
      marker = j + (i -1)* 24
      df[i,j]<- stats[marker]
    }
  }
  names4cols <- html_nodes(h, "#per_game-team thead .center+ .center") %>% html_text
  colnames(df)<- names4cols
  oppstats <- html_nodes(h, "#per_game-opponent tbody td") %>% html_text
  oppodf<- data.frame()
  for (i in c(1:30)) {
    for (j in c(1:24)){
      marker = j + (i -1)* 24
      oppodf[i,j]<- oppstats[marker]
    }
  }
  names4cols <- html_nodes(h, "#per_game-team thead .center+ .center") %>% html_text
  for (i in c(1:length(names4cols))){
    names4cols[i] = str_c("opp", names4cols[i])
    if (i == 1) {
      names4cols[i] <- "Team"
    }
  }
  colnames(oppodf)<- names4cols
  teamstats <- left_join(df, oppodf, by = "Team") %>%
    mutate(yearID = year)
  szn <- html_nodes(h, "#advanced-team tbody th+ .left , #advanced-team tbody .right:nth-child(5) , #advanced-team tbody .right:nth-child(4)") %>% html_text
  oppodf<- data.frame()
  for (i in c(1:30)) {
    for (j in c(1:3)){
      marker = j + (i -1)* 3
      oppodf[i,j]<- szn[marker]
    }
  }
  colnames(oppodf)<- c("Team", "W", "L")
  teamstats <- left_join(teamstats, oppodf, by = "Team")%>%
    mutate(Team = str_remove(Team, "\\*")) %>%
    na.omit
  for (i in c(2:50)) {
    teamstats[i] <- as.numeric(unlist(teamstats[i]))
  }
  teamstats <- teamstats %>%
    mutate(Wpct = W / G) %>%
    mutate(yearID = 2023) %>%
    mutate(MPG = MP / 5) %>%
    mutate(Playmaking = (AST - 2* TOV - oppBLK)- (oppAST - 2* oppTOV - BLK)) %>%
    mutate(Rebounding1 = (DRB -2* oppORB)) %>%
    mutate(Rebounding2 = (oppDRB -2* ORB)) %>%
    mutate(Rebounding = Rebounding1 / Rebounding2) %>%
    mutate(Defense = oppPTS) %>%
    mutate(Shooting = (`2P` * `2P%` + 1.5 *`3P`* `3P%`+ .5 *FT*`FT%`)/(`opp2P` * `opp2P%` + 1.5 *`opp3P`* `opp3P%`+ .5 *oppFT*`oppFT%`))%>%
    group_by(yearID) %>%
    mutate(Defense = (Defense - mean(Defense)) / sd(Defense)) %>%
    mutate(Shooting = (Shooting - mean(Shooting)) / sd(Shooting)) %>%
    mutate(Rebounding = (Rebounding - mean(Rebounding)) / sd(Rebounding)) %>%
    mutate(Playmaking = (Playmaking - mean(Playmaking)) / sd(Playmaking)) %>%
    ungroup() %>%
    mutate(TeamStrength = 0.499851+0.027328* Playmaking+0.006801*Rebounding-0.008344*Defense+0.123311*Shooting)
  return(teamstats)
}
getNBAsched <- function(team23,NBAResults){
  sched <- NBAResults %>% 
    select(AwayTeam,HomeTeam, PD, WTeam)
  teams <- c("Minnesota Timberwolves" ="MIN", "Memphis Grizzlies"="MEM", "Milwaukee Bucks"="MIL", "Charlotte Hornets"="CHA", "Phoenix Suns"="PHO", "Atlanta Hawks"="ATL", "Utah Jazz"="UTA", "San Antonio Spurs"="SAS", "Brooklyn Nets"="BRK", "Denver Nuggets"="DEN", "Los Angeles Lakers"="LAL", "Boston Celtics"="BOS", "Chicago Bulls"="CHI", "Indiana Pacers"="IND", "Golden State Warriors"="GSW", "Sacramento Kings"="SAC", "Miami Heat"="MIA", "Philadelphia 76ers"="PHI", "Houston Rockets"="HOU", "Toronto Raptors"="TOR", "New Orleans Pelicans"="NOP", "Washington Wizards"="WAS", "Los Angeles Clippers"="LAC", "Dallas Mavericks"="DAL", "Cleveland Cavaliers"="CLE", "New York Knicks"="NYK", "Portland Trail Blazers"="POR", "Detroit Pistons"="DET", "Orlando Magic"="ORL", "Oklahoma City Thunder"="OKC")
  team23$Team<- as.character(teams[team23$Team])
  Ateam <- team23 %>%
    select("AwayTeam"=Team, "AwayFS" =TeamStrength)
  Hteam <- team23 %>%
    select("HomeTeam"=Team,"HomeFS"= TeamStrength)
  sched <- left_join(sched, Ateam, by = "AwayTeam")
  sched<- left_join(sched, Hteam, by = "HomeTeam") %>%
    mutate(ASched = 0) %>%
    mutate(HSched = 0)
  ind <- which(sched$PD > 0)
  sched$ASched[ind] = sched$PD[ind] * sched$HomeFS[ind]
  sched$HSched[ind] = -1 * sched$PD[ind] * (1-sched$AwayFS[ind])
  ind <- which(sched$PD < 0)
  sched$HSched[ind] = -1 *sched$PD[ind] * sched$AwayFS[ind]
  sched$ASched[ind] =  sched$PD[ind] * (1-sched$HomeFS[ind])
  AFS <- sched %>%
    group_by(AwayTeam) %>%
    mutate(ASched = sum(ASched)) %>%
    mutate(AG = length(ASched)) %>%
    slice(1) %>%
    ungroup()%>%
    select("Team" = AwayTeam, ASched, AG)
  HFS <- sched %>%
    group_by(HomeTeam) %>%
    mutate(HSched = sum(HSched)) %>%
    mutate(HG = length(HSched)) %>%
    slice(1) %>%
    ungroup() %>%
    select("Team" = HomeTeam, HSched, HG)
  FSSc <- left_join(AFS, HFS, by = "Team") %>%
    mutate(SchedRating = (ASched + HSched)/ (AG+HG)) %>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(SchedRating = ((SchedRating-mean(SchedRating))/ sd(SchedRating))/6) %>%
    mutate(SchedRating = SchedRating + .5) %>%
    ungroup()%>%
    select(Team, SchedRating)
  return(FSSc)
}
getStreaks <- function(team23,NBAResults){
  sched <- NBAResults%>%
    mutate(date = str_extract(gameID, "\\d{1,8}$")) %>% 
    select(AwayTeam,HomeTeam, PD, WTeam,date) %>%
    ungroup() %>% 
    mutate(Date = NA)
  for (i in c(1:length(unique(sched$date)))) {
    i = i
    x <- unique(sched$date)[i]
    ind <- which(sched$date == x)
    sched$Date[ind] <- i
  }
  sched <- sched %>%
    select(-date)
  teams <- c("Minnesota Timberwolves" ="MIN", "Memphis Grizzlies"="MEM", "Milwaukee Bucks"="MIL", "Charlotte Hornets"="CHA", "Phoenix Suns"="PHO", "Atlanta Hawks"="ATL", "Utah Jazz"="UTA", "San Antonio Spurs"="SAS", "Brooklyn Nets"="BRK", "Denver Nuggets"="DEN", "Los Angeles Lakers"="LAL", "Boston Celtics"="BOS", "Chicago Bulls"="CHI", "Indiana Pacers"="IND", "Golden State Warriors"="GSW", "Sacramento Kings"="SAC", "Miami Heat"="MIA", "Philadelphia 76ers"="PHI", "Houston Rockets"="HOU", "Toronto Raptors"="TOR", "New Orleans Pelicans"="NOP", "Washington Wizards"="WAS", "Los Angeles Clippers"="LAC", "Dallas Mavericks"="DAL", "Cleveland Cavaliers"="CLE", "New York Knicks"="NYK", "Portland Trail Blazers"="POR", "Detroit Pistons"="DET", "Orlando Magic"="ORL", "Oklahoma City Thunder"="OKC")
  team23$Team<- as.character(teams[team23$Team])
  Ateam <- team23 %>%
    select("AwayTeam"=Team, "AwayFS" =TeamStrength)
  Hteam <- team23 %>%
    select("HomeTeam"=Team,"HomeFS"= TeamStrength)
  sched <- left_join(sched, Ateam, by = "AwayTeam")
  sched<- left_join(sched, Hteam, by = "HomeTeam") %>%
    mutate(ASched = 0) %>%
    mutate(HSched = 0)
  ind <- which(sched$PD > 0)
  sched$ASched[ind] = sched$PD[ind] * sched$HomeFS[ind]
  sched$HSched[ind] = -1 * sched$PD[ind] * (1-sched$AwayFS[ind])
  ind <- which(sched$PD < 0)
  sched$HSched[ind] = -1 *sched$PD[ind] * sched$AwayFS[ind]
  sched$ASched[ind] =  sched$PD[ind] * (1-sched$HomeFS[ind])
  away <- sched %>%
    select("Team" = AwayTeam,"Sched"= ASched, Date)
  home <- sched %>%
    select("Team" = HomeTeam, "Sched" =HSched,Date)
  sched3 <- rbind(away,home) %>%
    arrange(Date) %>%
    group_by(Team) %>%
    slice(1:3) %>%
    mutate(last3games = sum(Sched)/3) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last3games = ((last3games-mean(last3games))/ sd(last3games))/6) %>%
    mutate(last3games = last3games + .5) %>%
    ungroup()%>%
    select(Team, last3games)
  sched5 <- rbind(away,home) %>%
    arrange(Date) %>%
    group_by(Team) %>%
    slice(1:5) %>%
    mutate(last5games = sum(Sched)/5) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last5games = ((last5games-mean(last5games))/ sd(last5games))/6) %>%
    mutate(last5games = last5games + .5) %>%
    ungroup()%>%
    select(Team, last5games)
  sched7 <- rbind(away,home) %>%
    arrange(Date) %>%
    group_by(Team) %>%
    slice(1:7) %>%
    mutate(last7games = sum(Sched)/7) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last7games = ((last7games-mean(last7games))/ sd(last7games))/6) %>%
    mutate(last7games = last7games + .5) %>%
    ungroup()%>%
    select(Team, last7games)
  sched10 <- rbind(away,home) %>%
    arrange(Date) %>%
    group_by(Team) %>%
    slice(1:10) %>%
    mutate(last10games = sum(Sched)/10) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last10games = ((last10games-mean(last10games))/ sd(last10games))/6) %>%
    mutate(last10games = last10games + .5) %>%
    ungroup()%>%
    select(Team, last10games)
  lastx <- left_join(sched3,sched5, by = "Team")
  lastx <- left_join(lastx,sched7, by = "Team")
  lastx <- left_join(lastx,sched10, by = "Team")
  return(lastx)
}
getNBArank <- function(player23, team23, sched23, predteam23,lately23){
  getInjuries <- function(x){
    h <- read_html("https://www.espn.com/nba/injuries") 
    inj <- html_nodes(h, "#fittPageContainer .AnchorLink") %>% html_text
    status <-html_nodes(h, ".plain") %>% html_text
    len <- length(status)
    inj <- inj[1:len]
    injuries <- data.frame(inj, status) %>%
      filter(status == "Out")
    inj <- injuries$inj
    return(inj)
  }
  injured <- getInjuries(x)
  currentplay <- player23 %>%
    filter(!Player %in%injured) %>%
    group_by(Tm) %>%
    mutate(propmin = MPG / sum(MPG)) %>%
    mutate(wfs = propmin*FloStrength) %>%
    mutate(PlayerStrength = sum(wfs)) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(PlayerStrength = ((PlayerStrength-mean(PlayerStrength))/ sd(PlayerStrength))/6) %>%
    mutate(PlayerStrength = PlayerStrength + .5) %>%
    ungroup() %>%
    select("Team" = Tm, PlayerStrength)
  ind <- which(currentplay$Team == "CHO")
  currentplay$Team[ind] = "CHA"
  currteam <- team23 
  teams <- c("Minnesota Timberwolves" ="MIN", "Memphis Grizzlies"="MEM", "Milwaukee Bucks"="MIL", "Charlotte Hornets"="CHA", "Phoenix Suns"="PHO", "Atlanta Hawks"="ATL", "Utah Jazz"="UTA", "San Antonio Spurs"="SAS", "Brooklyn Nets"="BRK", "Denver Nuggets"="DEN", "Los Angeles Lakers"="LAL", "Boston Celtics"="BOS", "Chicago Bulls"="CHI", "Indiana Pacers"="IND", "Golden State Warriors"="GSW", "Sacramento Kings"="SAC", "Miami Heat"="MIA", "Philadelphia 76ers"="PHI", "Houston Rockets"="HOU", "Toronto Raptors"="TOR", "New Orleans Pelicans"="NOP", "Washington Wizards"="WAS", "Los Angeles Clippers"="LAC", "Dallas Mavericks"="DAL", "Cleveland Cavaliers"="CLE", "New York Knicks"="NYK", "Portland Trail Blazers"="POR", "Detroit Pistons"="DET", "Orlando Magic"="ORL", "Oklahoma City Thunder"="OKC")
  currteam$Team<- as.character(teams[currteam$Team])
  currpred <- predteam23 %>%
    select("PredRating"= "predWP", "Team" = "Tm")
  prdf <- left_join(currteam, currentplay, by = "Team")
  prdf <- left_join(prdf, currpred, by = "Team")
  prdf <- left_join(prdf, lately23, by = "Team")
  prdf <- left_join(prdf, sched23, by = "Team") %>%
    mutate(Rank = (1*TeamStrength+1.2*PlayerStrength+.5*SchedRating+.2*last3games+.16*last5games+.14*last7games+.1*last10games)/ 3.3) %>% 
    arrange(-Rank)
  NBAModel <- read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBAModel")
  colmod <- c(28:30,32:35,48)
  NBAModel<- NBAModel[,colmod]
  logmod <- glm(AWin~., data = NBAModel, family = binomial)
  prdf<-prdf%>%
    mutate(Rank = logmod$coefficients[1]+logmod$coefficients[2]*PlayerStrength+logmod$coefficients[3]*TeamStrength+logmod$coefficients[4]*SchedRating+logmod$coefficients[5]*last3games+logmod$coefficients[6]*last5games+logmod$coefficients[7]*last7games+logmod$coefficients[8]*last10games)%>%
    mutate(Rank = (Rank-mean(Rank))/sd(Rank))%>%
    mutate(Rank = (3+Rank)/6)%>%
    arrange(-Rank)
  return(prdf)
}
# update yesterday
updateNBAyest <- function(yestgameID){
  df <- read.csv("~/FloStrength/NBAFloStrength/yestmodpred") %>%
    select(AwayTeam,HomeTeam, OU,Spread,gameID, AOdd, HOdd, PredWinner, BookWin)
  h <- read_html(str_c("https://www.espn.com/nba/scoreboard/_/date/",yestgameID))
  AwayTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
  HomeTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
  teams <- c("Timberwolves" ="MIN", "Grizzlies"="MEM", "Bucks"="MIL", "Hornets"="CHA", "Suns"="PHO", "Hawks"="ATL", "Jazz"="UTA", "Spurs"="SAS", "Nets"="BRK", "Nuggets"="DEN", "Lakers"="LAL", "Celtics"="BOS", "Bulls"="CHI", "Pacers"="IND", "Warriors"="GSW", "Kings"="SAC", "Heat"="MIA", "76ers"="PHI", "Rockets"="HOU", "Raptors"="TOR", "Pelicans"="NOP", "Wizards"="WAS", "Clippers"="LAC", "Mavericks"="DAL", "Cavaliers"="CLE", "Knicks"="NYK", "Trail Blazers"="POR", "Pistons"="DET", "Magic"="ORL", "Thunder"="OKC")
  AwayTeam<- as.character(teams[AwayTeam])
  HomeTeam<- as.character(teams[HomeTeam])
  AwayScore <- html_nodes(h, ".ScoreboardScoreCell__Item--away .ScoreCell_Score--scoreboard") %>% html_text %>% as.numeric
  HomeScore <- html_nodes(h, ".ScoreboardScoreCell__Item--home .ScoreCell_Score--scoreboard") %>% html_text%>% as.numeric
  yestdf <- data.frame(AwayTeam, HomeTeam, AwayScore, HomeScore) %>%
    mutate(gameID = str_c(AwayTeam, HomeTeam, yestgameID)) %>%
    mutate(PD = AwayScore - HomeScore) %>%
    mutate(WTeam = AwayTeam )
  ind <- which(yestdf$PD < 0)
  yestdf$WTeam[ind] = yestdf$HomeTeam[ind]
  yestdf <- yestdf %>% select(-AwayTeam, -HomeTeam)
  df <- left_join(df, yestdf, by = "gameID")
  dfcom<- read.csv("~/FloStrength/NBAFloStrength/ComNBAResults")
  df<- rbind(df, dfcom)
  return(df)
}
updateNBAmod <- function(NBAResults){
  df <- read.csv("~/FloStrength/NBAFloStrength/yestmodpred")
  res <- NBAResults %>%
    select(gameID, AwayScore,HomeScore, PD, WTeam)
  df <- left_join(df, res, by = "gameID")%>%
    mutate(BookCorr = ifelse(BookWin == WTeam, 1,0)) %>%
    mutate(FloCorr = ifelse(PredWinner == WTeam, 1,0)) %>%
    mutate(AWin = ifelse(WTeam==AwayTeam,1,0)) %>%
    select(-nnpred,-rfpred, -gbmpred,-AOdd1)
  dfcom<- read.csv("~/FloStrength/NBAFloStrength/NBAModel")
  ind<- which(colnames(df)%in% colnames(dfcom))
  df<-df[,ind]
  df<- rbind(df, dfcom)
  return(df)
}
updateNBAbets <- function(nbabets, nbaresults){
  nbabetsp <- nbabets %>%
    filter(Earnings == .91)
  nbasp <- nbaresults %>%
    select(BookWin, Spread, WTeam,PD,gameID) %>%
    mutate(PD = abs(PD)) %>%
    mutate(Spread = as.numeric(Spread)) %>%
    mutate(Spread = abs(Spread)) %>%
    mutate(actSP = ifelse((BookWin == WTeam & PD>Spread),-1*Spread,Spread)) %>%
    select(gameID, actSP)
  nbaspread<- left_join(nbabetsp, nbasp, by = "gameID") %>%
    mutate(Bet = as.numeric(Bet)) %>%
    mutate(Earnings = ifelse(Bet == actSP, .91, -1))
  spreadearn = sum(nbaspread$Earnings)
  
  nbast <- nbabets %>%
    filter(Earnings != .91)
  nbastr <- nbaresults %>%
    select(WTeam,gameID)
  nbast <- left_join(nbast, nbastr, by = "gameID") %>%
    mutate(Earnings = ifelse(Bet == WTeam, Earnings, -1))
  strearn = sum(nbast$Earnings)
  nbaearn = (strearn + spreadearn)
  day = str_remove(nbast$gameID[1], "\\[A-Z]{1,6}")
  df <- data.frame(day, nbaearn)%>%
    mutate(day = str_remove(day, "[A-Z]{1,6}"))%>%
    mutate(Net = ifelse(nbaearn>0, "pos", "neg")) %>%
    select("Day"="day", "Earn"=nbaearn,Net) %>%
    mutate(Sport = "NBA") %>%
    na.omit()
  dfcomp <-read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBABetsDF")
  df <-  rbind(df, dfcomp)
  return(df)
}
#predict today
predictNBAtoday <- function(PowerRanking,todgameID){
  h <- read_html(str_c("https://www.cbssports.com/nba/scoreboard/",todgameID))
  AwayTeam <- html_nodes(h, "tr:nth-child(1) .rank+ a") %>% html_text
  HomeTeam <- html_nodes(h, "tr+ tr .rank+ a") %>% html_text
  OU <- html_nodes(h, ".in-progress-odds-away") %>% html_text %>% str_remove("o") %>% as.numeric()
  Spread <- html_nodes(h, ".in-progress-odds-home") %>% html_text
  ind <- which(Spread == "PK")
  Spread[ind] = 0
  Spread <- Spread %>% as.numeric()
  df <- data.frame(AwayTeam, HomeTeam, OU, Spread)
  h <- read_html("https://www.sportsline.com/nba/odds/money-line/")
  AwayTeam <- html_nodes(h, ".away-team .hMTCEK") %>% html_text
  ovr <-length(df$AwayTeam)
  AwayTeam<- AwayTeam[1:ovr]
  HomeTeam <- html_nodes(h, ".home-team .hMTCEK") %>% html_text
  HomeTeam<- HomeTeam[1:ovr]
  AOdd <- html_nodes(h, ".away-team .projected-score+ td .primary") %>% html_text%>% str_remove("\\+") %>% as.numeric()
  AOdd<- AOdd[1:ovr]
  HOdd <- html_nodes(h, ".home-team .projected-score+ td .primary") %>% html_text%>% str_remove("\\+") %>% as.numeric()
  HOdd<- HOdd[1:ovr]
  BookOdd <- data.frame(AwayTeam,HomeTeam,AOdd,HOdd)
  teams <- c("Timberwolves" ="MIN", "Grizzlies"="MEM", "Bucks"="MIL", "Hornets"="CHA", "Suns"="PHO", "Hawks"="ATL", "Jazz"="UTA", "Spurs"="SAS", "Nets"="BRK", "Nuggets"="DEN", "Lakers"="LAL", "Celtics"="BOS", "Bulls"="CHI", "Pacers"="IND", "Warriors"="GSW", "Kings"="SAC", "Heat"="MIA", "76ers"="PHI", "Rockets"="HOU", "Raptors"="TOR", "Pelicans"="NOP", "Wizards"="WAS", "Clippers"="LAC", "Mavericks"="DAL", "Cavaliers"="CLE", "Knicks"="NYK", "Trail Blazers"="POR", "Pistons"="DET", "Magic"="ORL", "Thunder"="OKC")
  df$AwayTeam<- as.character(teams[df$AwayTeam])
  df$HomeTeam<- as.character(teams[df$HomeTeam])
  BookOdd$AwayTeam<- as.character(teams[BookOdd$AwayTeam])
  BookOdd$HomeTeam<- as.character(teams[BookOdd$HomeTeam])
  df <- df %>% 
    mutate(gameID = str_c(AwayTeam, HomeTeam, todgameID))
  BookOdd <- BookOdd %>% 
    mutate(gameID = str_c(AwayTeam, HomeTeam, todgameID)) %>%
    select(-AwayTeam, -HomeTeam)
  predAct <- left_join(BookOdd, df, by ="gameID") %>% 
    mutate(BookWin = ifelse(Spread>0,AwayTeam, HomeTeam)) %>%
    rename("Team"="AwayTeam")%>%
    na.omit()
  predAct <- left_join(predAct,PowerRanking, by = "Team") %>%
    rename("AwayTeam" ="Team", "Team" = "HomeTeam", "AwayPlayer"= "PlayerStrength", "AwayTeamStr"= "TeamStrength", "AwaySched"= "SchedRating", "AwayPred"= "PredRating", "AwayRank"= "Rank", "Awaylast3games"= "last3games", "Awaylast5games"= "last5games", "Awaylast7games"= "last7games", "Awaylast10games"= "last10games")
  predAct <- left_join(predAct,PowerRanking, by = "Team") %>%
    rename("HomeTeam" ="Team",  "HomePlayer"= "PlayerStrength", "HomeTeamStr"= "TeamStrength", "HomeSched"= "SchedRating", "HomePred"= "PredRating", "HomeRank"= "Rank", "Homelast3games"= "last3games", "Homelast5games"= "last5games", "Homelast7games"= "last7games", "Homelast10games"= "last10games") %>%
    mutate(PlayerDiff = AwayPlayer - HomePlayer) %>%
    mutate(TeamDiff = AwayTeamStr- HomeTeamStr) %>%
    mutate(SchedDiff = AwaySched - HomeSched) %>%
    mutate(PredDiff = AwayPred - HomePred) %>%
    mutate(last3Diff = Awaylast3games - Homelast3games) %>%
    mutate(last5Diff = Awaylast5games - Homelast5games) %>%
    mutate(last7Diff = Awaylast7games - Homelast7games) %>%
    mutate(last10Diff = Awaylast10games - Homelast10games) %>%
    mutate(OvrDiff = AwayRank - HomeRank) %>%
    mutate(WinProb = .45 + OvrDiff) %>%
    mutate(PredWinner = ifelse(WinProb>.5,AwayTeam,HomeTeam)) %>%
    relocate(BookWin, PredWinner)%>%
    mutate(AOdd1 = ifelse(AOdd>0,(1/((AOdd/91)+1)), ((-1*(AOdd/110))/(-1*(AOdd/110)+1))))
  modeldf<- read.csv("~/FloStrength/NBAFloStrength/NBAModel") %>%
    mutate(AOdd1 = ifelse(AOdd>0,(1/((AOdd/91)+1)), ((-1*(AOdd/110))/(-1*(AOdd/110)+1))))
  set.seed(1212)
  logmod <- glm(AWin~PlayerDiff+TeamDiff+SchedDiff+last3Diff+last5Diff+last7Diff+last10Diff+AOdd1, data = modeldf,family = binomial)
  logpred1 <- predict(logmod, newdata = predAct, type = "response")
  set.seed(1212)
  linmod <- lm(PD~PlayerDiff+TeamDiff+SchedDiff+last3Diff+last5Diff+last7Diff+last10Diff+AOdd1, data = modeldf)
  linpred1 <- predict(linmod, newdata = predAct)
  logpred <- ifelse(logpred1>.5, 1,0)
  linpred <- ifelse(linpred1>0, 1,0)
  set.seed(1212)
  svmmod = svm(formula = AWin~PlayerDiff+TeamDiff+SchedDiff+last3Diff+last5Diff+last7Diff+last10Diff+AOdd1, data = modeldf, type = 'C-classification', kernel = 'linear')
  svmpred <- as.vector(predict(svmmod, newdata = predAct, type = "response"))
  set.seed(1212)
  rfmod <- randomForest(AWin~TeamDiff+PlayerDiff+SchedDiff+last3Diff+last5Diff+last7Diff+last10Diff+AOdd1, data = modeldf)
  rfpred<-as.vector(predict(rfmod, newdata = predAct))
  rfpred=ifelse(rfpred>0.5,1,0)
  set.seed(1212)
  nnmod  <- neuralnet(AWin~TeamDiff+PlayerDiff+SchedDiff+last3Diff+last5Diff+last7Diff+last10Diff+AOdd1, data = modeldf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  nnpred<-(compute(nnmod,predAct))$net.result
  nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
  set.seed(1212)
  gbmmod <- gbm(AWin~TeamDiff+PlayerDiff+SchedDiff+last3Diff+last5Diff+last7Diff+last10Diff+AOdd1, data = modeldf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 10000) 
  gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, predAct, type = "response")
  gbmpred=ifelse(gbmpred>0.5,1,0)
  
  predAct <- predAct %>%
    mutate(linpred = linpred) %>%
    mutate(predPD = linpred1) %>%
    mutate(logpred = logpred) %>%
    mutate(svmpred = svmpred) %>%
    mutate(nnpred = nnpred) %>%
    mutate(rfpred = rfpred)%>%
    mutate(gbmpred = gbmpred)
  return(predAct)
}
predictNBABets <- function(x){
  set.seed(1212)
  preddf1 <- read.csv("~/FloStrength/NBAFloStrength/NBAModel") %>%
    mutate(AWin = 1) %>%
    mutate(AOdd1 = ifelse(AOdd>0,(1/((AOdd/91)+1)), ((-1*(AOdd/110))/(-1*(AOdd/110)+1))))
  ind <- which(preddf1$WTeam == preddf1$HomeTeam)
  preddf1$AWin[ind] = 0
  
  colmod <- c(28:35,48)
  preddf<- preddf1[,colmod]
  colmod <- c(28:35,44)
  preddflin<- preddf1[,colmod]
  set.seed(1212)
  logmod <- glm(AWin~., data = preddf, family = binomial)
  set.seed(1212)
  svmmod = svm(formula = AWin~.,data = preddf,probability=TRUE)
  set.seed(1212)
  rfmod<-randomForest(AWin~., data = preddf)
  set.seed(1212)
  gbmmod <- gbm(AWin~., data = preddf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 100) 
  set.seed(1212)
  nnmod<-neuralnet(AWin~.,data=preddf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  linmod <- lm(PD~., data = preddflin)
  prednba <- read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/yestmodpred")%>%
    mutate(AOdd1 = ifelse(AOdd>0,(1/((AOdd/91)+1)), ((-1*(AOdd/110))/(-1*(AOdd/110)+1))))
  lin <- predict(linmod, newdata = prednba)
  log<- predict(logmod, newdata = prednba, type = "response")
  svm <- as.vector(predict(svmmod, newdata = prednba,probability=TRUE))
  rf<-as.vector(predict(rfmod, newdata = prednba))
  gbm <- predict(gbmmod, n.trees = gbmmod$n.trees, prednba, type = "response")
  nn <-(compute(nnmod,prednba))$net.result
  
  predweeknew <- prednba %>%
    mutate(logodds = round(log,4)) %>%
    mutate(svmodds = round(svm,4)) %>%
    mutate(rfodds = round(rf,4)) %>%
    mutate(gbmodds = round(gbm,4)) %>%
    mutate(nnodds = round(nn,4)) %>%
    mutate(log = ifelse(logodds>.5,1,0)) %>%
    mutate(svm = ifelse(svmodds>.5,1,0)) %>%
    mutate(rf = ifelse(rfodds>.5,1,0)) %>%
    mutate(gbm = ifelse(gbmodds>.5,1,0)) %>%
    mutate(nn = ifelse(nnodds>.5,1,0)) %>%
    mutate(ENSODDS = (nn+gbm+log + svm+rf)/5) %>%
    mutate(predwin = ifelse(ENSODDS>.5, AwayTeam, HomeTeam)) %>%
    mutate(predpd = round(lin,4)) %>%
    select(AwayTeam, HomeTeam, ENSODDS,logodds,svmodds, rfodds, gbmodds, nnodds, predwin, BookWin,AOdd, HOdd,predpd,Spread,gameID) %>%
    mutate(Game = str_c(AwayTeam, HomeTeam, sep  = " @ ")) %>%
    select(Game, ENSODDS,AOdd, HOdd, predwin, BookWin, Spread, predpd,gameID)
  straightbets <- predweeknew %>%
    filter(predwin != BookWin) %>%
    mutate(Earnings = ifelse(AOdd>HOdd, AOdd/100, HOdd/100)) %>%
    mutate(Earnings = ifelse(Earnings<0, -1*Earnings, Earnings)) %>%
    mutate(Bet = predwin) %>%
    select(Game, BookWin, Spread,Bet, Earnings,predwin,ENSODDS,gameID) %>%
    mutate(ENSODDS = ifelse(ENSODDS<.5, 1- ENSODDS, ENSODDS)) %>%
    arrange(-ENSODDS)
  straightbets2 <- predweeknew %>%
    filter(predwin == BookWin & abs(Spread) < 2) %>%
    mutate(Earnings = .877) %>%
    mutate(Bet = predwin) %>%
    select(Game, BookWin, Spread,Bet, Earnings,predwin,ENSODDS,gameID) %>%
    mutate(ENSODDS = ifelse(ENSODDS<.5, 1- ENSODDS, ENSODDS)) %>%
    arrange(-ENSODDS)
  spreadbets <- predweeknew %>%
    filter(predwin == BookWin & abs(Spread) >= 2) %>%
    mutate(spdiff = (abs(Spread)-abs(predpd))) %>%
    filter(abs(spdiff) > 3) %>%
    mutate(predpd = abs(predpd)) %>%
    mutate(Bet = ifelse(spdiff>0,abs(Spread), -1*abs(Spread)))%>%
    mutate(Earnings = 0.91) %>%
    select(Game, BookWin, Spread,Bet, Earnings,predwin,ENSODDS,gameID)
  betdf <- rbind(straightbets,straightbets2,spreadbets) %>%
    mutate(Bet = as.character(Bet))%>%
    mutate(predwin = as.character(predwin))%>%
    mutate(ENSODDS = as.numeric(ENSODDS))
  write_csv(betdf, "/Users/seanfloersch/FloStrength/NBAFloStrength/nbayestbetdf")
  betdf <- betdf %>%
    select(-gameID)
}
#model success
getNBAModSuc<- function(x){
  NBAModel <- read.csv("/Users/seanfloersch/FloStrength/NBAFloStrength/NBAModel") %>%
    mutate(gameID = str_remove(gameID, "[A-Z]{1,6}"))%>%
    mutate(AOdd1 = ifelse(AOdd>0,(1/((AOdd/91)+1)), ((-1*(AOdd/110))/(-1*(AOdd/110)+1))))
  nbaplot <- NBAModel %>%
    mutate(day = str_remove(gameID, "[A-Z]{1,6}")) %>%
    mutate(daynum = NA) %>% 
    mutate(weeknum = NA)
  nbaplot$BookWin <- ifelse(nbaplot$BookWin == nbaplot$AwayTeam, 1, 0)
  for (i in c(1:(length(unique(nbaplot$day))+1))) {
    i = i
    x <- unique(nbaplot$day)[i]
    ind <- which(nbaplot$day == x)
    nbaplot$daynum[ind] <- length(unique(nbaplot$day)) -i
  }
  
  traindf <- nbaplot %>% 
    filter(daynum < 7)
  colmod <- c(28:35,48,49)
  traindf<- traindf[,colmod]
  
  testdf <- nbaplot %>% 
    filter(daynum == 7)
  bookpred <- testdf$BookWin  
  actwin <- testdf$AWin
  games <- testdf$gameID
  daynum <- testdf$daynum
  testdf <- nbaplot %>% 
    filter(daynum == 7)
  testdf<- testdf[,colmod]
  set.seed(1212)
  logmod <- glm(AWin~., data = traindf, family = binomial)
  set.seed(1212)
  svmmod = svm(AWin~., data = traindf, type = 'C-classification', kernel = 'linear')
  svmpred <- as.vector(predict(svmmod, newdata = testdf, type = "response"))
  set.seed(1212)
  rfmod <- randomForest(AWin~., data = traindf)
  set.seed(1212)
  nnmod  <- neuralnet(AWin~., data = traindf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  logpred <- predict(logmod, newdata = testdf, type = "response")
  logpred <- ifelse(logpred>.5, 1,0)
  rfpred<-as.vector(predict(rfmod, newdata = testdf))
  rfpred=ifelse(rfpred>0.5,1,0)
  nnpred<-(compute(nnmod,testdf))$net.result
  nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
  set.seed(1212)
  gbmmod <- gbm(AWin~., data = traindf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 100) 
  gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, testdf, type = "response")
  gbmpred=ifelse(gbmpred>0.5,1,0)
  df <- data.frame(games,logpred, svmpred, rfpred, nnpred, gbmpred,bookpred, actwin,daynum) 
  compdf <- df
  
  for (i in c(8:(length(unique(nbaplot$daynum))-1))){
    traindf <- nbaplot %>% 
      filter(daynum < i)
    colmod <- c(28:35,48,49)
    traindf<- traindf[,colmod]
    testdf <- nbaplot %>% 
      filter(daynum == i)
    bookpred <- testdf$BookWin  
    actwin <- testdf$AWin
    games <- testdf$gameID
    daynum <- testdf$daynum
    testdf <- nbaplot %>% 
      filter(daynum == i)
    testdf<- testdf[,colmod]
    
    set.seed(1212)
    logmod <- glm(AWin~., data = traindf, family = binomial)
    set.seed(1212)
    svmmod = svm(AWin~., data = traindf, type = 'C-classification', kernel = 'linear')
    svmpred <- as.vector(predict(svmmod, newdata = testdf, type = "response"))
    set.seed(1212)
    rfmod <- randomForest(AWin~., data = traindf)
    set.seed(1212)
    nnmod  <- neuralnet(AWin~., data = traindf, hidden=1,act.fct = "logistic",linear.output = FALSE)
    logpred <- predict(logmod, newdata = testdf, type = "response")
    logpred <- ifelse(logpred>.5, 1,0)
    rfpred<-as.vector(predict(rfmod, newdata = testdf))
    rfpred=ifelse(rfpred>0.5,1,0)
    nnpred<-(compute(nnmod,testdf))$net.result
    nnpred <- as.vector(ifelse(nnpred>0.5, 1, 0))
    set.seed(1212)
    gbmmod <- gbm(AWin~., data = traindf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 100) 
    gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, testdf, type = "response")
    gbmpred=ifelse(gbmpred>0.5,1,0)
    df <- data.frame(games,logpred, svmpred, rfpred, nnpred, gbmpred,bookpred, actwin,daynum) 
    compdf <- rbind(compdf, df) 
  }
  compdf <- compdf %>%
    mutate(svmpred = as.numeric(svmpred)) %>%
    mutate(LogCorr = ifelse(logpred==actwin,1,0))%>%
    mutate(SVMCorr = ifelse(svmpred==actwin,1,0))%>%
    mutate(RFCorr = ifelse(rfpred==actwin,1,0))%>%
    mutate(NNCorr = ifelse(nnpred==actwin,1,0))%>%
    mutate(GBMCorr = ifelse(gbmpred==actwin,1,0)) %>%
    mutate(Ensemble = (logpred+svmpred+rfpred))%>%
    mutate(Ensemble2 = (nnpred+gbmpred+logpred+svmpred+rfpred))%>%
    mutate(Ensemble = ifelse(Ensemble>1.5,1,0)) %>%
    mutate(Ensemble2 = ifelse(Ensemble2>2.5,1,0)) %>%
    mutate(E1Corr = ifelse(Ensemble==actwin,1,0)) %>%
    mutate(E2Corr = ifelse(Ensemble2==actwin,1,0)) %>%
    mutate(BookCorr = ifelse(bookpred==actwin,1,0)) 
  ovrcorr <- compdf %>%
    filter(daynum == 7) %>%
    group_by(daynum) %>%
    mutate(LogCorr = mean(LogCorr))%>%
    mutate(SVMCorr = mean(SVMCorr))%>%
    mutate(RFCorr = mean(RFCorr))%>%
    mutate(NNCorr = mean(NNCorr))%>%
    mutate(GBMCorr = mean(GBMCorr))%>%
    mutate(E1Corr = mean(E1Corr))%>%
    mutate(E2Corr = mean(E2Corr))%>%
    mutate(BookCorrect = mean(BookCorr)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Day = daynum - 6) %>%
    select(Day, LogCorr, SVMCorr, RFCorr,NNCorr, GBMCorr, E1Corr,E2Corr, BookCorrect)
  last <- max(compdf$daynum)
  for (i in c(8:last)){
    ovrcorr1 <- compdf %>%
      filter(daynum <= i& daynum>7) %>%
      mutate(LogCorr = mean(LogCorr))%>%
      mutate(SVMCorr = mean(SVMCorr))%>%
      mutate(RFCorr = mean(RFCorr))%>%
      mutate(NNCorr = mean(NNCorr))%>%
      mutate(GBMCorr = mean(GBMCorr))%>%
      mutate(E1Corr = mean(E1Corr))%>%
      mutate(E2Corr = mean(E2Corr))%>%
      mutate(BookCorrect = mean(BookCorr)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(Day = i - 6) %>%
      select(Day, LogCorr, SVMCorr, RFCorr,NNCorr, GBMCorr, E1Corr,E2Corr, BookCorrect)
    
    ovrcorr <- rbind(ovrcorr, ovrcorr1)
  }
  ovr1 <- ovrcorr %>%
    select(Day,"Corr"=LogCorr) %>%
    mutate(Model = "Log")
  ovr2 <- ovrcorr %>%
    select(Day,"Corr"=RFCorr) %>%
    mutate(Model = "RF")
  ovr3 <- ovrcorr %>%
    select(Day,"Corr"=NNCorr) %>%
    mutate(Model = "NN")
  ovr4 <- ovrcorr %>%
    select(Day,"Corr"=GBMCorr) %>%
    mutate(Model = "GBM")
  ovr5 <- ovrcorr %>%
    select(Day,"Corr"=E1Corr) %>%
    mutate(Model = "E3")
  ovr6 <- ovrcorr %>%
    select(Day,"Corr"=SVMCorr) %>%
    mutate(Model = "SVM")
  ovr7 <- ovrcorr %>%
    select(Day,"Corr"=E2Corr) %>%
    mutate(Model = "Ens")
  ovr8 <- ovrcorr %>%
    select(Day,"Corr"=BookCorrect) %>%
    mutate(Model = "Book")
  ovrcorr<- rbind(ovr1,ovr2,ovr3,ovr4,ovr5,ovr6,ovr7,ovr8)
  write_csv(ovrcorr,"/Users/seanfloersch/FloStrength/FloStrengthApp/NBAModOvrSuc")
  dfcomp<- compdf %>%
    mutate(svm = svmpred) %>%
    mutate(E2 = Ensemble) %>%
    mutate(log = logpred) %>%
    mutate(rf = rfpred) %>%
    mutate(nn = nnpred) %>%
    mutate(gbm = gbmpred) %>%
    mutate(Ensemble = Ensemble2) %>%
    select(bookpred,svm, log,rf,nn,gbm,actwin,E2,Ensemble)
  svm<- dfcomp %>%
    mutate(Specificity = length(which(svm==actwin&svm==0))/length(which(svm==0)))%>%
    mutate(Sensitivity = length(which(svm==actwin&svm==1))/length(which(svm==1))) %>%
    mutate(Accuracy = length(which(svm==actwin))/length(svm)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(svm)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "SVM")
  gbm<- dfcomp %>%
    mutate(Specificity = length(which(gbm==actwin&gbm==0))/length(which(gbm==0)))%>%
    mutate(Sensitivity = length(which(gbm==actwin&gbm==1))/length(which(gbm==1))) %>%
    mutate(Accuracy = length(which(gbm==actwin))/length(gbm)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(gbm)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "GBM")
  nn<- dfcomp %>%
    mutate(Specificity = length(which(nn==actwin&nn==0))/length(which(nn==0)))%>%
    mutate(Sensitivity = length(which(nn==actwin&nn==1))/length(which(nn==1))) %>%
    mutate(Accuracy = length(which(nn==actwin))/length(nn)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(nn)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "NN")
  log<- dfcomp %>%
    mutate(Specificity = length(which(log==actwin&nn==0))/length(which(log==0)))%>%
    mutate(Sensitivity = length(which(log==actwin&log==1))/length(which(log==1))) %>%
    mutate(Accuracy = length(which(log==actwin))/length(log)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(log)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "Log")
  rf<- dfcomp %>%
    mutate(Specificity = length(which(rf==actwin&rf==0))/length(which(rf==0)))%>%
    mutate(Sensitivity = length(which(rf==actwin&rf==1))/length(which(rf==1))) %>%
    mutate(Accuracy = length(which(rf==actwin))/length(rf)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(rf)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "RF")
  e3<- dfcomp %>%
    mutate(Specificity = length(which(E2==actwin&E2==0))/length(which(E2==0)))%>%
    mutate(Sensitivity = length(which(E2==actwin&E2==1))/length(which(E2==1))) %>%
    mutate(Accuracy = length(which(E2==actwin))/length(E2)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(E2)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "E3")
  ens<- dfcomp %>%
    mutate(Specificity = length(which(Ensemble==actwin&Ensemble==0))/length(which(Ensemble==0)))%>%
    mutate(Sensitivity = length(which(Ensemble==actwin&Ensemble==1))/length(which(Ensemble==1))) %>%
    mutate(Accuracy = length(which(Ensemble==actwin))/length(Ensemble)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(Ensemble)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "Ensemble")
  book<- dfcomp %>%
    mutate(Specificity = length(which(bookpred==actwin&bookpred==0))/length(which(bookpred==0)))%>%
    mutate(Sensitivity = length(which(bookpred==actwin&bookpred==1))/length(which(bookpred==1))) %>%
    mutate(Accuracy = length(which(bookpred==actwin))/length(bookpred)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(bookpred)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "Book")
  dfcomp<- rbind(log,svm,rf,nn,gbm,e3,ens,book)%>%
    mutate(Specificity = round(Specificity,3))%>%
    mutate(Sensitivity = round(Sensitivity,3))%>%
    mutate(Accuracy = round(Accuracy,3))%>%
    mutate(F1Score = round(F1Score,3))
  names<-dfcomp$Model
  dfcomp<-data.frame(t(dfcomp))
  colnames(dfcomp)<- names
  dfcomp <-dfcomp[1:5,]
  write_csv(dfcomp,"/Users/seanfloersch/FloStrength/FloStrengthApp/NBAModSucDF")
}

 