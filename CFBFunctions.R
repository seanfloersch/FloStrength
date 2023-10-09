getNCAAFTeam <- function(x){
  getDefense <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/22"))
    Defense <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    dfdef <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:9)) {
        dfdef[i,j]<- Defense[j + (i -1)* 9]
      }
    }
    for (i in c(2:3)){
      h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/22/p",i))
      Defense <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:9)) {
          df1[j,k]<- Defense[k + (j -1)* 9]
        }
      }
      dfdef <- rbind(dfdef, df1)
    }
    colnames(dfdef)<- c("RankD", "Team", "GamesD", "PlaysD", "OppYds", "YPPD", "OTD","DTD", "DYPG")
    dfdef <- dfdef %>%
      select(Team,"GP"="GamesD",OppYds)
    return(dfdef)
  }
  dfdef <- getDefense(x)
  getOffense <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/21"))
    data <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:8)) {
        df[i,j]<- data[j + (i -1)* 8]
      }
    }
    for (i in c(2:3)){
      h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/21/p",i))
      data <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:8)) {
          df1[j,k]<- data[k + (j -1)* 8]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Games", "Plays", "Yds", "YPP", "OTD", "YPG")
    df <- df %>%
      select(Team,Yds)
    return(df)
  }
  dfoff <- getOffense(x)
  getTurnovers <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/29"))
    data <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:11)) {
        df[i,j]<- data[j + (i -1)* 11]
      }
    }
    for (i in c(2:3)){
      h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/29/p",i))
      data <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:11)) {
          df1[j,k]<- data[k + (j -1)*11]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Games", "FumRec", "OppINT", "TotTOD", "FumLost", "INT", "TotTO","TOMargin", "AvgToDiff")
    df <- df %>%
      select(Team,FumLost,"Int"=INT,"OppFum"=FumRec,"OppInt"=OppINT)
    return(df)
  }
  dfTO<-getTurnovers(x)
  getPen <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/876"))
    data <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:5)) {
        df[i,j]<- data[j + (i -1)* 5]
      }
    }
    for (i in c(2:3)){
      h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/876/p",i))
      data <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:5)) {
          df1[j,k]<- data[k + (j -1)*5]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Games", "Penalties", "PenYards")
    df <- df %>%
      select(Team, Penalties)
    return(df)
  }
  dfPen <- getPen(x)
  getRush <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/23"))
    data <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:8)) {
        df[i,j]<- data[j + (i -1)* 8]
      }
    }
    for (i in c(2:3)){
      h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/23/p",i))
      data <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:8)) {
          df1[j,k]<- data[k + (j -1)*8]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Games", "RushAtt", "RushYds","YPRush", "RushTD", "RushYPG")
    df <- df %>%
      select(Team,RushYds,RushTD,RushAtt)
    return(df)
  }
  dfRushO<-getRush(x)
  getRushDef <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/24"))
    data <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:8)) {
        df[i,j]<- data[j + (i -1)* 8]
      }
    }
    for (i in c(2:3)){
      h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/24/p",i))
      data <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:8)) {
          df1[j,k]<- data[k + (j -1)*8]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Games", "OppRushAtt", "OppRushYds","OppYPRush", "OppRushTD", "OppRushYPG")
    df <- df %>%
      select(Team,OppRushYds,OppRushTD,OppRushAtt)
    return(df)
  }
  dfRushD<-getRushDef(x)
  getPass <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/25"))
    data <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:11)) {
        df[i,j]<- data[j + (i -1)* 11]
      }
    }
    for (i in c(2:3)){
      h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/25/p",i))
      data <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:11)) {
          df1[j,k]<- data[k + (j -1)*11]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Games", "PassAtt", "PassComp","PassInt", "PassYds", "PYPA", "PYPC", "PassTD", "YPG")
    df <- df %>%
      select(Team,PassComp,PassYds,PassTD,PassInt,PassAtt)
    return(df)
  }
  dfPassO<-getPass(x)
  getPassD <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/40"))
    data <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:9)) {
        df[i,j]<- data[j + (i -1)* 9]
      }
    }
    for (i in c(2:3)){
      h <- read_html(str_c("https://www.ncaa.com/stats/football/fbs/current/team/40/p",i))
      data <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:9)) {
          df1[j,k]<- data[k + (j -1)*9]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Games", "OppPassAtt", "OppPassComp","OppPassInt", "OppPassYds", "OppPassTD", "DPassEff")
    df <- df %>%
      select(Team,OppPassComp,OppPassYds,OppPassTD,OppPassInt,OppPassAtt)
    return(df)
  }
  dfPassD<-getPassD(x)
  
  dfcomp<- left_join(dfoff, dfdef, by = "Team")
  dfcomp<- left_join(dfcomp, dfPen, by = "Team")
  dfcomp<- left_join(dfcomp, dfTO, by = "Team")
  dfcomp<- left_join(dfcomp, dfRushO, by = "Team")
  dfcomp<- left_join(dfcomp, dfRushD, by = "Team")
  dfcomp<- left_join(dfcomp, dfPassO, by = "Team")
  dfcomp<- left_join(dfcomp, dfPassD, by = "Team")
  for (i in c(2:25)){
    dfcomp[i] <- as.numeric(unlist(dfcomp[i]))
  }
  dfcomp1 <- dfcomp %>%
    mutate(yearID = 2023) %>%
    mutate(Passing= (PassComp*PassYds*(PassTD-(PassInt*1.21)))/PassAtt) %>%
    mutate(Rushing = (RushYds*(2.51*RushTD-FumLost))/RushAtt)%>%
    mutate(OppPass = (OppPassComp*OppPassYds*(OppPassTD-(OppPassInt*1.21)))/OppPassAtt)%>%
    mutate(OppRush = (OppRushYds*(2.51*OppRushTD-1.21*OppFum))/OppRushAtt) %>%
    mutate(Pen=Penalties/GP)%>%
    group_by(yearID) %>%
    mutate(Pen = -1*(Pen - mean(Pen))/ sd(Pen))%>%
    mutate(Pen = ifelse(is.na(Pen)==TRUE,0,Pen))%>%
    mutate(Passing = (Passing - mean(Passing))/ sd(Passing))%>%
    mutate(Rushing = (Rushing - mean(Rushing))/ sd(Rushing))%>%
    mutate(OppPass = -1*(OppPass - mean(OppPass))/ sd(OppPass))%>%
    mutate(OppRush = -1*(OppRush - mean(OppRush))/ sd(OppRush))%>%
    ungroup() %>%
    mutate(Offense=(Passing+Rushing)/2)%>%
    mutate(Defense=(OppPass+OppRush)/2)%>%
    mutate(FloTeam = .5 + .05454 * Rushing + .09056*Passing+.05137*OppPass+.06272*OppRush+.01205*Pen)
  getStandings <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/standings/football/fbs/all-conf"))
    conferences <- html_nodes(h, ".standings-conference") %>% html_text
    Team<- html_nodes(h, ".standings-team") %>% html_text
    W<- html_nodes(h, "td:nth-child(4)") %>% html_text %>% as.numeric()
    L<- html_nodes(h, "td:nth-child(5)") %>% html_text%>% as.numeric()
    PF<- html_nodes(h, "td:nth-child(6)") %>% html_text%>% as.numeric()
    PA<- html_nodes(h, "td:nth-child(7)") %>% html_text%>% as.numeric()
    df<- data.frame(Team, W, L,PF,PA) %>%
      mutate(Conference = NA) %>%
      mutate(Wpct = W/ (W+L))
    df$Conference[1:14]<- "American"
    df$Conference[15:28]<- "ACC"
    df$Conference[29:42]<- "Big 12"
    df$Conference[43:56]<- "Big 10"
    df$Conference[57:65]<- "CUSA"
    df$Conference[66:69]<- "Ind FBS"
    df$Conference[70:81]<- "MAC"
    df$Conference[82:93]<- "MWC"
    df$Conference[94:105]<- "Pac-12"
    df$Conference[106:119]<- "SEC"
    df$Conference[120:133]<- "S Belt"
    h <- read_html(str_c("https://www.colleyrankings.com/curconf.html"))
    Conference <- html_nodes(h, "th a") %>% html_text
    ConfRating<- html_nodes(h, "th~ td:nth-child(3)") %>% html_text %>% as.numeric()
    confdf<- data.frame(Conference,ConfRating)
    df <- left_join(df, confdf, by = "Conference")%>%
      mutate(ConfRating=ifelse(Team=="Notre Dame",.6,ConfRating))
    return(df)
  }
  standings <- getStandings(x)
  dfcomp1<- right_join(dfcomp1, standings, by = "Team")
  dfcomp1 <- dfcomp1 %>%
    mutate(TeamRating = (3+log(FloTeam*ConfRating))/3) %>%
    na.omit()
  return(dfcomp1)
}
getSOS <- function(masterdf,weeknum){
  h <- read_html(str_c("https://www.ncaa.com/scoreboard/football/fbs/2023/","01","/all-conf"))
  away <- html_nodes(h, "#scoreboardGames li:nth-child(1) .gamePod-game-team-name") %>% html_text
  home <- html_nodes(h, "#scoreboardGames li:nth-child(2) .gamePod-game-team-name") %>% html_text
  awayscore <- html_nodes(h, "#scoreboardGames li:nth-child(1) .gamePod-game-team-score") %>% html_text %>% as.numeric()
  homescore <- html_nodes(h, "#scoreboardGames li:nth-child(2) .gamePod-game-team-score") %>% html_text%>% as.numeric()
  sosdf <- data.frame(away,home,awayscore,homescore) %>%
    mutate(PD = abs(awayscore - homescore))
  for (i in c(2:weeknum)){
    if (i < 10){
      i <- str_c("0",i)
    }
    h <- read_html(str_c("https://www.ncaa.com/scoreboard/football/fbs/2023/",i,"/all-conf"))
    away <- html_nodes(h, "#scoreboardGames li:nth-child(1) .gamePod-game-team-name") %>% html_text
    home <- html_nodes(h, "#scoreboardGames li:nth-child(2) .gamePod-game-team-name") %>% html_text
    awayscore <- html_nodes(h, "#scoreboardGames li:nth-child(1) .gamePod-game-team-score") %>% html_text %>% as.numeric()
    homescore <- html_nodes(h, "#scoreboardGames li:nth-child(2) .gamePod-game-team-score") %>% html_text%>% as.numeric()
    sosdf1 <- data.frame(away,home,awayscore,homescore) %>%
      mutate(PD = abs(awayscore - homescore))
    sosdf <- rbind(sosdf, sosdf1)
  }
  awaydf <- masterdf %>%
    select("away" = "Team", "awayFS" = "TeamRating")
  homedf <- masterdf %>%
    select("home" = "Team", "homeFS" = "TeamRating")
  sosdf <- left_join(sosdf, awaydf, by = "away")
  sosdf <- left_join(sosdf, homedf, by = "home") %>%
    mutate(awaysched = NA) %>%
    mutate(homesched = NA)
  ind <- which(is.na(sosdf$awayFS)==TRUE)
  sosdf$awayFS[ind] = .1
  ind <- which(is.na(sosdf$homeFS)==TRUE)
  sosdf$homeFS[ind] = .1
  ind <- which(sosdf$awayscore < sosdf$homescore)
  sosdf$awaysched[ind] = -1 * sosdf$PD[ind]*(1-sosdf$homeFS[ind])
  sosdf$homesched[ind] = sosdf$PD[ind]*(sosdf$awayFS[ind])
  ind <- which(sosdf$awayscore > sosdf$homescore)
  sosdf$homesched[ind] = -1 * sosdf$PD[ind]*(1-sosdf$awayFS[ind])
  sosdf$awaysched[ind] = sosdf$PD[ind]*(sosdf$homeFS[ind])
  sosdf <- sosdf %>%
    na.omit()
  awayscheddf <- sosdf %>%
    group_by(away) %>%
    mutate(AFS = sum(awaysched))%>%
    mutate(Agp = length(awayFS)) %>%
    slice(1) %>%
    ungroup %>%
    select("Team" =away, AFS, Agp)
  homescheddf <- sosdf %>%
    group_by(home) %>%
    mutate(HFS = sum(homesched))%>%
    mutate(Hgp = length(homeFS)) %>%
    slice(1) %>%
    ungroup %>%
    select("Team" =home, HFS, Hgp)
  scheddf <- full_join(awayscheddf, homescheddf, by = "Team") %>%
    mutate(AFS=ifelse(is.na(AFS)==TRUE,0,AFS))%>%
    mutate(Agp=ifelse(is.na(Agp)==TRUE,0,Agp))%>%
    na.omit %>%
    mutate(SchedRating = (log(15+((AFS + HFS)/ (Agp+Hgp)))-1.5)/2) %>%
    select(Team, SchedRating)
  return(scheddf)
}
