################GET#####################



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
getGameID <- function(x){
  x <- Sys.Date()
  x <- str_remove_all(x, "-")
  year <- x %>% str_extract("\\d{4}") %>% str_extract("\\d{2}$")
  month <-x %>% str_extract("\\d{6}") %>% str_extract("\\d{2}$")
  day <-x %>% str_extract("\\d{2}$")
  x <- str_c(month, day, year)
  return(x)
}
getyestGameID <- function(x){
  x <- Sys.Date() -1
  x <- str_remove_all(x, "-")
  year <- x %>% str_extract("\\d{4}") %>% str_extract("\\d{2}$")
  month <-x %>% str_extract("\\d{6}") %>% str_extract("\\d{2}$")
  day <-x %>% str_extract("\\d{2}$")
  x <- str_c(month, day, year)
  return(x)
}
getMLBHitters<- function(x){
  teams <- c("HOU", "TOR", "LAA", "BOS", "WSH", "CHW", "CIN", "SD", "KC", "COL", "SF", "MIN", "LAD", "ATL", "PHI", "DET", "PIT", "BAL", "NYM", "NYY", "MIA", "ARI", "OAK", "TB", "STL", "CLE", "CHC", "TEX", "MIL", "SEA")
  urls <- str_c("https://www.espn.com/mlb/team/stats/_/name/",teams,"/season/2023/seasontype/2", sep="")
  hitterfun<- function(url){
    url<- ifelse(url=="https://www.espn.com/mlb/team/stats/_/name/COL","https://www.espn.com/mlb/team/stats/_/name/COL/colorado-rockies",url)
    h <- read_html(url) 
    team <- str_extract(url, "name/[A-Z]{1,4}") %>% str_extract("/[A-Z]{1,3}")%>% str_extract("[A-Z]{1,3}")
    data <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD") %>% html_text() %>% as.numeric
    players <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__TD .AnchorLink") %>% html_text() 
    numplay = length(players)
    df<- data.frame(Name =players)
    for (i in c(1:17)){
      for (j in c(1:numplay)){
        df[j,(i+1)]= data[i + (j -1)* 17]
      }
    }
    df <- df %>% mutate(Team = team)
    colnames(df)= c("Name","GP","AB","R","H","Doub","Trip","HR","RBI","TB","BB","SO","SB","AVG","OBP","SLG","OPS","WAR","Team")
    return(df)
  }
  dfhit<- map_df(.x= urls, .f=hitterfun)
  urls <- str_c("https://www.espn.com/mlb/team/stats/_/type/fielding/name/",teams,"/season/2023/seasontype/2", sep="")
  fieldingfun<- function(url){
    h <- read_html(url) 
    team <- str_extract(url, "name/[A-Z]{1,4}") %>% str_extract("/[A-Z]{1,3}")%>% str_extract("[A-Z]{1,3}")
    data <- html_nodes(h, ".remove_capitalize .Table__Scroller .Table__TD") %>% html_text() %>% as.numeric
    players <- html_nodes(h, ".Table__TD .AnchorLink") %>% html_text() 
    positions<-html_nodes(h,".font10")%>% html_text()
    numplay = length(players)
    df<- data.frame(Name =players, Pos = positions)
    for (i in c(1:11)){
      for (j in c(1:numplay)){
        df[j,(i+2)]= data[i + (j -1)* 11]
      }
    }
    df <- df %>% mutate(Team = team)
    colnames(df)= c("Name","Pos","GP","GS","FIP","TC","PO","A","FP","E","DP","RF","DWAR","Team")
    df<- df %>%
      mutate(Pos = str_remove_all(Pos," "))
    return(df)
  }
  dffield<- map_df(.x=urls,.f=fieldingfun)%>%
    select(Name,Pos, Team, PO,A,E,FIP,DWAR)%>%
    mutate(yearID = 2023) %>%
    mutate(InnOuts = round(FIP * 3)) %>%
    select(-FIP) %>%
    filter(Pos != "P"& InnOuts>=max(InnOuts)*.3) %>%
    mutate(Pos = ifelse(Pos %in% c("2B","SS","3B"), "IF", Pos)) %>%
    mutate(Pos = ifelse(Pos %in% c("LF","CF","RF"), "OF", Pos)) %>%
    mutate(Fielding = ifelse(Pos =="OF",((PO+3*A-4*E)/(InnOuts/27)),NA))%>%
    mutate(Fielding = ifelse(Pos =="IF"&is.na(Fielding)==TRUE,((PO+2*A-2*E)/(InnOuts/27)),Fielding))%>%
    mutate(Fielding = ifelse(Pos =="1B"&is.na(Fielding)==TRUE,((PO+3*A-3*E)/(InnOuts/27)),Fielding))%>%
    mutate(Fielding = ifelse(Pos =="C",((PO+15*(A)-3*E)/(InnOuts/27)),Fielding)) %>%
    mutate(Fielding=DWAR/InnOuts)%>%
    group_by(yearID, Pos) %>%
    mutate(Fielding = (Fielding-mean(Fielding))/sd(Fielding)) %>%
    ungroup() %>%
    group_by(Name, yearID,Team) %>%
    mutate(Fielding = sum(Fielding*(InnOuts)/sum(InnOuts))) %>%
    mutate(PO = sum(PO))%>%
    mutate(A = sum(A))%>%
    mutate(E = sum(E))%>%
    mutate(Fpct = (PO+A)/(PO+A+E)) %>%
    arrange(-InnOuts) %>%
    mutate(Innings = sum(InnOuts)/3) %>%
    slice(1)%>%
    ungroup() %>%
    filter(Pos != "SP"&Pos != "RP")%>%
    select(Name, Team,Pos, Fielding, Fpct,Innings) %>%
    mutate(FValue = Fielding * (Innings/2716))
  df<- left_join(dfhit,dffield, by = c("Name","Team"))%>%
    mutate(yearID = 2023) %>%
    mutate(Pos = ifelse(is.na(Pos)==TRUE,"DH",Pos))%>%
    mutate(Fielding = ifelse(is.na(Fielding)==TRUE,0,Fielding))%>%
    mutate(Fpct = ifelse(is.na(Fpct)==TRUE,0,Fpct))%>%
    mutate(Innings = ifelse(is.na(Innings)==TRUE,0,Innings))%>%
    mutate(FValue = ifelse(is.na(FValue)==TRUE,0,FValue))%>%
    mutate(PA= AB+BB)%>%
    filter(GP>10 &RBI>0)%>%
    filter(R>0)%>%
    group_by(Team,yearID) %>%
    mutate(RPG = R / PA) %>%
    mutate(RBIPG = RBI / PA) %>%
    mutate(normRun = (RPG -mean(RPG))/ sd(RPG)) %>%
    mutate(normRBI = (RBIPG -mean(RBIPG))/ sd(RBIPG)) %>%
    ungroup %>%
    group_by(Name)%>%
    arrange(GP)%>%
    mutate(rat=AB/sum(AB))%>%
    mutate(normRun=sum(rat*(normRun)))%>%
    mutate(normRBI=sum(rat*(normRBI)))%>%
    select(-rat)%>%
    mutate(GP=sum(GP))%>%
    mutate(AB=sum(AB))%>%
    mutate(H=sum(H))%>%
    mutate(Doub=sum(Doub))%>%
    mutate(Trip=sum(Trip))%>%
    mutate(HR=sum(HR))%>%
    mutate(R=sum(R))%>%
    mutate(RBI=sum(RBI))%>%
    mutate(TB=sum(TB))%>%
    mutate(BB=sum(BB))%>%
    mutate(SO=sum(SO))%>%
    mutate(SB=sum(SB))%>%
    mutate(PA= AB+BB)%>%
    mutate(Fielding=mean(Fielding))%>%
    mutate(FValue=sum(FValue))%>%
    slice(1)%>%
    ungroup()%>%
    mutate(Power =((4* HR+ 3 * Trip + 2 * Doub + (H - Doub - Trip - HR)-1.2*SO+BB)/(PA))) %>%
    mutate(PAPG = (PA)/GP) %>%
    filter(PAPG >2&GP>30)%>%
    group_by(yearID) %>%
    mutate(SBV = (SB)^.125) %>%
    mutate(SBV = ((SBV - mean(SBV))/sd(SBV))/2) %>%
    mutate(Power = (Power - mean(Power))/sd(Power)) %>%
    mutate(Other = normRun +normRBI +.3*SBV) %>%
    mutate(Other = (Other - mean(Other))/sd(Other)) %>%
    ungroup() %>%
    mutate(OffFS = (.354*Other +.646*Power)) %>%
    mutate(OffValue = OffFS * (PA/164))%>%
    mutate(FloValue= OffValue+FValue)%>%
    rename("DefFS"="Fielding")%>%
    mutate(Team=ifelse(Name=="Aaron Hicks","BAL",Team))
  write_csv(df,"/Users/seanfloersch/FloStrength/MLBFloStrength/MLBHitters23")
  return(df)
}
getMLBPitchers <- function(x){
  options(warn=-1)
  teams <- c("HOU", "TOR", "LAA", "BOS", "WSH", "CHW", "CIN", "SD", "KC", "COL", "SF", "MIN", "LAD", "ATL", "PHI", "DET", "PIT", "BAL", "NYM", "NYY", "MIA", "ARI", "OAK", "TB", "STL", "CLE", "CHC", "TEX", "MIL", "SEA")
  urls <- str_c("https://www.espn.com/mlb/team/stats/_/type/pitching/name/",teams,"/season/2023/seasontype/2", sep="")
  getpitchfun<- function(url){
    h <- read_html(url) 
    team <- str_extract(url, "name/[A-Z]{1,4}") %>% str_extract("/[A-Z]{1,3}")%>% str_extract("[A-Z]{1,3}")
    names <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__TD .AnchorLink") %>% html_text
    df<- data.frame(Name=names)%>%
      mutate(Team=team)
    data <- html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD") %>% html_text%>%as.numeric()
    numplay = length(names)
    for (i in c(1:18)){
      for (j in c(1:numplay)){
        df[j,(i+2)]= data[i + (j -1)* 18]
      }
    }
    colnames(df)=c("Name","Team", html_nodes(h, ".ResponsiveWrapper+ .remove_capitalize .clr-gray-01")%>% html_text())
    return(df)
  }
  df<- map_df(.x=urls,.f=getpitchfun)
  df1<-df%>%
    group_by(Name)%>%
    arrange(GP)%>%
    mutate(GP=sum(GP))%>%
    mutate(GS=sum(GS))%>%
    mutate(W=sum(W))%>%
    mutate(L=sum(L))%>%
    mutate(SV=sum(SV))%>%
    mutate(HLD=sum(HLD))%>%
    mutate(IP=sum(IP))%>%
    mutate(H=sum(H))%>%
    mutate(ER=sum(ER))%>%
    mutate(HR=sum(HR))%>%
    mutate(BB=sum(BB))%>%
    mutate(K=sum(K))%>%
    mutate(IPouts=(1/3)*as.numeric(str_sub(10*IP, start= -1)))%>%
    mutate(IP = round(round(IP)+IPouts,1))%>%
    mutate(Pos = ifelse(GS>.6*GP,"SP","RP"))%>%
    slice(1)%>%
    ungroup()%>%
    filter(IP>max(GP)*.6)%>%
    mutate(IPouts = round(IP*3))%>%
    mutate(yearID= 2023)%>%
    mutate(FloStrength =-1*(4*HR+1.5*(H-HR)-2*K+BB)/(IPouts+1)) %>%
    group_by(yearID,Pos) %>%
    mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength)) %>%
    ungroup() %>%
    mutate(FloValue = FloStrength * (IP/87))
  write_csv(df1,"/Users/seanfloersch/FloStrength/MLBFloStrength/MLBPitchers23")
  
  return(df1)
}
getMLBTeam <- function(Hitters,Pitchers,Games){
  getActiveHitters<-function(Hitters){
    teams <- c("HOU", "TOR", "LAA", "BOS", "WSH", "CHW", "CIN", "SD", "KC", "COL", "SF", "MIN", "LAD", "ATL", "PHI", "DET", "PIT", "BAL", "NYM", "NYY", "MIA", "ARI", "OAK", "TB", "STL", "CLE", "CHC", "TEX", "MIL", "SEA")
    get_roster_data <- function(team){
      url <- str_c("https://www.espn.com/mlb/team/roster/_/name/",team, sep="")
      h <- read_html(url) 
      Name <- html_nodes(h, ".Table__TD+ .Table__TD .AnchorLink") %>% html_text
      Pos <- html_nodes(h, ".Table__TD:nth-child(3) .inline") %>% html_text
      a <- data.frame(Name, Pos) %>%
        mutate(Team = team)%>%
        mutate(Pos= ifelse(Pos %in% c("2B","SS","3B"),"IF",Pos))%>%
        mutate(Pos= ifelse(Pos %in% c("LF","CF","RF"),"OF",Pos))
      return(a)
    }
    roster <- map_df(.x= teams, .f=get_roster_data)%>%
      rename("rPos"="Pos")
    injured<- str_c("https://www.espn.com/mlb/injuries")
    h <- read_html(injured) 
    Name <- html_nodes(h, ".Table__TD .AnchorLink") %>% html_text
    Status <- html_nodes(h, ".plain") %>% html_text
    rPos = html_nodes(h, ".col-pos") %>% html_text
    injured <- data.frame(Name, Status,rPos)
    actHit = left_join(roster, Hitters, by = c("Team","Name"))%>%
      filter(Pos != "SP"& Pos!="RP")
    actHit = left_join(actHit, injured, by = c("rPos","Name"))%>%
      filter(is.na(Status)==TRUE)%>%
      select(Pos,Name, Team,"FloStrength"=OffFS,GP,AB)%>%
      na.omit()%>%
      mutate(Ratio = AB/GP)%>%
      group_by(Team)%>%
      mutate(Ratio = Ratio/sum(Ratio))%>%
      mutate(Ratio = FloStrength * Ratio)%>%
      mutate(HitterScore= sum(Ratio))%>%
      slice(1)%>%
      ungroup()%>%
      select(Team, HitterScore)%>%
      mutate(HitterScore = (HitterScore-mean(HitterScore))/sd(HitterScore))
    return(actHit)
  }
  getActivePitchers<-function(Pitchers){
    teams <- c("HOU", "TOR", "LAA", "BOS", "WSH", "CHW", "CIN", "SD", "KC", "COL", "SF", "MIN", "LAD", "ATL", "PHI", "DET", "PIT", "BAL", "NYM", "NYY", "MIA", "ARI", "OAK", "TB", "STL", "CLE", "CHC", "TEX", "MIL", "SEA")
    get_roster_data <- function(team){
      url <- str_c("https://www.espn.com/mlb/team/roster/_/name/",team, sep="")
      h <- read_html(url) 
      Name <- html_nodes(h, ".Table__TD+ .Table__TD .AnchorLink") %>% html_text
      Pos <- html_nodes(h, ".Table__TD:nth-child(3) .inline") %>% html_text
      a <- data.frame(Name, Pos) %>%
        mutate(Team = team)%>%
        mutate(Pos= ifelse(Pos %in% c("RP","SP"),"P",Pos))
      return(a)
    }
    roster <- map_df(.x= teams, .f=get_roster_data)
    injured<- str_c("https://www.espn.com/mlb/injuries")
    h <- read_html(injured) 
    Name <- html_nodes(h, ".Table__TD .AnchorLink") %>% html_text
    Status <- html_nodes(h, ".plain") %>% html_text
    Pos = html_nodes(h, ".col-pos") %>% html_text
    injured <- data.frame(Name, Status,Pos)%>%
      mutate(Pos= ifelse(Pos %in% c("RP","SP"),"P",Pos))
    actPitch = left_join(roster, Pitchers, by = c("Team","Name"))%>%
      filter(Pos.x == "P")%>%
      mutate(Pos = Pos.x)
    actPitch = left_join(actPitch, injured, by = c("Pos","Name"))%>%
      filter(is.na(Status)==TRUE)%>%
      select("Pos"=Pos.y,Name, Team,FloStrength,IP,GP)%>%
      mutate(Ratio = IP/GP)%>%
      na.omit()%>%
      group_by(Team)%>%
      mutate(Ratio = Ratio/sum(Ratio))%>%
      mutate(Ratio = FloStrength * Ratio)%>%
      mutate(PitchScore= sum(Ratio))%>%
      slice(1)%>%
      ungroup()%>%
      select(Team, PitchScore)%>%
      mutate(PitchScore = (PitchScore-mean(PitchScore))/sd(PitchScore))
    return(actPitch)
  }
  Pitchers<- getActivePitchers(Pitchers)
  Hitters<- getActiveHitters(Hitters)
  getTeamStats<- function(x){
    #Teams <- c("Los Angeles Dodgers"="LAD", "Los Angeles Angels"="LAA", "Tampa Bay Rays" = "TB", "Baltimore Orioles" = "BAL", "Milwaukee Brewers" = "MIL", "San Francisco Giants" = "SF", "Miami Marlins" = "MIA", "San Diego Padres" = "SD", "Chicago White Sox" = "CHW", "New York Mets" = "NYM", "Oakland Athletics" = "OAK", "Houston Astros" = "HOU", "New York Yankees" = "NYY", "Toronto Blue Jays" = "TOR", "Atlanta Braves" = "ATL", "Chicago Cubs" = "CHC", "Seattle Mariners" = "SEA", "Texas Rangers" = "TEX", "Minnesota Twins" = "MIN", "Cleveland Guardians" = "CLE", "Kansas City Royals" = "KC", "Detroit Tigers" = "DET", "Boston Red Sox" = "BOS", "Colorado Rockies" = "COL", "Arizona Diamondbacks" = "ARI", "Cincinnati Reds" = "CIN", "St. Louis Cardinals" = "STL", "Pittsburgh Pirates" = "PIT", "Philadelphia Phillies" = "PHI", "Washington Nationals" = "WSH")
    Teams <- c("L.A. Dodgers"="LAD", "L.A. Angels"="LAA", "Tampa Bay" = "TB", "Baltimore" = "BAL", "Milwaukee" = "MIL", "San Francisco" = "SF", "Miami" = "MIA", "San Diego" = "SD", "Chi. White Sox" = "CHW", "N.Y. Mets" = "NYM", "Oakland" = "OAK", "Houston" = "HOU", "N.Y. Yankees" = "NYY", "Toronto" = "TOR", "Atlanta" = "ATL", "Chi. Cubs" = "CHC", "Seattle" = "SEA", "Texas" = "TEX", "Minnesota" = "MIN", "Cleveland" = "CLE", "Kansas City" = "KC", "Detroit" = "DET", "Boston" = "BOS", "Colorado" = "COL", "Arizona" = "ARI", "Cincinnati" = "CIN", "St. Louis" = "STL", "Pittsburgh" = "PIT", "Philadelphia" = "PHI", "Washington" = "WSH")
    h <- read_html("https://www.cbssports.com/mlb/stats/team/team/batting/mlb/regular/?sortdir=descending&sortcol=avg") 
    Team <- html_nodes(h, ".TeamName a") %>% html_text
    GP <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(2)') %>% html_text() %>% as.numeric
    AB <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(4)') %>% html_text() %>% str_remove(",") %>% as.numeric
    H <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(6)') %>% html_text() %>% str_remove(",") %>% as.numeric
    Doubles <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(7)') %>% html_text() %>% as.numeric
    Triples <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(8)') %>% html_text() %>% as.numeric
    Triples=ifelse(is.na(Triples)==TRUE,0,Triples)
    HR <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(9)') %>% html_text() %>% as.numeric
    RBI <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(10)') %>% html_text() %>% as.numeric
    BB <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(13)') %>% html_text() %>% as.numeric
    SO <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(14)') %>% html_text() %>% str_remove(",") %>% as.numeric
    SB <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(11)') %>% html_text() %>% as.numeric
    AVG <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(3)') %>% html_text() %>% as.numeric
    OBP <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(15)') %>% html_text() %>% as.numeric
    SLG <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(16)') %>% html_text() %>% as.numeric
    OPS <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(17)') %>% html_text() %>% as.numeric
    teamhit <- data.frame(Team, GP, AB, H, Doubles, Triples, HR, RBI, BB, SO, SB, AVG, OBP, SLG, OPS) 
    ### Team pitching
    h <- read_html("https://www.cbssports.com/mlb/stats/team/team/pitching/mlb/regular/") 
    Team <- html_nodes(h, ".TeamName a") %>% html_text
    GP <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(2)') %>% html_text() %>% as.numeric
    ERA <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(12)') %>% html_text() %>% as.numeric
    SV <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(6)') %>% html_text() %>% as.numeric
    IP <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(3)') %>% html_text() %>% str_remove(",") %>% as.numeric
    HA <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(8)') %>% html_text() %>% str_remove(",") %>% as.numeric
    ER <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(11)') %>% html_text() %>% as.numeric
    HRA <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(10)') %>% html_text() %>% as.numeric
    BBA <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(13)') %>% html_text() %>% as.numeric
    SOA <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(14)') %>% html_text() %>% str_remove(",") %>% as.numeric
    WHIP <- html_nodes(h, '.TableBase-bodyTd--number:nth-child(15)') %>% html_text() %>% as.numeric
    teampitch <- data.frame(Team, ERA, SV, IP, HA, ER, HRA, BBA, SOA, WHIP)
    
    h <- read_html("https://www.espn.com/mlb/standings/_/sort/gamesbehind/dir/asc/group/overall") 
    W <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(1)") %>% html_text %>% as.numeric
    L <- html_nodes(h, ".Table__TD:nth-child(2)") %>% html_text %>% as.numeric
    Team <- html_nodes(h, ".hide-mobile .AnchorLink") %>% html_text
    RS <- html_nodes(h, ".Table__TD:nth-child(7)") %>% html_text %>% as.numeric
    RA <- html_nodes(h, ".Table__TD:nth-child(8)") %>% html_text %>% as.numeric
    teamstand <- data.frame(Team, W, L, RS, RA)
    
    teamhit$Team = as.character(Teams[teamhit$Team])
    teampitch$Team = as.character(Teams[teampitch$Team])
    Teams <- c("Los Angeles Dodgers"="LAD", "Los Angeles Angels"="LAA", "Tampa Bay Rays" = "TB", "Baltimore Orioles" = "BAL", "Milwaukee Brewers" = "MIL", "San Francisco Giants" = "SF", "Miami Marlins" = "MIA", "San Diego Padres" = "SD", "Chicago White Sox" = "CHW", "New York Mets" = "NYM", "Oakland Athletics" = "OAK", "Houston Astros" = "HOU", "New York Yankees" = "NYY", "Toronto Blue Jays" = "TOR", "Atlanta Braves" = "ATL", "Chicago Cubs" = "CHC", "Seattle Mariners" = "SEA", "Texas Rangers" = "TEX", "Minnesota Twins" = "MIN", "Cleveland Guardians" = "CLE", "Kansas City Royals" = "KC", "Detroit Tigers" = "DET", "Boston Red Sox" = "BOS", "Colorado Rockies" = "COL", "Arizona Diamondbacks" = "ARI", "Cincinnati Reds" = "CIN", "St. Louis Cardinals" = "STL", "Pittsburgh Pirates" = "PIT", "Philadelphia Phillies" = "PHI", "Washington Nationals" = "WSH")
    
    teamstand$Team = as.character(Teams[teamstand$Team])
    
    teamdata1 <- merge(teampitch, teamhit, by = "Team")
    teamdata1 <- merge(teamstand, teamdata1, by = "Team")%>%mutate(WinPercent = W / (W + L))
    return(teamdata1)
  }
  TeamStats<-getTeamStats(x)
  TeamStats<-left_join(TeamStats,Pitchers, by="Team")
  TeamStats<-left_join(TeamStats,Hitters, by="Team")
  TeamStats<- TeamStats %>%
    mutate(Hitting = (4*HR+3*Triples+2*Doubles+(H-HR-Doubles-Triples)-2*SO+BB+SB)/(GP*9)) %>%
    mutate(Pitching = -1*(4*HRA+1.5*(HA-HRA)-2*SOA+BBA)/(GP*9)) %>%
    mutate(Hitting = (Hitting-mean(Hitting))/sd(Hitting))%>%
    mutate(Pitching = (Pitching-mean(Pitching))/sd(Pitching))%>%
    mutate(FloStrength= 10*Hitting+10*Pitching+4.469 * PitchScore+14.625*HitterScore)%>%
    mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
    mutate(FloStrength = (5+FloStrength)/10)
  
  AllMLBGames<- read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/MLBGames2023")
  afs=TeamStats %>%
    select("ATeam"=Team,"afs"="FloStrength")
  hfs=TeamStats %>%
    select("HTeam"=Team,"hfs"="FloStrength")
  AllMLBGames<- left_join(AllMLBGames,afs, by = "ATeam")
  AllMLBGames<- left_join(AllMLBGames,hfs, by = "HTeam")%>%
    mutate(PD = abs(AScore-HScore))%>%
    mutate(AFSI= ifelse(AScore-HScore>0,PD*hfs,-1*PD*(1-hfs)))%>%
    mutate(HFSI= ifelse(AScore-HScore>0,-1*PD*(1-afs),PD*afs))
  afs<- AllMLBGames%>%
    group_by(ATeam)%>%
    mutate(ag= length(AFSI))%>%
    mutate(afs = sum(AFSI))%>%
    slice(1)%>%
    ungroup()%>%
    select("Team"="ATeam",ag,afs)
  hfs<- AllMLBGames%>%
    group_by(HTeam)%>%
    mutate(hg= length(HFSI))%>%
    mutate(hfs = sum(HFSI))%>%
    slice(1)%>%
    ungroup()%>%
    select("Team"="HTeam",hg,hfs)
  sfs<- full_join(afs,hfs, by="Team")%>%
    mutate(ag= ifelse(is.na(ag)==TRUE,0,ag))%>%
    mutate(hg= ifelse(is.na(hg)==TRUE,0,hg))%>%
    mutate(afs= ifelse(is.na(afs)==TRUE,0,afs))%>%
    mutate(hfs= ifelse(is.na(hfs)==TRUE,0,hfs))%>%
    mutate(FSSched = (afs+hfs)/(ag+hg))%>%
    select(Team,FSSched)
  TeamStats<- left_join(TeamStats,sfs, by = "Team")
  lagdfa<- AllMLBGames%>%
    mutate(Date = str_extract(gameID,"\\d{1,8}"))%>%
    mutate(Date= as.numeric(Date))%>%
    select(Date, "Team"=ATeam, "FSI"=AFSI)
  lagdfh<- AllMLBGames%>%
    mutate(Date = str_extract(gameID,"\\d{1,8}"))%>%
    mutate(Date= as.numeric(Date))%>%
    select(Date, "Team"=HTeam, "FSI"=HFSI)
  lagdf<- rbind(lagdfa,lagdfh)
  for (i in c(1:length(unique(lagdf$Date)))) {
    i = i
    x <- unique(lagdf$Date)[i]
    ind <- which(lagdf$Date == x)
    lagdf$Date[ind] <- i
  }
  todday<- max(lagdf$Date)
  last5<- lagdf %>%
    mutate(Date= as.numeric(Date))%>%
    arrange(-Date)%>%
    group_by(Team)%>%
    slice(1:5)%>%
    mutate(fs= mean(FSI))%>%
    slice(1)%>%
    ungroup()%>%
    select(Team, "last5"=fs)
  last10<- lagdf %>%
    mutate(Date= as.numeric(Date))%>%
    arrange(-Date)%>%
    group_by(Team)%>%
    slice(1:10)%>%
    mutate(fs= mean(FSI))%>%
    slice(1)%>%
    ungroup()%>%
    select(Team, "last10"=fs)
  last15<- lagdf %>%
    mutate(Date= as.numeric(Date))%>%
    arrange(-Date)%>%
    group_by(Team)%>%
    slice(1:15)%>%
    mutate(fs= mean(FSI))%>%
    slice(1)%>%
    ungroup()%>%
    select(Team, "last15"=fs)
  last20<- lagdf %>%
    mutate(Date= as.numeric(Date))%>%
    arrange(-Date)%>%
    group_by(Team)%>%
    slice(1:20)%>%
    mutate(fs= mean(FSI))%>%
    slice(1)%>%
    ungroup()%>%
    select(Team, "last20"=fs)
  TeamStats <- left_join(TeamStats, last5, by = "Team")
  TeamStats <- left_join(TeamStats, last10, by = "Team")
  TeamStats <- left_join(TeamStats, last15, by = "Team")
  TeamStats <- left_join(TeamStats, last20, by = "Team")
  divisions<-c(1,3,3,3,2,2,2,2,1,2,1,2,1,1,3,2,2,3,3,1,3,2,1,1,1,2,3,1,3,3)
  leagues<-c(1,1,2,2,1,2,1,2,1,2,2,2,2,1,1,1,2,1,2,2,1,1,1,2,1,1,2,2,2,1)
  TeamStats<-TeamStats %>%
    mutate(FloStrength =10*Hitting+10*Pitching+5*PitchScore+5*HitterScore+20*FSSched+2*last5+4*last10+3*last15+2*last20)%>%
    mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
    mutate(FloStrength = (5+FloStrength)/10)%>%
    arrange(Team)%>%
    mutate(Div=divisions)%>%
    mutate(Lg=leagues)%>%
    mutate(Div=ifelse(Div==1,"West",Div))%>%
    mutate(Div=ifelse(Div==2,"Central",Div))%>%
    mutate(Div=ifelse(Div==3,"East",Div))%>%
    mutate(Lg=ifelse(Lg==1,"NL",Lg))%>%
    mutate(Lg=ifelse(Lg==2,"AL",Lg))%>%
    mutate(Div=str_c(Lg," ",Div))
  for(i in c(29:39)){
    TeamStats[,i]=round(TeamStats[,i],3)
  }
  
  return(TeamStats)
}
getMLBResults <- function(yestdate){
  AllMLBGames<- read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/MLBGames2023")
  url <- str_c("https://www.espn.com/mlb/scoreboard/_/date/",yestdate)
  h <- read_html(url) 
  ATeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
  HTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
  AScore<- html_nodes(h, ".ScoreboardScoreCell__Item--away .baseball:nth-child(1)") %>% html_text%>%as.numeric()
  HScore<- html_nodes(h, ".ScoreboardScoreCell__Item--home .baseball:nth-child(1)") %>% html_text%>%as.numeric()
  ATeam = ATeam[0:length(AScore)]
  HTeam = HTeam[0:length(HScore)]
  
  MLBGames<-data.frame(ATeam,HTeam,AScore,HScore)%>%
    mutate(gameID= str_c(yestdate,c(1:length(AScore))))
  Teams <- c("Dodgers"="LAD", "Angels"="LAA", "Rays" = "TB", "Orioles" = "BAL", "Brewers" = "MIL", "Giants" = "SF", "Marlins" = "MIA", "Padres" = "SD", "White Sox" = "CHW", "Mets" = "NYM", "Athletics" = "OAK", "Astros" = "HOU", "Yankees" = "NYY", "Blue Jays" = "TOR", "Braves" = "ATL", "Cubs" = "CHC", "Mariners" = "SEA", "Rangers" = "TEX", "Twins" = "MIN", "Guardians" = "CLE", "Royals" = "KC", "Tigers" = "DET", "Red Sox" = "BOS", "Rockies" = "COL", "Diamondbacks" = "ARI", "Reds" = "CIN", "Cardinals" = "STL", "Pirates" = "PIT", "Phillies" = "PHI", "Nationals" = "WSH")
  MLBGames$ATeam <-as.character(Teams[MLBGames$ATeam])
  MLBGames$HTeam <-as.character(Teams[MLBGames$HTeam])
  
  MLBGames<- rbind(AllMLBGames,MLBGames) 
  MLBGames<-unique(MLBGames)
  write_csv(MLBGames,"/Users/seanfloersch/FloStrength/MLBFloStrength/MLBGames2023")
  return(MLBGames)
} 

################PREDICT#####################


predMLBToday<-function(toddate){
  DailyOdds<- function(toddate) {
    h <- read_html("https://www.sportsline.com/mlb/odds/money-line/") 
    Team <- html_nodes(h, ".away-team .cQwbsx") %>% html_text
    Opponent <- html_nodes(h, ".home-team .cQwbsx") %>% html_text

    h <- read_html("https://www.sportsline.com/mlb/odds/picks-against-the-spread/") 
    SpreadAway1 <- html_nodes(h, ".away-team .projected-score+ td .sc-75ef2e8c-2") %>% html_text
    SpreadAway=substr(SpreadAway1,1,4)
    SpreadAway <- str_replace_all(SpreadAway, "\\+", "") %>% as.numeric
    SpreadAwayOdd<-substr(SpreadAway1,5,8)
    SpreadAwayOdd <- str_replace_all(SpreadAwayOdd, "\\+", "") %>% as.numeric
    
    SpreadHome1 <- html_nodes(h, ".home-team .projected-score+ td .sc-75ef2e8c-2") %>% html_text
    SpreadHome=substr(SpreadHome1,1,4)
    SpreadHome <- str_replace_all(SpreadHome, "\\+", "") %>% as.numeric
    SpreadHomeOdd<-substr(SpreadHome1,5,8)
    SpreadHomeOdd <- str_replace_all(SpreadHomeOdd, "\\+", "") %>% as.numeric
    
    h <- read_html("https://www.sportsline.com/mlb/odds/over-under/") 
    OU <- html_nodes(h, ".away-team td:nth-child(4)") %>% html_text
    OU<-substr(OU,2,5) 
    OU<-str_replace(OU,"-","+")
    OU<-sub("\\+.*", "", OU)%>%as.numeric()
    BookOdds <- data.frame(Team, SpreadAwayOdd, Opponent, SpreadHomeOdd, SpreadAway, SpreadAwayOdd, SpreadHomeOdd,OU)
    colnames(BookOdds) <- c("ATeam", "Aodd", "HTeam", "Hodd", "Spread", "SpreadOdd", "oppoSpreadOdd","OU")
    
    BookOdds <-BookOdds %>%
      na.omit() %>%
      mutate(BookWin = NA) 
    Teams <- c("Dodgers"="lad", "Angels"="laa", "Rays" = "tb", "Orioles" = "bal", "Brewers" = "mil", "Giants" = "sf", "Marlins" = "mia", "Padres" = "sd", "White Sox" = "chw", "Mets" = "nym", "Athletics" = "oak", "Astros" = "hou", "Yankees" = "nyy", "Blue Jays" = "tor", "Braves" = "atl", "Cubs" = "chc", "Mariners" = "sea", "Rangers" = "tex", "Twins" = "min", "Guardians" = "cle", "Royals" = "kc", "Tigers" = "det", "Red Sox" = "bos", "Rockies" = "col", "Diamondbacks" = "ari", "Reds" = "cin", "Cardinals" = "stl", "Pirates" = "pit", "Phillies" = "phi", "Nationals" = "wsh")
    BookOdds$ATeam <-as.character(Teams[BookOdds$ATeam])
    BookOdds$HTeam <-as.character(Teams[BookOdds$HTeam])
    BookOdds <- BookOdds %>%
      mutate(ATeam = toupper(ATeam)) %>%
      mutate(HTeam = toupper(HTeam))
    ind <- which(BookOdds$Hodd < 0)
    BookOdds$BookWin[ind] <- BookOdds$HTeam[ind]
    ind <- which(BookOdds$Hodd > 0)
    BookOdds$BookWin[ind] <- BookOdds$ATeam[ind]
    
    len = length(BookOdds$BookWin) %>% str_remove_all("\\[a,z]") %>% as.numeric
    BookOdds <- BookOdds %>%
      arrange(ATeam) %>%
      mutate(gamenumber = c(1:len)) %>%
      group_by(ATeam) %>%
      mutate(gameID = str_c(ATeam, HTeam, toddate,"game",c(1:length(ATeam)))) %>%
      ungroup %>%
      select(-ATeam, -HTeam,-gamenumber)
    
    BookOdds <- BookOdds
    return(BookOdds)
  }
  BO <- DailyOdds(toddate)%>%
    select(gameID, BookWin,Spread, Hodd, Aodd, SpreadOdd, oppoSpreadOdd, OU)
  
  h <- read_html(str_c("https://www.espn.com/mlb/scoreboard/_/date/", toddate, "/")) 
  AwayTeam<- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text()
  #AwayTeam=AwayTeam[3:15]
  HomeTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text()
  #HomeTeam=HomeTeam[3:15]
  AwaySP <- html_nodes(h, ".ContentList__Item:nth-child(1) .Athlete__PlayerName") %>% html_text()%>%tolower%>%str_remove(". ")
  #AwaySP=AwaySP[5:17]
  HomeSP <- html_nodes(h, ".ContentList__Item+ .ContentList__Item .Athlete__PlayerName") %>% html_text()%>%tolower%>%str_remove(". ")
  todsched<- data.frame(AwayTeam, HomeTeam, AwaySP,HomeSP)
  Teams <- c("Dodgers"="LAD", "Angels"="LAA", "Rays" = "TB", "Orioles" = "BAL", "Brewers" = "MIL", "Giants" = "SF", "Marlins" = "MIA", "Padres" = "SD", "White Sox" = "CHW", "Mets" = "NYM", "Athletics" = "OAK", "Astros" = "HOU", "Yankees" = "NYY", "Blue Jays" = "TOR", "Braves" = "ATL", "Cubs" = "CHC", "Mariners" = "SEA", "Rangers" = "TEX", "Twins" = "MIN", "Guardians" = "CLE", "Royals" = "KC", "Tigers" = "DET", "Red Sox" = "BOS", "Rockies" = "COL", "Diamondbacks" = "ARI", "Reds" = "CIN", "Cardinals" = "STL", "Pirates" = "PIT", "Phillies" = "PHI", "Nationals" = "WSH")
  todsched$AwayTeam <-as.character(Teams[todsched$AwayTeam])
  todsched$HomeTeam <-as.character(Teams[todsched$HomeTeam])
  teams<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthCurrent/MLB/TeamMaster.csv" )%>%
    select(Team,PitchScore,HitterScore, Hitting, Pitching, FloStrength, FSSched, last5,last10,last15,last20)
  pitchers<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthCurrent/MLBPitchers" )
  getActivePitchers<-function(Pitchers){
    teams <- c("HOU", "TOR", "LAA", "BOS", "WSH", "CHW", "CIN", "SD", "KC", "COL", "SF", "MIN", "LAD", "ATL", "PHI", "DET", "PIT", "BAL", "NYM", "NYY", "MIA", "ARI", "OAK", "TB", "STL", "CLE", "CHC", "TEX", "MIL", "SEA")
    get_roster_data <- function(team){
      url <- str_c("https://www.espn.com/mlb/team/roster/_/name/",team, sep="")
      h <- read_html(url) 
      Name <- html_nodes(h, ".Table__TD+ .Table__TD .AnchorLink") %>% html_text
      Pos <- html_nodes(h, ".Table__TD:nth-child(3) .inline") %>% html_text
      a <- data.frame(Name, Pos) %>%
        mutate(Team = team)%>%
        mutate(Pos= ifelse(Pos %in% c("RP","SP"),"P",Pos))
      return(a)
    }
    roster <- map_df(.x= teams, .f=get_roster_data)
    injured<- str_c("https://www.espn.com/mlb/injuries")
    h <- read_html(injured) 
    Name <- html_nodes(h, ".Table__TD .AnchorLink") %>% html_text
    Status <- html_nodes(h, ".plain") %>% html_text%>%tolower()
    Pos = html_nodes(h, ".col-pos") %>% html_text
    injured <- data.frame(Name, Status,Pos)%>%
      mutate(Pos= ifelse(Pos %in% c("RP","SP"),"P",Pos))%>%
      filter(Status!="day-to-day")
    actPitch = left_join(roster, Pitchers, by = c("Team","Name"))%>%
      filter(Pos.x == "P")%>%
      mutate(Pos = Pos.x)
    actPitch = left_join(actPitch, injured, by = c("Pos","Name"))%>%
      filter(is.na(Status)==TRUE)
    return(actPitch)
  }
  pitchers<- getActivePitchers(pitchers)%>%
    select(Name,Team,GS,GP,IP,FloStrength)%>%
    na.omit()
  sp<- pitchers%>%
    filter(GS/GP>.33)%>%
    mutate(Name=tolower(Name))%>%
    mutate(AwaySP=str_c(str_extract(Name, "[a-z]"),str_remove(Name,"[a-z]{1,15}\\s")))%>%
    mutate(IPerS=IP/GP)%>%
    select(AwaySP, "AwayTeam"=Team,"AwayIPerS"=IPerS,"AwayStarterFS"=FloStrength)
  toddf<-left_join(todsched,sp,by=c("AwayTeam","AwaySP"))
  sp<- pitchers%>%
    filter(GS/GP>.33)%>%
    mutate(Name=tolower(Name))%>%
    mutate(HomeSP=str_c(str_extract(Name, "[a-z]"),str_remove(Name,"[a-z]{1,15}\\s")))%>%
    mutate(IPerS=IP/GP)%>%
    select(HomeSP, "HomeTeam"=Team,"HomeIPerS"=IPerS,"HomeStarterFS"=FloStrength)
  toddf<-left_join(toddf,sp,by=c("HomeTeam","HomeSP"))
  rp<- pitchers%>%
    filter(GS/GP<=.33)%>%
    group_by(Team)%>%
    mutate(ipperg=IP/GP)%>%
    mutate(prop=ipperg/sum(ipperg))%>%
    mutate(ReliefFS= sum(prop*FloStrength))%>%
    slice(1)%>%
    select(Team,ReliefFS)
  arp<- rp%>% select("AwayTeam"=Team,"AwayReliefFS"="ReliefFS")
  hrp<- rp%>% select("HomeTeam"=Team,"HomeReliefFS"="ReliefFS")
  toddf<-left_join(toddf,arp,by="AwayTeam")
  toddf<-left_join(toddf,hrp,by="HomeTeam")%>%
    mutate(AwayIPerS=ifelse(is.na(AwayIPerS)==TRUE,0,AwayIPerS))%>%
    mutate(HomeIPerS=ifelse(is.na(HomeIPerS)==TRUE,0,HomeIPerS))%>%
    mutate(AwayStarterFS=ifelse(is.na(AwayStarterFS)==TRUE,0,AwayStarterFS))%>%
    mutate(HomeStarterFS=ifelse(is.na(HomeStarterFS)==TRUE,0,HomeStarterFS))
  todpreds<- toddf%>%
    mutate(AwayPitchTodFS= (AwayIPerS*AwayStarterFS+(9-AwayIPerS)*AwayReliefFS)/9)%>%
    mutate(HomePitchTodFS= (HomeIPerS*HomeStarterFS+(9-HomeIPerS)*HomeReliefFS)/9)
  ateams<- teams
  for (i in c(1:11)) {
    colnames(ateams)[i]=str_c("Away",colnames(ateams)[i])
  }
  hteams<- teams
  for (i in c(1:11)) {
    colnames(hteams)[i]=str_c("Home",colnames(hteams)[i])
  }
  todpreds<-left_join(todpreds,ateams,by="AwayTeam")
  todpreds<-left_join(todpreds,hteams,by="HomeTeam")%>%
    group_by(AwayTeam)%>%
    mutate(gamenum=c(1:length(AwayTeam)))%>%
    ungroup()%>%
    mutate(gameID=str_c(AwayTeam,HomeTeam,toddate,"game",gamenum))
  todpreds<-left_join(todpreds,BO,by="gameID")
  return(todpreds)
}
predMLBModel<- function(toddate){
  h <- read_html("https://www.fantasypros.com/mlb/park-factors.php") 
  team <- html_nodes(h, "#data-table small") %>% html_text %>% str_remove("\\(")%>% str_remove("\\)")
  vec <- c()
  for (i in c(1:30)){
    vec[i] <- 3 * (i-1) + 1
  }
  Team <- team[vec]
  rating <- html_nodes(h, "td:nth-child(2) b") %>% html_text %>% as.numeric()
  StadRat <- rating[vec]
  
  Teams <- c("Los Angeles Dodgers"="lad", "Los Angeles Angels"="laa", "Tampa Bay Rays" = "tb", "Baltimore Orioles" = "bal", "Milwaukee Brewers" = "mil", "San Francisco Giants" = "sf", "Miami Marlins" = "mia", "San Diego Padres" = "sd", "Chicago White Sox" = "chw", "New York Mets" = "nym", "Oakland Athletics" = "oak", "Houston Astros" = "hou", "New York Yankees" = "nyy", "Toronto Blue Jays" = "tor", "Atlanta Braves" = "atl", "Chicago Cubs" = "chc", "Seattle Mariners" = "sea", "Texas Rangers" = "tex", "Minnesota Twins" = "min", "Cleveland Guardians" = "cle", "Kansas City Royals" = "kc", "Detroit Tigers" = "det", "Boston Red Sox" = "bos", "Colorado Rockies" = "col", "Arizona Diamondbacks" = "ari", "Cincinnati Reds" = "cin", "St. Louis Cardinals" = "stl", "Pittsburgh Pirates" = "pit", "Philadelphia Phillies" = "phi", "Washington Nationals" = "wsh")
  Team<- as.character(Teams[Team])
  HomeTeam <- toupper(Team)
  parkdf <- data.frame(HomeTeam, StadRat)
  allmod<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/MLBModelDF23")
  allmod<-left_join(allmod,parkdf,by="HomeTeam")
  modf<- allmod%>% 
    mutate(BookWin=ifelse(Spread==-1.5|(Spread==1.5&SpreadOdd< -240),AwayTeam,HomeTeam))%>%
    mutate(PD=AScore-HScore)%>% 
    na.omit()%>%
    mutate(AwayFSSched=(3+AwayFSSched)/6)%>%
    mutate(Awaylast5=(3+Awaylast5)/6)%>%
    mutate(Awaylast10=(3+Awaylast10)/6)%>%
    mutate(Awaylast15=(3+Awaylast15)/6)%>%
    mutate(Awaylast20=(3+Awaylast20)/6)%>%
    mutate(HomeFSSched=(3+HomeFSSched)/6)%>%
    mutate(Homelast5=(3+Homelast5)/6)%>%
    mutate(Homelast10=(3+Homelast10)/6)%>%
    mutate(Homelast15=(3+Homelast15)/6)%>%
    mutate(Homelast20=(3+Homelast20)/6)%>%
    mutate(AwayACWR5=Awaylast5/AwayFSSched)%>%
    mutate(AwayACWR10=Awaylast10/AwayFSSched)%>%
    mutate(AwayACWR15=Awaylast15/AwayFSSched)%>%
    mutate(AwayACWR20=Awaylast20/AwayFSSched)%>%
    mutate(HomeACWR5=Homelast5/HomeFSSched)%>%
    mutate(HomeACWR10=Homelast10/HomeFSSched)%>%
    mutate(HomeACWR15=Homelast15/HomeFSSched)%>%
    mutate(HomeACWR20=Homelast20/HomeFSSched)%>%
    mutate(AWin=ifelse(PD>0,1,0))%>%
    mutate(totalruns=AScore+HScore)%>%
    mutate(AwayBO=ifelse(Aodd<0&Spread==1.5,(-1*Aodd)/(-1*Aodd+200),0))%>%
    mutate(AwayBO=ifelse(Aodd>0&Spread==1.5,(200-Aodd)/((200-Aodd)+200),AwayBO))%>%
    mutate(AwayBO=ifelse(Spread==-1.5&Hodd<0,1-(-1*Hodd)/(-1*Hodd+200),AwayBO))%>%
    mutate(AwayBO=ifelse(Spread==-1.5&Hodd>0,(Hodd+100)/(Hodd+200),AwayBO))%>%
    mutate(AwayBO=ifelse(Aodd< -200&Spread==1.5,.5,AwayBO))%>%
    mutate(TeamDiff1=AwayHitterScore-HomePitchScore)%>%
    mutate(TeamDiff2=AwayPitchScore-HomeHitterScore)%>%
    mutate(TeamDiff3=AwayHitting-HomePitching)%>%
    mutate(TeamDiff4=AwayPitching-HomeHitting)%>%
    mutate(last5diff=Awaylast5-Homelast5)%>%
    mutate(last10diff=Awaylast10-Homelast10)%>%
    mutate(last15diff=Awaylast15-Homelast15)%>%
    mutate(last20diff=Awaylast20-Homelast20)%>%
    mutate(schedDiff= AwayFSSched-HomeFSSched)
  spreadmod<-lm(PD~AwayBO+AwayPitchTodFS+HomePitchTodFS+TeamDiff1+TeamDiff2+TeamDiff3+TeamDiff4+last5diff+last10diff+last15diff+last20diff+schedDiff,data=modf)
  oumod<-lm(totalruns~OU+StadRat*(AwayFloStrength+AwayFSSched+Awaylast5+Awaylast10+Awaylast15+Awaylast20+AwayPitchTodFS*HomeHitterScore+AwayHitterScore*HomePitchTodFS+HomeFloStrength+ HomeFSSched+Homelast5+Homelast10+Homelast15+Homelast20),data=modf)
  logmod<-glm(AWin~AwayBO+AwayPitchTodFS+HomePitchTodFS+TeamDiff1+TeamDiff2+TeamDiff3+TeamDiff4+last5diff+last10diff+last15diff+last20diff+schedDiff,data=modf,family = binomial)
  set.seed(121212)
  svmmod <- svm(AWin~AwayBO+AwayPitchTodFS+HomePitchTodFS+TeamDiff1+TeamDiff2+TeamDiff3+TeamDiff4+last5diff+last10diff+last15diff+last20diff+schedDiff, data = modf,probability = TRUE)
  rfmod <- randomForest(AWin~AwayBO+AwayPitchTodFS+HomePitchTodFS+TeamDiff1+TeamDiff2+TeamDiff3+TeamDiff4+last5diff+last10diff+last15diff+last20diff+schedDiff, data = modf)
  nnmod  <- neuralnet(AWin~AwayBO+AwayPitchTodFS+HomePitchTodFS+TeamDiff1+TeamDiff2+TeamDiff3+TeamDiff4+last5diff+last10diff+last15diff+last20diff+schedDiff, data = modf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  gbmmod <- gbm(AWin~AwayBO+AwayPitchTodFS+HomePitchTodFS+TeamDiff1+TeamDiff2+TeamDiff3+TeamDiff4+last5diff+last10diff+last15diff+last20diff+schedDiff, data = modf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 100) 
  
  df<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/yestmoddf")%>%
    mutate(BookWin=ifelse(Spread==-1.5|(Spread==1.5&SpreadOdd< -240),AwayTeam,HomeTeam))%>%
    na.omit()%>%
    mutate(AwayFSSched=(3+AwayFSSched)/6)%>%
    mutate(Awaylast5=(3+Awaylast5)/6)%>%
    mutate(Awaylast10=(3+Awaylast10)/6)%>%
    mutate(Awaylast15=(3+Awaylast15)/6)%>%
    mutate(Awaylast20=(3+Awaylast20)/6)%>%
    mutate(HomeFSSched=(3+HomeFSSched)/6)%>%
    mutate(Homelast5=(3+Homelast5)/6)%>%
    mutate(Homelast10=(3+Homelast10)/6)%>%
    mutate(Homelast15=(3+Homelast15)/6)%>%
    mutate(Homelast20=(3+Homelast20)/6)%>%
    mutate(AwayACWR5=Awaylast5/AwayFSSched)%>%
    mutate(AwayACWR10=Awaylast10/AwayFSSched)%>%
    mutate(AwayACWR15=Awaylast15/AwayFSSched)%>%
    mutate(AwayACWR20=Awaylast20/AwayFSSched)%>%
    mutate(HomeACWR5=Homelast5/HomeFSSched)%>%
    mutate(HomeACWR10=Homelast10/HomeFSSched)%>%
    mutate(HomeACWR15=Homelast15/HomeFSSched)%>%
    mutate(HomeACWR20=Homelast20/HomeFSSched)%>%
    mutate(AwayBO=ifelse(Aodd<0&Spread==1.5,(-1*Aodd)/(-1*Aodd+200),0))%>%
    mutate(AwayBO=ifelse(Aodd>0&Spread==1.5,(200-Aodd)/((200-Aodd)+200),AwayBO))%>%
    mutate(AwayBO=ifelse(Spread==-1.5&Hodd<0,1-(-1*Hodd)/(-1*Hodd+200),AwayBO))%>%
    mutate(AwayBO=ifelse(Spread==-1.5&Hodd>0,(Hodd+100)/(Hodd+200),AwayBO))%>%
    mutate(AwayBO=ifelse(Aodd< -200&Spread==1.5,.5,AwayBO))%>%
    mutate(TeamDiff1=AwayHitterScore-HomePitchScore)%>%
    mutate(TeamDiff2=AwayPitchScore-HomeHitterScore)%>%
    mutate(TeamDiff3=AwayHitting-HomePitching)%>%
    mutate(TeamDiff4=AwayPitching-HomeHitting)%>%
    mutate(last5diff=Awaylast5-Homelast5)%>%
    mutate(last10diff=Awaylast10-Homelast10)%>%
    mutate(last15diff=Awaylast15-Homelast15)%>%
    mutate(last20diff=Awaylast20-Homelast20)%>%
    mutate(schedDiff= AwayFSSched-HomeFSSched)
  df<-left_join(df,parkdf,by="HomeTeam")
  logpred<- predict(logmod, newdata = df,type = "response")
  svmpred <- predict(svmmod, newdata = df, probability = TRUE)
  svmwin <- as.vector(ifelse(svmpred>.5,1,0))
  rfpred<-as.vector(predict(rfmod, newdata = df))
  rfwin=ifelse(rfpred>0.5,1,0)
  nnpred<-as.vector((compute(nnmod,df))$net.result)
  nnwin <- as.vector(ifelse(nnpred>0.5, 1, 0))
  gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, df, type = "response")
  gbmwin=ifelse(gbmpred>0.5,1,0)
  spreadpred<-predict(spreadmod, newdata = df)
  oupred<-predict(oumod, newdata = df)
  
  df<- df%>%
    mutate(spreadProb= (4+spreadpred)/8)%>%
    mutate(prob=(logpred+nnpred+gbmpred)/3)%>%
    mutate(log=ifelse(logpred>.5,1,0))%>%
    mutate(book=ifelse(BookWin==AwayTeam,1,0))%>%
    mutate(svm=svmwin)%>%
    mutate(rf=rfwin)%>%
    mutate(nn=nnwin)%>%
    mutate(gbm=gbmwin)%>%
    mutate(Ens=ifelse((prob)>.5,1,0))%>%
    mutate(predOU=oupred)%>%
    mutate(predSpread=spreadpred)%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    filter(date==toddate)
   
  write_csv(df,"/Users/seanfloersch/FloStrength/MLBFloStrength/mlbmodpredictyest")
  
  modeldf<-df%>%
    select(AwayTeam,HomeTeam,gameID,BookWin, log,svm,rf,nn,gbm,Ens,book, predOU,predSpread,Aodd,Hodd,OU,Spread,SpreadOdd,oppoSpreadOdd,prob,AwayBO)%>%
    mutate(PredSp=ifelse(predSpread>1.5&book==1,-1.5,NA))%>%
    mutate(PredSp=ifelse(is.na(PredSp)==TRUE&(predSpread< -1.5&book==0),-1.5,PredSp))%>%
    mutate(PredSp=ifelse(is.na(PredSp)==TRUE,1.5,PredSp))%>%
    mutate(Spread=ifelse((book==1 & Hodd> -110)&Spread!=1.5,-1.5,1.5))%>%
    mutate(Spread=ifelse(book==0 & Aodd> -110,-1.5,Spread))
  betdf<-modeldf%>%
    mutate(WLwinnings= ifelse(Ens==0,(AwayBO*1.82)/1,((1-AwayBO)*1.82)/1))%>%
    mutate(WLBet=ifelse(Ens==1,AwayTeam,HomeTeam))%>%
    mutate(SPRwinnings=ifelse(Spread==PredSp&HomeTeam==BookWin,-100/(Aodd),NA))%>%
    mutate(SPRwinnings=ifelse(Spread==PredSp&AwayTeam==BookWin,-100/(Hodd),SPRwinnings))%>%
    mutate(SPRwinnings=ifelse(Spread!=PredSp&HomeTeam==BookWin,Hodd/(100),SPRwinnings))%>%
    mutate(SPRwinnings=ifelse(Spread!=PredSp&AwayTeam==BookWin,Aodd/(100),SPRwinnings))%>%
    mutate(SPRwinnings=ifelse(SPRwinnings< -1, -1/SPRwinnings,SPRwinnings ))%>%
    mutate(SPRwinnings=ifelse(SPRwinnings< 0, -1 * SPRwinnings,SPRwinnings ))%>%
    mutate(SPRBet=PredSp)%>%
    mutate(OUwinnings= .91)%>%
    mutate(oudiff=OU-predOU)%>%
    mutate(OUBet=ifelse(oudiff<0,"Over","Under"))%>%
    select(AwayTeam,HomeTeam,gameID,book,Ens, predOU,predSpread,AwayBO, prob,Spread,SpreadOdd,oppoSpreadOdd,Aodd,Hodd,BookWin,WLBet,WLwinnings,SPRBet,SPRwinnings,OU,OUBet,OUwinnings)%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    filter(date==toddate)%>%
    select(-date)%>%
    mutate(WLDiff=ifelse(WLBet==BookWin,AwayBO-prob,abs(AwayBO-prob)))%>%
    mutate(OUDiff=predOU-OU)%>%
    mutate(OUProb=pnorm(abs(predOU-OU),mean=0,sd=4.586072))%>%
    mutate(SprDiff=abs((-1*predSpread)-Spread))%>%
    mutate(pos1.5prob= ifelse(predSpread>0&AwayTeam==BookWin,1-pnorm(predSpread-1.5,sd=4.23),0))%>%
    mutate(pos1.5prob= ifelse(predSpread<0&HomeTeam==BookWin,1-pnorm(-1*(predSpread)-1.5,sd=4.23),pos1.5prob))%>%
    mutate(pos1.5prob= ifelse(predSpread>0&HomeTeam==BookWin,pnorm((predSpread)+1.5,sd=4.23),pos1.5prob))%>%
    mutate(pos1.5prob= ifelse(predSpread<0&AwayTeam==BookWin,pnorm(-1*(predSpread)+1.5,sd=4.23),pos1.5prob))%>%
    mutate(neg1.5prob=1-pos1.5prob)%>%
    mutate(Bneg1.5prob=ifelse(Spread==-1.5&HomeTeam==BookWin,-1*oppoSpreadOdd/(100+-1*oppoSpreadOdd),NA))%>%
    mutate(Bneg1.5prob=ifelse(Spread==-1.5&AwayTeam==BookWin,-1*SpreadOdd/(100+-1*SpreadOdd),Bneg1.5prob))%>%
    mutate(Bneg1.5prob=ifelse(Spread==1.5&HomeTeam==BookWin,100/(100+oppoSpreadOdd),Bneg1.5prob))%>%
    mutate(Bneg1.5prob=ifelse(Spread==1.5&AwayTeam==BookWin,100/(100+SpreadOdd),Bneg1.5prob))%>%
    mutate(Bpos1.5prob=ifelse(Spread==1.5&AwayTeam==BookWin,-1*oppoSpreadOdd/(100+-1*oppoSpreadOdd),NA))%>%
    mutate(Bpos1.5prob=ifelse(Spread==1.5&HomeTeam==BookWin,-1*SpreadOdd/(100+-1*SpreadOdd),Bpos1.5prob))%>%
    mutate(Bpos1.5prob=ifelse(Spread==-1.5&AwayTeam==BookWin,100/(100+oppoSpreadOdd),Bpos1.5prob))%>%
    mutate(Bpos1.5prob=ifelse(Spread==-1.5&HomeTeam==BookWin,100/(100+SpreadOdd),Bpos1.5prob))%>%
    mutate(Bneg1.5prob=ifelse(Bneg1.5prob<0,-1*SpreadOdd/(100-SpreadOdd),Bneg1.5prob))%>%
    mutate(Bpos1.5prob=ifelse(Bpos1.5prob<0,-1*oppoSpreadOdd/(100-oppoSpreadOdd),Bpos1.5prob))%>%
    mutate(SpreadProbDiff= ifelse(SPRBet==-1.5,neg1.5prob-Bneg1.5prob,pos1.5prob-Bpos1.5prob))
  write_csv(betdf,"/Users/seanfloersch/FloStrength/MLBFloStrength/mlballbetsyest")
  betFS<- betdf%>%
    select(gameID,AwayTeam,HomeTeam,BookWin,OU,WLBet,SPRBet,OUBet,WLDiff,OUProb,SpreadProbDiff,SPRwinnings,WLwinnings,OUwinnings)%>%
    mutate(OUConfidence= ifelse(OUProb<.542,1,NA))%>%
    mutate(OUConfidence=ifelse(OUProb>=.542&OUProb<.6,2,OUConfidence))%>%
    mutate(OUConfidence=ifelse(OUProb>=.6&OUProb<.65,3,OUConfidence))%>%
    mutate(OUConfidence=ifelse(OUProb>=.65&OUProb<.7,4,OUConfidence))%>%
    mutate(OUConfidence=ifelse(OUProb>=.7,5,OUConfidence))%>%
    mutate(SPRConfidence=ifelse(SpreadProbDiff< -.0423,1,NA))%>%
    mutate(SPRConfidence=ifelse(SpreadProbDiff>= -.0423&SpreadProbDiff<.0423,2,SPRConfidence))%>%
    mutate(SPRConfidence=ifelse(SpreadProbDiff>= .0423&SpreadProbDiff<.09,3,SPRConfidence))%>%
    mutate(SPRConfidence=ifelse(SpreadProbDiff>= .09&SpreadProbDiff<.135,4,SPRConfidence))%>%
    mutate(SPRConfidence=ifelse(SpreadProbDiff>= .135,5,SPRConfidence))%>%
    mutate(WLConfidence =ifelse(WLDiff<0,1,NA))%>%
    mutate(WLConfidence =ifelse(WLDiff>=0&WLDiff< .0423,2,WLConfidence))%>%
    mutate(WLConfidence =ifelse(WLDiff>=.0423&WLDiff< .09,3,WLConfidence))%>%
    mutate(WLConfidence =ifelse(WLDiff>=0.09&WLDiff< .135,4,WLConfidence))%>%
    mutate(WLConfidence =ifelse(WLDiff>=.135,5,WLConfidence))%>%
    select(-WLDiff,-OUProb,-SpreadProbDiff)
  write_csv(betFS,"/Users/seanfloersch/FloStrength/MLBFloStrength/mlbbestbetsyest")
}

################UPDATE#####################


updMLBModel<- function(yestdate){
  mlbgames<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/MLBGames2023")%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    filter(date==yestdate)%>%
    group_by(date,ATeam)%>%
    mutate(gamenum=c(1:length(ATeam)))%>%
    ungroup()%>%
    mutate(gameID=str_c(ATeam,HTeam,date,"game",gamenum))%>%
    select(AScore,HScore,gameID)
  yestmod<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/yestmoddf")
  yestmoddf<-left_join(yestmod,mlbgames, by="gameID")
  allmod<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/MLBModelDF23")
  allmod<-rbind(allmod,yestmoddf)
  allmod<-unique(allmod)
  modf<- allmod%>% mutate(PD=AScore-HScore)%>% na.omit()%>%
    mutate(AwayFSSched=(3+AwayFSSched)/6)%>%
    mutate(Awaylast5=(3+Awaylast5)/6)%>%
    mutate(Awaylast10=(3+Awaylast10)/6)%>%
    mutate(Awaylast15=(3+Awaylast15)/6)%>%
    mutate(Awaylast20=(3+Awaylast20)/6)%>%
    mutate(HomeFSSched=(3+HomeFSSched)/6)%>%
    mutate(Homelast5=(3+Homelast5)/6)%>%
    mutate(Homelast10=(3+Homelast10)/6)%>%
    mutate(Homelast15=(3+Homelast15)/6)%>%
    mutate(Homelast20=(3+Homelast20)/6)%>%
    mutate(AwayACWR5=Awaylast5/AwayFSSched)%>%
    mutate(AwayACWR10=Awaylast10/AwayFSSched)%>%
    mutate(AwayACWR15=Awaylast15/AwayFSSched)%>%
    mutate(AwayACWR20=Awaylast20/AwayFSSched)%>%
    mutate(HomeACWR5=Homelast5/HomeFSSched)%>%
    mutate(HomeACWR10=Homelast10/HomeFSSched)%>%
    mutate(HomeACWR15=Homelast15/HomeFSSched)%>%
    mutate(HomeACWR20=Homelast20/HomeFSSched)%>%
    mutate(AWin=ifelse(PD>0,1,0))%>%
    mutate(totalruns=AScore+HScore)
  write_csv(allmod,"/Users/seanfloersch/FloStrength/MLBFloStrength/MLBModelDF23")
}
updMLBModelRes<-function(yestdate){
  yestpreds<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/mlbmodpredictyest")%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    filter(date==yestdate)
  mlbgames<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/MLBGames2023")%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    filter(date==yestdate)%>%
    group_by(date,ATeam)%>%
    mutate(gamenum=c(1:length(ATeam)))%>%
    ungroup()%>%
    mutate(gameID=str_c(ATeam,HTeam,date,"game",gamenum))%>%
    select(AScore,HScore,gameID)%>%
    mutate(totRuns=AScore+HScore)%>%
    mutate(AWin=ifelse(AScore>HScore,1,0))%>%
    select(AWin,gameID)
  yestpreds<-left_join(yestpreds,mlbgames,by="gameID")%>%
    mutate(logcorr=ifelse(log==AWin,1,0))%>%
    mutate(svmcorr=ifelse(svm==AWin,1,0))%>%
    mutate(rfcorr=ifelse(rf==AWin,1,0))%>%
    mutate(nncorr=ifelse(nn==AWin,1,0))%>%
    mutate(gbmcorr=ifelse(gbm==AWin,1,0))%>%
    mutate(enscorr=ifelse(Ens==AWin,1,0))%>%
    mutate(bookcorr=ifelse(book==AWin,1,0))
  modres<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/mlbCompleteModelRes")%>%na.omit
  modres<-unique(rbind(yestpreds,modres))%>%na.omit
  write_csv(modres,"/Users/seanfloersch/FloStrength/MLBFloStrength/mlbCompleteModelRes")
}
updMLBBets<-function(yestdate){
  yestbets<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/mlballbetsyest")%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    filter(date==yestdate)
  mlbgames<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/MLBGames2023")%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    filter(date==yestdate)%>%
    group_by(date,ATeam)%>%
    mutate(gamenum=c(1:length(ATeam)))%>%
    ungroup()%>%
    mutate(gameID=str_c(ATeam,HTeam,date,"game",gamenum))%>%
    select(AScore,HScore,gameID)%>%
    mutate(totRuns=AScore+HScore)
  allbets<-left_join(yestbets,mlbgames,by="gameID")%>%
    mutate(PD=abs(AScore-HScore))%>%
    mutate(WTeam=ifelse(AScore>HScore,AwayTeam,HomeTeam))%>%
    na.omit()
  df<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/mlbbetresultsComplete")
  allbets<-unique(rbind(df,allbets))
  write_csv(allbets,"/Users/seanfloersch/FloStrength/MLBFloStrength/mlbbetresultsComplete")
  df<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/mlbbestbetsyest")
  df<-left_join(df,mlbgames,by="gameID")%>%
    mutate(PD=abs(AScore-HScore))%>%
    mutate(WTeam=ifelse(AScore>HScore,AwayTeam,HomeTeam))%>%
    na.omit()
  write_csv(df,"/Users/seanfloersch/FloStrength/MLBFloStrength/mlbbestbetsHisttest")
  dfSpr<- df %>%
    select(gameID,BookWin,WTeam, SPRBet,SPRConfidence, SPRwinnings,PD)%>%
    mutate(ActSpr= ifelse(WTeam==BookWin & PD > 1.5, -1.5,1.5))%>%
    mutate(Winnings=ifelse(SPRBet==ActSpr,SPRwinnings,-1))%>%
    mutate(BetType = "Spread")%>%
    mutate(BetCorrect= ifelse(SPRBet==ActSpr,1,0))%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    select(date, BetType, BetCorrect, "Confidence"=SPRConfidence, Winnings,gameID)
  dfWL <- df %>%
    select(gameID, WTeam, WLBet,WLwinnings, WLConfidence)%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    mutate(Winnings = ifelse(WTeam==WLBet,WLwinnings, -1))%>%
    mutate(BetType = "WL")%>%
    mutate(BetCorrect= ifelse(WTeam==WLBet,1,0))%>%  
    select(date, BetType, BetCorrect, "Confidence"=WLConfidence, Winnings,gameID)
  dfOU <- df %>%
    select(gameID, totRuns,OU, OUBet,OUwinnings, OUConfidence)%>%
    mutate(date=str_extract(gameID,"\\d{1,8}"))%>%
    mutate(actOU= ifelse(totRuns==OU, "Push", NA))%>%
    mutate(actOU= ifelse(totRuns> OU, "Over", actOU))%>%
    mutate(actOU= ifelse(totRuns< OU, "Under", actOU))%>%
    filter(actOU != "Push")%>%
    mutate(Winnings = ifelse(actOU==OUBet,OUwinnings, -1))%>%
    mutate(BetType = "OU")%>%
    mutate(BetCorrect= ifelse(actOU==OUBet,1,0))%>%  
    select(date, BetType, BetCorrect, "Confidence"=OUConfidence, Winnings,gameID)
  dfbet<- rbind(dfWL,dfSpr,dfOU)%>%
    mutate(Sport = "MLB")
  df<-read.csv("/Users/seanfloersch/FloStrength/MLBFloStrength/mlbbestbetsHist")
  dfbet<-unique(rbind(df,dfbet))
  write_csv(dfbet,"/Users/seanfloersch/FloStrength/MLBFloStrength/mlbbestbetsHist")
}
