getMLBSched<-function(x){
  date=str_remove_all(Sys.Date(), "-")
  year <- str_extract(date, "\\d{1,4}")
  month <- str_extract(date, "\\d{5,6}") 
  month = str_extract(month, "\\d{1,2}$")
  day <- str_extract(date, "\\d{1,2}$")
  date<- str_c(year,month,day, sep = "")
  h <- read_html(str_c("https://www.espn.com/mlb/schedule/_/date/",date)) 
  Away <- html_nodes(h, ".ScheduleTables--baseball:nth-child(1) .away .AnchorLink+ .AnchorLink") %>% html_text
  Home <- html_nodes(h, ".ScheduleTables--baseball:nth-child(1) .items-center .AnchorLink+ .AnchorLink") %>% html_text
  Time <- html_nodes(h, ".ScheduleTables--baseball:nth-child(1) .date__col .AnchorLink") %>% html_text
  if (length(Time)==length(Away)){
    mlb<- data.frame(Away,Home,Time) %>%
      mutate(Sport = "MLB")
  }else{
    mlb<- data.frame(Away,Home) %>%
      mutate(Sport = "MLB")%>%
      mutate(Time = NA)%>%
      na.omit()
  }
}
getMLBResults<-function(x){
  date=str_remove_all(Sys.Date()-1, "-")
  year <- str_extract(date, "\\d{1,4}")
  month <- str_extract(date, "\\d{5,6}") 
  month = str_extract(month, "\\d{1,2}$")
  day <- str_extract(date, "\\d{1,2}$")
  date<- str_c(year,month,day, sep = "")
  h <- read_html(str_c("https://www.espn.com/mlb/schedule/_/date/",date)) 
  Away <- html_nodes(h, ".ScheduleTables--baseball:nth-child(1) .away .AnchorLink+ .AnchorLink") %>% html_text
  Home <- html_nodes(h, ".ScheduleTables--baseball:nth-child(1) .items-center .AnchorLink+ .AnchorLink") %>% html_text
  Scores <- html_nodes(h, ".ScheduleTables--baseball:nth-child(1) .colspan__col+ .Table__TD .AnchorLink") %>% html_text
  if (length(Time)==length(Away)){
    mlb<- data.frame(Scores) %>%
      mutate(Sport = "MLB")
  }else{
    mlb<- data.frame(Away,Home) %>%
      mutate(Sport = "MLB")%>%
      mutate(Scores = NA)
  }
}
getNFLSched<- function(weeknum){
  h <- read_html(str_c("https://www.espn.com/nfl/schedule/_/week/",weeknum,"/year/2023/seasontype/2")) 
  Away <- html_nodes(h, ".away .AnchorLink+ .AnchorLink") %>% html_text
  Home <- html_nodes(h, ".items-center .AnchorLink+ .AnchorLink") %>% html_text
  Time <- html_nodes(h, ".date__col .AnchorLink") %>% html_text
  if (length(Time)==length(Away)){
    nfl<- data.frame(Away,Home,Time) %>%
      mutate(Sport = "NFL")
  }else{
    nfl<- data.frame(Away,Home) %>%
      mutate(Sport = "NFL")%>%
      mutate(Time = NA)
  }
}