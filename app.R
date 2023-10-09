library(ggplot2)
library(tidyverse)
library(plotly)
library(stringr)
library(rsconnect)
library(shinythemes)
library(bslib)
library(shinydashboard)
library(htmlwidgets)
library(shinybrowser)
library(rvest)

#setwd('/Users/seanfloersch/FloStrength/FloStrengthApp')
nbadf <- read.csv("NBAPlayersATforapp")
nfldf <- read.csv("NFLTeamsATforapp")
nflplayerdf1 <- read.csv("NFLCareersforapp")
nflplayerdf <- read.csv("QBSeasonsforapp")
mlbhitAT <- read.csv("MLBBattersATforapp")
mlbpitAT<- read.csv("MLBPitchersATforapp")
nflnames <- unique(nflplayerdf1$Name)
nflnames1 <- nflnames[nflnames!= ("Tom Brady")]
nflnames1 <- nflnames1[nflnames1!= ("Aaron Rodgers")]
nbanames <- unique(nbadf$Name)
nbanames<-nbanames[nbanames != ("Michael Jordan")]
nbanames<-nbanames[nbanames != ("LeBron James")]
mlbhitname <- unique(mlbhitAT$Name)
mlbhitname<-mlbhitname[mlbhitname != ("Babe Ruth")]
mlbhitname<-mlbhitname[mlbhitname != ("Albert Pujols")]
mlbpitchname <- unique(mlbpitAT$Name)
mlbpitchname<-mlbpitchname[mlbpitchname != ("Nolan Ryan")]
mlbpitchname<-mlbpitchname[mlbpitchname != ("Pedro Martinez")]
nhldf<- read.csv("NHLTeamsATforapp")
nhlnames<- read.csv("NHLPlayersATforapp")
nhlnames<- unique(nhlnames$Player)
nhlnames1<-nhlnames[nhlnames != ("Connor McDavid")]
nhlnames1<-nhlnames1[nhlnames1 != ("Auston Matthews")]
ui <-tagList(
  tags$style("html,body{background-color: white;}
                .container{
                    width: 130%;
                    margin: 0 auto;
                    padding: 0;
                }
                #myimg{
                    width:30%;
                }
               @media screen and (min-width: 100%){
                .container{
                    width: 100%;
                }
               }"),
  tags$div(class="container",
           dashboardPage(
             dashboardHeader(title = "FloStrength Sports"),
             dashboardSidebar(
               collapsed = TRUE,
               sidebarMenu(
                 menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard", startExpanded=FALSE)),
                 menuItem("All-Time Rankings", icon = icon("calendar"),
                          menuSubItem("Athletes", tabName = "tab_topathletes"),
                          menuSubItem("Teams", tabName = "tab_topteams")),
                 menuItem("Modelling", tabName="tab_model",icon = icon("chart-line")),
                 menuItem("Betting", icon = icon("dollar"),
                          menuSubItem("Today", tabName = "tab_betstoday"),
                          menuSubItem("History", tabName = "tab_betsAT")),
                 menuItem("NBA", tabName = "tab_nba", icon = icon("basketball"),
                          menuSubItem("Current", tabName = "tab_nbacurr"),
                          menuSubItem("Historical", tabName = "tab_nbahist"),
                          menuSubItem("Predictions", tabName = "tab_nbapred")),
                 menuItem("NFL", icon = icon("football"),
                          menuSubItem("Historical", tabName = "tab_nflhist")),
                 menuItem("NHL", icon = icon("hockey-puck"),
                          menuSubItem("Current", tabName = "tab_nhlcurr"),
                          menuSubItem("Historical", tabName = "tab_nhlhist"),
                          menuSubItem("Predictions", tabName = "tab_nhlpred")),
                 menuItem("NCAAMB", icon = icon("building-columns"),
                          menuSubItem("Current", tabName = "tab_ncaambcurr"),
                          menuSubItem("Historical", tabName = "tab_ncaambhist")),
                 menuItem("MLB", icon = icon("baseball"),
                          #menuSubItem("Current", tabName = "tab_mlbcurr"),
                          menuSubItem("Historical", tabName = "tab_mlbhist"),
                          menuSubItem("Projections", tabName = "tab_mlbpred"))
               )
             ),
             dashboardBody(
               tabItems(
                 #############Schedulling###########
                 tabItem("dashboard",
                         fluidRow(
                           mainPanel(
                             h1("Welcome to FloStrength Sports"),
                             h2("Updated Website Moved to"),
                             h2("https://rpubs.com/seanfloersch/flostrengthsports"),

                          )
                         )),
                 tabItem("tab_topathletes",
                         fluidRow(
                           mainPanel(
                             DT::dataTableOutput("TopAthletes")
                           )
                         )),
                 tabItem("tab_topteams",
                         fluidRow(
                           mainPanel(
                             DT::dataTableOutput("TopTeams")
                           )
                         )),
                 #############Modelling#############
                 tabItem("tab_model",
                         fluidRow(
                           mainPanel(
                             tabsetPanel(
                               tabPanel("All Sports",
                           sidebarPanel(
                             selectizeInput("modtype", "Model:", choices=c("Ens","GBM","NN","RF","SVM","Log", "E3")),
                             selectizeInput("modsport", "Sport:", choices=c("NBA","NHL","NCAAMB")),
                             tableOutput("ModelResDF"),
                             hr()),
                           mainPanel(
                             plotlyOutput("ModelRes"),
                             h2("Machine Learning Models"),
                             h3(strong("Logistic Regression (Log)")),
                             h4("Logistic regression takes in the variables and weights them linearly to create a probability of an event happening, in this case of a win."),
                             h4(em("PROS")),
                             h4("-Interpretability of variables and their importance"),
                             h4("-Good in binomial situations (0/1, wins/losses)"),
                             h4("-Commonly used/ known"),
                             h4(em("CONS")),
                             h4("-Assumes independence among variables"),
                             h4("-Assumes linearity"),
                             h4("-Can only predict discrete variables (0,1,2,3...)"),
                             h3(strong("Support Vector Machines (SVM)")),
                             h4("Support vector machines take in variables to linearly separate a predictor into separate classes, in this case a binary win/loss."),
                             h4(em("PROS")),
                             h4("-Good with multiple variables"),
                             h4("-Preforms well with smaller sample size"),
                             h4(em("CONS")),
                             h4("-Probability estimates are not interpretable"),
                             h4("-Struggles with overlapping classification groups"),
                             h4("-Larger training size decreases efficiency"),
                             h3(strong("Gradint Boosting Machines (GBM)")),
                             h4("GBMs apply a weaker machine type, such as a decision tree, and run iterations upon iterations to improve success."),
                             h4(em("PROS")),
                             h4("-Good with imbalanced data"),
                             h4("-Reduces error effectively"),
                             h4("-Minimizes impact of outliers"),
                             h4(em("CONS")),
                             h4("-If data is noisy, it can overfit and struggle with test data"),
                             h4("-Variable signifcance loses interpretability"),
                             h3(strong("Neural Network (NN)")),
                             h4("Neural networks are a deep learning, articial inteligence method that uses nodes to classify predictors based on recognition of patterns"),
                             h4(em("PROS")),
                             h4("-Good with large sample size"),
                             h4("-Can be used with regression and classification"),
                             h4(em("CONS")),
                             h4("-Variable signficance is lost"),
                             h4("-Susceptible to overfitting"),
                             h3(strong("Random Forest (RF)")),
                             h4("Random forests grow multiple,random decision trees and ensemble, choosing the response with the majority of trees"),
                             h4(em("PROS")),
                             h4("-Used for classification and regression"),
                             h4("-Efficient with large sample size"),
                             h4("-Deals well with outliers"),
                             h4(em("CONS")),
                             h4("-Not easily interpretable"),
                             h4("-Not controllable"),
                             h3(strong("Majority Ensemble (Ens)")),
                             h4("Majority ensembles take in the 5 main models and takes the projected winner as predicted my the majority of the models, in this case 3 or more of the 5 models."),
                             h4(em("PROS")),
                             h4("-Avoids bias from any model"),
                             h4("-More stability"),
                             h4("-Beneficial for multiple data types (categorical, numeric, etc.)"),
                             h4(em("CONS")),
                             h4("-Lower interpretability and lack of insights"),
                             h3(strong("Partial Ensemble (E3)")),
                             h4("Paritial Ensemble takes the highest three performing models and does a majority vote on those models, selecting a winner based on the team that was chose by 2 or 3 models."),
                             h4(em("PROS")),
                             h4("-Avoids bias from any model"),
                             h4("-More stability"),
                             h4("-Beneficial for multiple data types (categorical, numeric, etc.)"),
                             h4(em("CONS")),
                             h4("-Lower interpretability and lack of insights")
                           ))))
                         )),
                 #############Bets##############
                 tabItem("tab_betstoday",
                         selectizeInput("todbetsports", "Sport:", choices=c("All","NBA", "NHL", "NCAAMB")),
                         fluidRow(
                           mainPanel(
                             DT::dataTableOutput("BetsToday"))
                         )
                 ),
                 tabItem("tab_betsAT",
                         selectizeInput("bettimeframe", "Time frame:", choices=c("Yesterday","Last Week", "Last Month", "Last Year","All Time")),
                         selectizeInput("betsports", "Sport:", choices=c("All","NBA", "NHL", "NCAAMB")),
                         selectizeInput("betview", "Plot:", choices=c("Net overtime","Net Daily")),
                         fluidRow(
                           mainPanel(
                             plotlyOutput("BetPlot"),
                             DT::dataTableOutput("BetsHistory"))
                         )
                         
                 ),
                 
                 #############NBA###############
                 tabItem("tab_nbacurr",
                         fluidRow(
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Power Rankings",
                                        DT::dataTableOutput("NBAPowerRank")),
                               tabPanel("MVP Rankings",
                                        DT::dataTableOutput("NBAMVPRank"))
                             )
                           )
                         )),
                 tabItem("tab_nbahist",
                         fluidRow(
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Teams",
                                        sidebarLayout(      
                                          sidebarPanel(
                                            selectizeInput("nbateam", "Team:", choices=c("All",unique(nbadf$Tm))),
                                            sliderInput("yearnbateam", "Season Year", 1980, 2022, value = c(1980, 2022),sep = ""),
                                            hr()),
                                          mainPanel(DT::dataTableOutput("teamsNBA"))),
                               ),
                               tabPanel("Players",
                                        tabsetPanel(
                                          tabPanel("Seasons",
                                                   sidebarLayout(      
                                                     sidebarPanel(
                                                       selectizeInput("team", "Team:", choices=c("All",unique(nbadf$Tm))),
                                                       sliderInput("year", "Season Year", 1980, 2022, value = c(1980, 2022),sep = ""),
                                                       selectizeInput("nbaplayerszn", "Player:", choices=c("All",unique(nbadf$Name))),
                                                       selectizeInput("nbasznpos", "Position:", choices=c("All","PG","SG","SF","PF","C")),
                                                       radioButtons("nbasznradio", label = h3("Statistic Type"),
                                                                    choices = list("Flo Metrics" = 1, "Counting Stats" = 2, "Per Game Stats" = 3), 
                                                                    selected = 1),
                                                       hr()),
                                                     mainPanel(DT::dataTableOutput("seasonsNBA"))),
                                          ),
                                          tabPanel("Career",
                                                   sidebarLayout(      
                                                     sidebarPanel(
                                                       radioButtons("nbacarradio", label = h3("Statistic Type"),
                                                                    choices = list("Flo Metrics" = 1, "Counting Stats" = 2, "Per Game Stats" = 3), 
                                                                    selected = 1)),
                                                     mainPanel(DT::dataTableOutput("careerNBA")))
                                          )
                                        )),
                               tabPanel("Explore",
                                        tabsetPanel(
                                          tabPanel("Compare Players",
                                                   sidebarLayout(
                                                     sidebarPanel(
                                                       selectizeInput("nbacomp1", "Player 1:", choices=c("Michael Jordan",unique(nbanames))),
                                                       selectizeInput("nbacomp2", "Player 2:", choices=c("LeBron James",unique(nbanames))),
                                                       radioButtons("nbacomptype", label = h3("Comparison Type"),
                                                                    choices = list("Career"=1,"By Age" = 2, "By Experiance" = 3), 
                                                                    selected = 1),
                                                       radioButtons("nbacompcartype", label = h3("Comparison Stat"),
                                                                    choices = list("Value" = 1, "FloStrength" = 2,"Points"=3), 
                                                                    selected = 1)),
                                                     mainPanel(plotlyOutput("CompCarNBAPlot"))
                                                   )
                                          ),
                                          tabPanel("Impact of Age",
                                                   sidebarLayout(
                                                     sidebarPanel(
                                                       selectizeInput("nbaagepos", "Position:", choices=c("All","PG","SG","SF","PF","C")),
                                                       radioButtons("nbaagestat", label = h3("Stat"),
                                                                    choices = list("Value"=1,"FloStrength" = 2), 
                                                                    selected = 1)),
                                                     mainPanel(plotlyOutput("nbaageplot"))
                                                   )
                                          ),
                                          tabPanel("Impact of Year",
                                                   sidebarLayout(
                                                     sidebarPanel(
                                                       selectizeInput("nbayearpos", "Position:", choices=c("All","PG","SG","SF","PF","C")),
                                                       radioButtons("nbayearstat", label = h3("Stat"),
                                                                    choices = list("Value"=1,"FloStrength" = 2), 
                                                                    selected = 1)),
                                                     mainPanel(plotlyOutput("nbayearplot"))
                                                   )
                                          ),
                                          tabPanel("Trends",
                                                   sidebarLayout(
                                                     sidebarPanel(
                                                       radioButtons("nbacompyrstat", label = h3("Stat"),
                                                                    choices = list("Points Per Game"=1,"Competitiveness" = 2,"Three Pointers"=3), 
                                                                    selected = 1)),
                                                     mainPanel(plotlyOutput("nbayearcompplot"))
                                                   )
                                          ),
                                          tabPanel("Correlations",
                                                   selectizeInput("nbavar1", "Variable X:", choices=c("FloStrength","Wpct","PlayFS","TeamFS","SchedFS","Scoring","Defense","Rebounding","Passing")),
                                                   selectizeInput("nbavar2", "Variable Y:", choices=c("Wpct","FloStrength","PlayFS","TeamFS","SchedFS","Scoring","Defense","Rebounding","Passing")),
                                                   textOutput("corrNBA"),
                                                   mainPanel(plotlyOutput("corrNBAPlot")))
                                        )
                               ))
                           ))),
                 #############NFL##############
                 
                 tabItem("tab_nflhist",
                         fluidRow(
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Teams",
                                        tabsetPanel(
                                          tabPanel("Teams",
                                                   sidebarPanel(
                                                     selectizeInput("nflteam", "Team:", choices=c("All",unique(nfldf$Tm))),
                                                     hr()),
                                                   mainPanel(),
                                                   DT::dataTableOutput("teamsNFL")),
                                          tabPanel("Franchises",
                                                   mainPanel(DT::dataTableOutput("franNFL"))
                                          ))),
                               tabPanel("Players",
                                        tabsetPanel(
                                          tabPanel("Seasons",
                                                   sidebarPanel(
                                                     selectizeInput("nflsznteam", "Team:", choices=c("All",unique(nflplayerdf$Tm))),
                                                     selectizeInput("nflsznplayer", "Name:", choices=c("All",unique(nflnames))),
                                                     selectizeInput("nflsznpos", "Position:", choices=c("All","QB","RB","WR/TE","Defense")),
                                                     sliderInput("nflsznyr", "Season Year", 1967, 2022, value = c(1967, 2022),sep = ""),
                                                     radioButtons("nflsznradio", label = h3("Statistic Type"),
                                                                  choices = list("Flo Metrics" = 1, "Counting Stats" = 2, "Per Game Stats" = 3), 
                                                                  selected = 1),
                                                     hr(),
                                                   ),
                                                   mainPanel(DT::dataTableOutput("NFLSeasons"))
                                          ),
                                          tabPanel("Careers",
                                                   sidebarPanel(
                                                     selectizeInput("nflcarplayer", "Name:", choices=c("All",unique(nflnames))),
                                                     selectizeInput("nflcarpos", "Position:", choices=c("All","QB","RB","WR/TE","Defense")),
                                                     radioButtons("nflcarradio", label = h3("Statistic Type"),
                                                                  choices = list("Flo Metrics" = 1, "Counting Stats" = 2, "Per Game Stats" = 3), 
                                                                  selected = 1),
                                                     hr(),
                                                   ),
                                                   mainPanel(DT::dataTableOutput("NFLCareers"))
                                          ),
                                          
                                        )
                               ),
                               tabPanel("Explore",
                                        tabsetPanel(
                                          tabPanel("Compare Players",
                                                   sidebarPanel(
                                                     selectizeInput("nflcomppos", "Position:", choices = c("QB","RB","WR/TE","Defense")),
                                                     selectizeInput("nflcomp1", "Player 1:", choices = c("Tom Brady",unique(nflnames1))),
                                                     selectizeInput("nflcomp2", "Player 2:", choices = c("Aaron Rodgers",unique(nflnames1))),
                                                     radioButtons("nflcompradio", label = h3("Comparison Type"),
                                                                  choices = list("Career" = 1, "By Age" = 2, "By Experiance" = 3), 
                                                                  selected = 1),
                                                     hr()),
                                                   mainPanel(plotlyOutput("NFLCompPlot"))
                                          ),
                                          tabPanel("Impact of Age",
                                                   sidebarPanel(
                                                     selectizeInput("nflagepos", "Position:", choices=c("All","QB","RB","WR/TE","Defense")),
                                                     hr()),
                                                   mainPanel(plotlyOutput("NFLAgePlot"))
                                          ),
                                          tabPanel("Impact of Year",
                                                   sidebarPanel(
                                                     selectizeInput("nflyrpos", "Position:", choices=c("All","QB","RB","WR/TE","Defense")),
                                                     hr()),
                                                   mainPanel(plotlyOutput("NFLYrPlot"))
                                          ),
                                          tabPanel("League Trends",
                                                   sidebarPanel(
                                                     radioButtons("nfltrendstat", label = h3("Statistic"),
                                                                  choices = list("Points" = 1, "Competetiveness" = 2, "Play Type" = 3), 
                                                                  selected = 1),
                                                     hr()),
                                                   mainPanel(plotlyOutput("NFLTrendPlot"))
                                          ),
                                          tabPanel("Correlations",
                                                   sidebarPanel(
                                                     selectizeInput("nflvar1", "X Variable:", choices = c("FloStrength","TeamFS","SchedFS","PassingO","RushingO","PSPG","PassingD","RushingD","PAPG","Wpct")),
                                                     selectizeInput("nflvar2", "Y Variable:", choices = c("Wpct","FloStrength","TeamFS","SchedFS","PassingO","RushingO","PSPG","PassingD","RushingD","PAPG")),
                                                     hr()),
                                                   mainPanel(textOutput("corrNFL"),
                                                             plotlyOutput("corrNFLPlot"))
                                          )
                                        )
                               )
                             ))
                           
                         )),
                 #############NHL##############
                 
                 tabItem("tab_nhlcurr",
                         fluidRow(
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Teams",
                                        DT::dataTableOutput("NHLTeams")),
                               tabPanel("Players",
                                        DT::dataTableOutput("NHLPlayers")
                               ))
                           )
                         )
                 ),
                 tabItem("tab_nhlhist",
                         fluidRow(
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Teams",
                                        sidebarLayout(      
                                          sidebarPanel(
                                            selectizeInput("nhlteam", "Team:", choices=c("All",unique(nhldf$Team))),
                                            sliderInput("yearnhlteam", "Season Year", 1968, 2022, value = c(1968, 2022),sep = ""),
                                            hr()),
                                          mainPanel(DT::dataTableOutput("NHLTeamsAT")))),
                               tabPanel("Players",
                                        tabsetPanel(
                                          tabPanel("Careers",
                                                   sidebarLayout(      
                                                     sidebarPanel(
                                                       selectizeInput("nhlcarname", "Name:", choices=c("All",nhlnames)),
                                                       selectizeInput("nhlcarpos", "Position:", choices=c("All","C","W","D","G")),
                                                       radioButtons("nhlcarstat", label = h3("Statistic Type"),
                                                                    choices = list("FloMetrics"=1,"Totals" = 2,"By Game"=3), 
                                                                    selected = 1),
                                                       hr()),
                                                     mainPanel(DT::dataTableOutput("NHLCareers")))),
                                          tabPanel("Seasons",
                                                   sidebarLayout(      
                                                     sidebarPanel(
                                                       selectizeInput("nhlsznname", "Name:", choices=c("All",nhlnames)),
                                                       selectizeInput("nhlsznteam", "Team:", choices=c("All",unique(nhldf$Team))),
                                                       selectizeInput("nhlsznpos", "Position:", choices=c("All","C","W","D","G")),
                                                       sliderInput("yearnhlszn", "Season Year", 1968, 2022, value = c(1968, 2022),sep = ""),
                                                       radioButtons("nhlsznstat", label = h3("Statistic Type"),
                                                                    choices = list("FloMetrics"=1,"Totals" = 2,"By Game"=3), 
                                                                    selected = 1),
                                                       hr()),
                                                     mainPanel(DT::dataTableOutput("NHLSeasons"))))
                                        )
                               ),
                               tabPanel("Explore",
                                 tabsetPanel(
                                   tabPanel("Compare Players",
                                            sidebarLayout(      
                                              sidebarPanel(
                                                selectizeInput("nhlcomp1", "Player 1:", choices=c("Connor McDavid",nhlnames1)),
                                                selectizeInput("nhlcomp2", "Player 2:", choices=c("Auston Matthews",nhlnames1)),
                                                radioButtons("nhlcompradio", label = h3("Statistic Type"),
                                                             choices = list("Career"=1,"By Age" = 2,"By Experiance"=3), 
                                                             selected = 1),
                                                hr()),
                                              mainPanel(plotlyOutput("NHLCompPlot")))
                                   ),
                                   tabPanel("Impact of Age",
                                            sidebarLayout(      
                                              sidebarPanel(
                                                selectizeInput("nhlagepos", "Position:", choices=c("All","C","W","D","G")),
                                                radioButtons("nhlagestat", label = h3("Statistic Type"),
                                                             choices = list("Value"=1,"FloStrength" = 2), 
                                                             selected = 1),
                                                hr()),
                                              mainPanel(plotlyOutput("NHLAgePlot")))
                                   ),
                                   tabPanel("Impact of Year",
                                            sidebarLayout(      
                                              sidebarPanel(
                                                selectizeInput("nhlyearpos", "Position:", choices=c("All","C","W","D","G")),
                                                radioButtons("nhlyearstat", label = h3("Statistic Type"),
                                                             choices = list("Value"=1,"FloStrength" = 2), 
                                                             selected = 1),
                                                hr()),
                                              mainPanel(plotlyOutput("NHLYrPlot")))
                                            
                                   ),
                                   tabPanel("Trends",
                                            sidebarLayout(
                                              sidebarPanel(
                                                radioButtons("nhlcompyrstat", label = h3("Stat"),
                                                             choices = list("Goals Per Game"=1,"Competitiveness" = 2), 
                                                             selected = 1)),
                                              mainPanel(plotlyOutput("nhlyearcompplot"))
                                            )
                                            
                                   ),
                                   tabPanel("Correlations",
                                            selectizeInput("nhlvar1", "Variable X:", choices=c("FloStrength","WinPct","TeamScore","PlayWL","SchedFS","Offense","Defense","Discapline","GS","GA")),
                                            selectizeInput("nhlvar2", "Variable Y:", choices=c("WinPct","FloStrength","TeamScore","PlayWL","SchedFS","Offense","Defense","Discapline","GS","GA")),
                                            textOutput("NHLCorr"),
                                            mainPanel(plotlyOutput("NHLCorrPlot"))
                                   )
                                 )
                               )
                               
                             )
                           )
                         )
                 ),
                 #############NCAAMB##############
                 
                 tabItem("tab_ncaambcurr",
                         fluidRow(
                           mainPanel(
                             DT::dataTableOutput("NCAAMBTeams")
                           )
                         )
                 ),
                 tabItem("tab_ncaambhist",
                         fluidRow(
                           mainPanel(
                             DT::dataTableOutput("NCAAMBHistory")
                           )
                         )),
                 #############MLB##############
                 
                 tabItem("tab_mlbhist",
                         fluidRow(
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Teams",
                                        DT::dataTableOutput("MLBTeamsAT")
                               ),
                               tabPanel("Players",
                                        tabsetPanel(
                                          tabPanel("Careers",
                                                   tabsetPanel(
                                                     tabPanel("All",
                                                              DT::dataTableOutput("MLBHOF")
                                                     ),
                                                     tabPanel("Hitters",
                                                              DT::dataTableOutput("MLBBattersCar")
                                                     ),
                                                     tabPanel("Pitchers",
                                                              DT::dataTableOutput("MLBPitchersCar"))
                                                   )),
                                          tabPanel("Seasons",
                                                   tabsetPanel(
                                                     tabPanel("Hitters",
                                                              sidebarPanel(selectizeInput("mlbhit", "Name:", choices=c("All",unique(mlbhitAT$Name))),
                                                                           selectizeInput("mlbhitteam", "Team:", choices=c("All",unique(mlbhitAT$teamID))),
                                                                           selectizeInput("mlbhitpos", "Position:", choices=c("All",unique(mlbhitAT$POS))),
                                                                           selectizeInput("mlbhityrsing", "Single Year:", choices=c("All",unique(mlbhitAT$yearID))),
                                                                           sliderInput("mlbhityr", "Years", 1903, 2022, value = c(1903, 2022),sep = ""),width =3),
                                                              mainPanel(DT::dataTableOutput("MLBBatters"))
                                                     ),
                                                     tabPanel("Pitchers",
                                                              sidebarPanel(
                                                                selectizeInput("mlbpitcher", "Name:", choices=c("All",unique(mlbpitAT$Name))),
                                                                selectizeInput("mlbpitchteam", "Team:", choices=c("All",unique(mlbpitAT$teamID))),
                                                                selectizeInput("mlbpitchyrsing", "Single Year:", choices=c("All",unique(mlbpitAT$yearID))),
                                                                sliderInput("mlbpitchyr", "Years", 1903, 2022, value = c(1903, 2022),sep = ""),width = 3),
                                                              mainPanel(DT::dataTableOutput("MLBPitchers"))
                                                     )
                                                   )),
                                          
                                          
                                        )
                               ),
                               tabPanel("Explore",
                                        tabsetPanel(
                                          tabPanel("Compare Hitters",
                                                   sidebarPanel(selectizeInput("mlbcompvar", "By:", choices=c("Age","Year","Experiance")),
                                                                selectizeInput("mlbcompp1", "Player 1:", choices=c("Babe Ruth",mlbhitname)),
                                                                selectizeInput("mlbcompp2", "Player 2:", choices=c("Albert Pujols",mlbhitname))),
                                                   mainPanel(plotlyOutput("MLBPlotCompHitter")),
                                          ),
                                          tabPanel("Compare Pitchers",
                                                   sidebarPanel(selectizeInput("mlbcompvarp", "By:", choices=c("Age","Year","Experiance")),
                                                                selectizeInput("mlbcomppit1", "Player 1:", choices=c("Nolan Ryan",mlbpitchname)),
                                                                selectizeInput("mlbcomppit2", "Player 2:", choices=c("Pedro Martinez",mlbpitchname))),
                                                   mainPanel(plotlyOutput("MLBPlotCompPitcher"))),
                                          tabPanel("Impact of Age",
                                                   selectizeInput("mlbagefilt", "Position:", choices=c("All","P","C","1B","IF","OF")),
                                                   mainPanel(plotlyOutput("MLBPlotAge"))),
                                          tabPanel("Impact of Year",
                                                   selectizeInput("mlbyrfilt", "Position:", choices=c("All","P","C","1B","IF","OF")),
                                                   mainPanel(plotlyOutput("MLBPlotYear"))),
                                          tabPanel("Compare Years",
                                                   selectizeInput("mlberavar", "Variable:", choices=c("Runs","Competetiveness","HR","SO","Hits")),
                                                   mainPanel(plotlyOutput("MLBPlotEras"))),
                                          tabPanel("Correlations",
                                                   selectizeInput("mlbcorrvar1", "Variable X:", choices=c("FloStrength","Wpct","FSPlay","FSTeam","R","RA","HR","SO","H")),
                                                   selectizeInput("mlbcorrvar2", "Variable Y:", choices=c("Wpct","FloStrength","FSPlay","FSTeam","R","RA","HR","SO","H")),
                                                   textOutput("MLBCorr"),
                                                   mainPanel(plotlyOutput("MLBPlotCorr")))
                                        ))))
                         )),
                 tabItem("tab_mlbpred",
                         fluidRow(
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Team",
                                        DT::dataTableOutput("mlbteamproj")
                               ),
                               tabPanel("Player",
                                        tabsetPanel(
                                          tabPanel("Hitters",
                                                   DT::dataTableOutput("mlbhitterproj")
                                          ),
                                          tabPanel("Pitchers",
                                                   DT::dataTableOutput("mlbpitcherproj"))
                                        )
                                 
                               )
                             )
                           )
                         ))
               )))))

############Server############
server <- function(input, output) {
  output$MVPAllSports <- DT::renderDataTable({
    if(input$mvpsport=="NFL"){qb <- read.csv("NFLQBRank") %>%
      select(Name,Team,Value)
    rb <- read.csv("NFLRBRank") %>%
      select(Name,Team,Value)
    wr <- read.csv("NFLWRRank") %>%
      select(Name,Team,Value)
    def <- read.csv("NFLDefRank") %>%
      select(Name,Team,Value)
    df <- rbind(qb,rb,wr,def) %>%
      arrange(-Value) %>%
      slice(1:10)    
    }else{
      if(input$mvpsport=="NBA"){
        df <-read.csv("NBAMVPRank")[1:10,]
        df<-df %>% select(Player,Tm,Value)
      }else{
        df <-read.csv("NHLMVP")[1:10,]
        df<- df %>%
          select(-FloStrength,-Pos)
      }
  }
    DT::datatable(df)
  })
  output$PRAllSports <- DT::renderDataTable({
    if(input$prsport=="NFL"){
      df <-read.csv("NFLPowerRank")[1:10,]
      df<- df %>% select(Team,Rating)%>% mutate(Rating = round(Rating,3))
    }else{
      if(input$prsport=="NBA"){
        df <-read.csv("NBAPowerRank")[1:10,]
        df<- df %>% select(Team,Rating)
      }else{
        df <-read.csv("NHLPowerRank")[1:10,]
        df<- df %>% select(Team,"Rating"="FloStrength")
      }
    }
    DT::datatable(df)
  })
  output$TopAthletes<- DT::renderDataTable({
    qb<- read.csv("QBCareersforapp")%>%
      filter(GS>32)%>%
      mutate(Value = Value-min(Value))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(Pos = "NFL")%>%
      mutate(FloStrength = (RushFS*RushAtt+PassFS*Att)/(RushAtt+Att))%>%
      select(Name,Pos,Value,"FloStrength")%>%
      mutate(Value = (Value-mean(Value))/sd(Value))%>%
      mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = FloStrength /max(FloStrength))%>%
      mutate(Rating = (2*Value+1*FloStrength)/3)
    rb<- read.csv("RBCareersforapp")%>%
      filter(GS>32)%>%
      mutate(Value = Value-min(Value))%>%
      mutate(Value = Value /max(Value))%>%
      filter(Name!="Deebo Samuel")%>%
      mutate(Pos = "NFL")%>%
      mutate(FloStrength = (RecFS*Rec+RushFS*Att)/(Rec+Att))%>%
      select(Name,Pos,Value,"FloStrength")%>%
      mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
      mutate(Value = (Value-mean(Value))/sd(Value))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = FloStrength /max(FloStrength))%>%
      mutate(Rating = (2*Value+1*FloStrength)/3)
    wr<-read.csv("WRCareersforapp")%>%
      filter(GS>32)%>%
      mutate(Value = Value-min(Value))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(Pos = "NFL")%>%
      select(Name,Pos,Value,"FloStrength")%>%
      mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
      mutate(Value = (Value-mean(Value))/sd(Value))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = FloStrength /max(FloStrength))%>%
      mutate(Rating = (2*Value+1*FloStrength)/3)
    def<- read.csv("DEFCareersforapp") %>%
      filter(G>32)%>%
      mutate(Value = Value-min(Value))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(Pos = "NFL")%>%
      select(Name,Pos,Value,"FloStrength"="DefFS")%>%
      mutate(Value = (Value-mean(Value))/sd(Value))%>%
      mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = FloStrength /max(FloStrength))%>%
      mutate(Rating = (2*Value+1*FloStrength)/3)
    nbadf <- read.csv("NBACareersforapp")%>%
      filter(G>250&RookieYr<2018)%>%
      mutate(Value = Value-min(Value))%>%
      mutate(Value = Value /max(Value))%>%
      select(Name,Pos,Value,"FloStrength")%>%
      mutate(Value = (Value-mean(Value))/sd(Value))%>%
      mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = FloStrength /max(FloStrength))%>%
      mutate(Rating = (2*Value+1*FloStrength)/3)%>%
      mutate(Pos="NBA")
    bat <-read.csv("MLBBattersCarforapp")%>%
      filter(G>400)%>%
      mutate(Value = FloValue)%>%
      mutate(Value = Value-min(Value))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = .1*DefFS+.9*OffFS)%>%
      select(Name,"Value","FloStrength")%>%
      mutate(Value = (Value-mean(Value))/sd(Value))%>%
      mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = FloStrength /max(FloStrength))%>%
      mutate(Rating = (2*Value+1*FloStrength)/3)%>%
      mutate(Pos="MLB")
    pit <-read.csv("MLBPitchersCarforapp")%>%
      filter(IP>400)%>%
      mutate(Value = FloValue)%>%
      mutate(Value = Value-min(Value))%>%
      mutate(Value = Value /max(Value))%>%
      select(Name,"Value","FloStrength")%>%
      na.omit()%>%
      mutate(Value = (Value-mean(Value))/sd(Value))%>%
      mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = FloStrength /max(FloStrength))%>%
      mutate(Rating = (2*Value+1*FloStrength)/3)%>%
      mutate(Pos="MLB")

    hock <-read.csv("NHLPlayerCarATforapp")%>%
      filter(GP>250)%>%
      mutate(Value = Value-min(Value))%>%
      mutate(Value = Value /max(Value))%>%
      arrange(-Value)%>%
      na.omit()%>%
      mutate(Value = (Value-mean(Value))/sd(Value))%>%
      mutate(FloStrength = (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
      mutate(Value = Value /max(Value))%>%
      mutate(FloStrength = FloStrength /max(FloStrength))%>%
      mutate(Rating = (2*Value+1*FloStrength)/3)%>%
      select("Name"="Player",Rating,"Value","FloStrength",Pos)%>%
      mutate(Pos="NHL")
      
    at<- rbind(qb,rb,wr,def,nbadf,bat,pit,hock)%>%
      arrange(-Rating)%>%
      group_by(Pos)%>%
      ungroup()%>%
      slice(1:500)%>%
      select(Name, Pos,Rating)%>%
      mutate(Rating = round(Rating,2))
    DT::datatable(at,options = list(scrollX = TRUE))
  })
  output$TopTeams<-DT::renderDataTable({
    nbadf <- read.csv("NBATeamsATforapp")%>%
      select(Team,yearID,POSuc, FloStrength)%>%
      mutate(Sport = "NBA")%>%
      mutate(POSuc = ifelse(POSuc==4,1,0))
    nflteams<- read.csv("NFLTeamsATforapp")%>%
      select("Team"=Tm,yearID,"POSuc"=PO, FloStrength)%>%
      mutate(POSuc = ifelse(POSuc==5,1,0))%>%
      mutate(Sport = "NFL")
    df <-read.csv("NHLTeamsATforapp")%>%
      select(Team,yearID,"POSuc"=SCChamp,FloStrength)%>%
      mutate(Sport = "NHL")
    df1 <-read.csv("MLBTeamsATforapp")%>%
      select("Team"=teamID,yearID,"POSuc"="WSWin", FloStrength)%>%
      mutate(Sport = "MLB")
    df<-rbind(nbadf,nflteams,df,df1)%>%
      group_by(Sport)%>%
      mutate(Ranking = (FloStrength -mean(FloStrength))/sd(FloStrength))%>%
      ungroup()%>%
      mutate(Ranking = ifelse(POSuc==1,Ranking+1,Ranking))%>%
      mutate(Ranking= round((4+Ranking)/8,3))%>%
      rename("Champion"="POSuc")%>%
      select(-FloStrength)%>%
      arrange(-Ranking)%>%
      slice(1:100)
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  ################################################
  ################################################
  ##############    Schedule
  ################################################
  ################################################
  
  output$SchedToday <- DT::renderDataTable({
    df <- read.csv("TodSched")
    DT::datatable(df)
  })
  output$YestResults <- DT::renderDataTable({
    df <- read.csv("YestResults")
    DT::datatable(df)
  })
  
  ################################################
  ################################################
  ##############    Modelling
  ################################################
  ################################################
  
  output$ModelRes <- renderPlotly({
    if (input$modsport=="NBA"){
      df <- read.csv("NBAModOvrSuc")
    }else{
      if(input$modsport=="NHL"){
        df <- read.csv("NHLModOvrSuc") 
      }else{
        if(input$modsport=="NCAAMB"){
          df <- read.csv("NCAAMBModOvrSuc") 
        }else{
          if(input$modsport=="NFL"){
            
          }}}}
    if(input$modtype=="Log"){
      df<-df%>%
        filter(Model == "Book" | Model == "Log")
    }
    if(input$modtype=="SVM"){
      df<-df%>%
        filter(Model == "SVM" | Model == "Book")
    }
    if(input$modtype=="RF"){
      df<-df%>%
        filter(Model == "RF" | Model == "Book")
    }
    if(input$modtype=="NN"){
      df<-df%>%
        filter(Model == "NN" | Model == "Book")
    }
    if(input$modtype=="GBM"){
      df<-df%>%
        filter(Model == "GBM" | Model == "Book")
    }
    if(input$modtype=="Ens"){
      df<-df%>%
        filter(Model == "Ens" | Model == "Book")
    }
    if(input$modtype=="E3"){
      df<-df%>%
        filter(Model == "E3" | Model == "Book")
    }
    plot_ly(df, x = ~Day, y = ~Corr, type = 'scatter', color = ~Model)%>%
      layout(yaxis= list(range = c(.5,.8)))
  })
  output$ModelResDF <- renderTable({
    if (input$modsport=="NBA"){
      df <- read.csv("NBAModSucDF")
    }else{
      if(input$modsport=="NHL"){
        df <- read.csv("NHLModSucDF") 
      }else{
        if(input$modsport=="NCAAMB"){
          df <- read.csv("NCAAMBModSucDF") 
        }else{
          if(input$modsport=="NFL"){
          }}}}
    if(input$modtype=="Log"){
      df<-df[,c(1,8)]
    }
    if(input$modtype=="SVM"){
      df<-df[,c(2,8)]
    }
    if(input$modtype=="RF"){
      df<-df[,c(3,8)]
    }
    if(input$modtype=="NN"){
      df<-df[,c(4,8)]
    }
    if(input$modtype=="GBM"){
      df<-df[,c(5,8)]
    }
    if(input$modtype=="Ens"){
      df<-df[,c(7,8)]
    }
    if(input$modtype=="E3"){
      df<-df[,c(6,8)]
    }
    rownames(df)<- c("Accuracy","F1Score","Specificity","Sensitivity","SampSize")
    df
  },rownames = TRUE)
  ################################################
  ################################################
  ##############    Betting
  ################################################
  
  output$BetsToday <- DT::renderDataTable({
    df <- read.csv("todaybets")
    if(input$todbetsports=="NBA"){
      df <- df %>% filter(Sport=="NBA")
    }
    if(input$todbetsports=="NCAAMB"){
      df <- df %>% filter(Sport=="NCAAMB")
    }
    if(input$todbetsports=="NHL"){
      df <- df %>% filter(Sport=="NHL")
    }else{
      df<- df
    }
    DT::datatable(df)
  })
  output$BetsHistory <- DT::renderDataTable({
    getyestDate <- function(x){
      x <- Sys.Date() -1
      x <- str_remove_all(x, "-")
      return(x)
    }
    getlastWeek <- function(x){
      x <- Sys.Date() -8
      x <- str_remove_all(x, "-")
      return(x)
    }
    getlastMonth <- function(x){
      x <- Sys.Date() -31
      x <- str_remove_all(x, "-")
      return(x)
    }
    getlastYear <- function(x){
      x <- Sys.Date() -366
      x <- str_remove_all(x, "-")
      return(x)
    }
    df<-read.csv("betsdata") %>%
      mutate(Earn = round(Earn,2))
    if(input$bettimeframe=="Yesterday"&input$betsports=="All"){df <- df %>% filter(Day == getyestDate())}
    if(input$bettimeframe=="Yesterday"&input$betsports=="NBA"){df <- df %>%filter(Day == getyestDate()& Sport == "NBA")}
    if(input$bettimeframe=="Yesterday"&input$betsports=="NHL"){df <- df %>%filter(Day == getyestDate()& Sport == "NHL")}
    if(input$bettimeframe=="Yesterday"&input$betsports=="NCAAMB"){df <- df %>% filter(Day == getyestDate()& Sport == "NCAAMB")}
    if (input$bettimeframe=="Last Week"&input$betsports=="All"){df <- df %>%filter(Day > getlastWeek())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup() %>% mutate(Sport = "All")}
    if(input$bettimeframe=="Last Week"&input$betsports=="NBA"){df <- df %>%filter(Day > getlastWeek()& Sport == "NBA")}
    if(input$bettimeframe=="Last Week"&input$betsports=="NHL"){df <- df %>%filter(Day > getlastWeek()& Sport == "NHL")}
    if(input$bettimeframe=="Last Week"&input$betsports=="NCAAMB"){df <- df %>% filter(Day > getlastWeek()& Sport == "NCAAMB")}
    if (input$bettimeframe=="Last Month"&input$betsports=="All"){df <- df %>%filter(Day > getlastMonth())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup() %>% mutate(Sport = "All")}
    if(input$bettimeframe=="Last Month"&input$betsports=="NBA"){df <- df %>%filter(Day > getlastMonth()& Sport == "NBA")}
    if(input$bettimeframe=="Last Month"&input$betsports=="NHL"){df <- df %>%filter(Day > getlastMonth()& Sport == "NHL")}
    if(input$bettimeframe=="Last Month"&input$betsports=="NCAAMB"){df <- df %>% filter(Day > getlastMonth()& Sport == "NCAAMB")}
    if (input$bettimeframe=="Last Year"&input$betsports=="All"){df <- df %>%filter(Day > getlastYear())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup() %>% mutate(Sport = "All")}
    if(input$bettimeframe=="Last Year"&input$betsports=="NBA"){df <- df %>%filter(Day > getlastYear()& Sport == "NBA")}
    if(input$bettimeframe=="Last Year"&input$betsports=="NHL"){df <- df %>%filter(Day > getlastYear()& Sport == "NHL")}
    if(input$bettimeframe=="Last Year"&input$betsports=="NCAAMB"){df <- df %>% filter(Day > getlastYear()& Sport == "NCAAMB")}
    if (input$bettimeframe=="All Time"&input$betsports=="All"){df <- df%>%filter(Day > getlastMonth())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup() %>% mutate(Sport = "All")}
    if(input$bettimeframe=="All Time"&input$betsports=="NBA"){df <- df %>%filter(Sport == "NBA")}
    if(input$bettimeframe=="All Time"&input$betsports=="NHL"){df <- df %>%filter(Sport == "NHL")}
    if(input$bettimeframe=="All Time"&input$betsports=="NCAAMB"){df <- df %>% filter(Sport == "NCAAMB")}
    DT::datatable(df)
  })
  output$BetPlot <- renderPlotly({
    getyestDate <- function(x){
      x <- Sys.Date() -1
      x <- str_remove_all(x, "-")
      return(x)
    }
    getlastWeek <- function(x){
      x <- Sys.Date() -8
      x <- str_remove_all(x, "-")
      return(x)
    }
    getlastMonth <- function(x){
      x <- Sys.Date() -31
      x <- str_remove_all(x, "-")
      return(x)
    }
    getlastYear <- function(x){
      x <- Sys.Date() -366
      x <- str_remove_all(x, "-")
      return(x)
    }
    if(input$betview == "Net Daily"|input$bettimeframe=="Yesterday"){
      df <- read.csv("betsdata")%>%
        mutate(Day=as.character(Day)) %>%
        mutate(Earn = round(Earn,2))
      tf <- input$bettimeframe
      sport <- input$betsports
      yestDate<- getyestDate()
      if(input$bettimeframe=="Yesterday"&input$betsports=="All"){df <- df %>% filter(Day == getyestDate())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup()%>% mutate(Net = ifelse(Earn>0,"pos","neg"))}
      if(input$bettimeframe=="Yesterday"&input$betsports=="NBA"){df <- df %>%filter(Day == getyestDate()& Sport == "NBA")%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup()%>% mutate(Net = ifelse(Earn>0,"pos","neg"))}
      if(input$bettimeframe=="Yesterday"&input$betsports=="NHL"){df <- df %>%filter(Day == getyestDate()& Sport == "NHL")%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup()%>% mutate(Net = ifelse(Earn>0,"pos","neg"))}
      if(input$bettimeframe=="Yesterday"&input$betsports=="NCAAMB"){df <- df %>% filter(Day == getyestDate()& Sport == "NCAAMB")%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup()%>% mutate(Net = ifelse(Earn>0,"pos","neg"))}
      if (input$bettimeframe=="Last Week"&input$betsports=="All"){df <- df %>%filter(Day > getlastWeek())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup() %>% mutate(Sport = "All")%>% mutate(Net = ifelse(Earn>0,"pos","neg"))}
      if(input$bettimeframe=="Last Week"&input$betsports=="NBA"){df <- df %>%filter(Day > getlastWeek()& Sport == "NBA")}
      if(input$bettimeframe=="Last Week"&input$betsports=="NHL"){df <- df %>%filter(Day > getlastWeek()& Sport == "NHL")}
      if(input$bettimeframe=="Last Week"&input$betsports=="NCAAMB"){df <- df %>% filter(Day > getlastWeek()& Sport == "NCAAMB")}
      if (input$bettimeframe=="Last Month"&input$betsports=="All"){df <- df %>%filter(Day > getlastMonth())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup() %>% mutate(Sport = "All")%>% mutate(Net = ifelse(Earn>0,"pos","neg"))}
      if(input$bettimeframe=="Last Month"&input$betsports=="NBA"){df <- df %>%filter(Day > getlastMonth()& Sport == "NBA")}
      if(input$bettimeframe=="Last Month"&input$betsports=="NHL"){df <- df %>%filter(Day > getlastMonth()& Sport == "NHL")}
      if(input$bettimeframe=="Last Month"&input$betsports=="NCAAMB"){df <- df %>% filter(Day > getlastMonth()& Sport == "NCAAMB")}
      if (input$bettimeframe=="Last Year"&input$betsports=="All"){df <- df %>%filter(Day > getlastYear())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup() %>% mutate(Sport = "All")%>% mutate(Net = ifelse(Earn>0,"pos","neg"))}
      if(input$bettimeframe=="Last Year"&input$betsports=="NBA"){df <- df %>%filter(Day > getlastYear()& Sport == "NBA")}
      if(input$bettimeframe=="Last Year"&input$betsports=="NHL"){df <- df %>%filter(Day > getlastYear()& Sport == "NHL")}
      if(input$bettimeframe=="Last Year"&input$betsports=="NCAAMB"){df <- df %>% filter(Day > getlastYear()& Sport == "NCAAMB")}
      if (input$bettimeframe=="All Time"&input$betsports=="All"){df <- df%>%filter(Day > getlastMonth())%>% group_by(Day)%>% mutate(Earn = round(sum(Earn),2)) %>% slice(1)%>% ungroup() %>% mutate(Sport = "All") %>% mutate(Net = ifelse(Earn>0,"pos","neg"))}
      if(input$bettimeframe=="All Time"&input$betsports=="NBA"){df <- df %>%filter(Sport == "NBA")}
      if(input$bettimeframe=="All Time"&input$betsports=="NHL"){df <- df %>%filter(Sport == "NHL")}
      if(input$bettimeframe=="All Time"&input$betsports=="NCAAMB"){df <- df %>% filter(Sport == "NCAAMB")}
      plot_ly(df, x = ~Day, y = ~Earn, type = 'bar',color = ~Net,colors = c("pos"="green", "neg"="red"))%>%
        layout(yaxis= list(range = c(-15,15)))
    } else{
      df <- read.csv("betsdata")
      if (input$betsports=="NCAAMB"){df <- df %>%filter(Sport == "NCAAMB")}
      if (input$betsports=="NBA"){df <- df %>%filter(Sport == "NBA")}
      if (input$betsports=="NHL"){df <- df %>%filter(Sport == "NHL")}
      df <- df %>%
        rename("Date"="Day") %>%
        group_by(Date) %>%
        mutate(Daily = sum(Earn)) %>%
        slice(1) %>%
        ungroup()
      days <- sort(as.numeric(unique(df$Date)))
      if (input$bettimeframe=="Last Week"){df <- df %>%slice((length(days)-6):length(days))}
      if (input$bettimeframe=="Last Month"& length(days>29)){df <- df %>%slice((length(days)-29):length(days))}
      if (input$bettimeframe=="Last Year"& length(days>364)){df <- df%>%slice((length(days)-364):length(days))}
      day1 <- min(df$Date)
      df1 <- df %>%
        filter(Date == day1) %>%
        mutate(Day = 1) %>%
        mutate(Total = Earn) %>%
        select(Day, Total,Daily)
      days <- sort(as.numeric(unique(df$Date)))
      for (i in c(2:length(days))){
        df2 <- df %>%
          filter(Date <= days[i]) %>%
          mutate(Day = i) %>%
          mutate(Total = sum(Daily)) %>%
          select(Day, Total,Daily) %>%
          slice(i)
        df1 <- rbind(df1, df2)
      }
      df <- df1 %>%
        mutate(Net = ifelse(Total > 0,"pos","neg"))
      
      plot_ly(df, x = ~Day, y = ~Total, type = 'bar',color = ~Net,colors = c("pos"="green", "neg"="red"),
              text = ~paste('Daily: ', Daily))%>%
        layout(yaxis= list(range = c(-5,(max(df$Total)+5))))
    }
  })
  
  
  ################################################
  ##############    NBA
  ################################################
  
  output$NBAMVPRank <- DT::renderDataTable({
    df <-read.csv("NBAMVPRank")
    DT::datatable(df,filter = "top",options = list(scrollX = TRUE))
  })
  output$NBAPowerRank <- DT::renderDataTable({
    df <-read.csv("NBAPowerRank")
    DT::datatable(df,filter = "top",options = list(scrollX = TRUE))
  })
  output$seasonsNBA <- DT::renderDataTable({
    players <- read.csv("NBAPlayersATforapp")%>%
      filter(yearID >= input$year[1]) %>%
      filter(yearID <= input$year[2])%>%
      mutate(FloStrength = round(FloStrength,3))%>%
      mutate(Rebounding = round(Rebounding,3))%>%
      mutate(Passing = round(Passing,3))%>%
      mutate(Defense = round(Defense,3))%>%
      mutate(Scoring = round(Scoring,3))%>%
      mutate(Value = round(Value,2))
    if (input$team != "All"){
      players<- players%>%
        filter(Tm == input$team) 
    }
    if (input$nbaplayerszn!= "All"){
      players <- players %>%
        filter(Name == input$nbaplayerszn)
    }
    if (input$nbasznpos!= "All"){
      players <- players %>%
        filter(Pos == input$nbasznpos)
    }
    if (input$nbasznradio==1){
      players<- players %>% 
        select(-G,-PTS,-TRB,-AST,-STL,-BLK)
    } 
    if (input$nbasznradio==2){
      players<- players %>% 
        select(-FloStrength,-Defense,-Passing,-Rebounding,-Scoring)
    } 
    if (input$nbasznradio==3){
      players<- players %>% 
        select(-FloStrength,-Defense,-Passing,-Rebounding,-Scoring) %>%
        mutate(PTS = round(PTS/G,1))%>%
        mutate(TRB = round(TRB/G,1))%>%
        mutate(AST = round(AST/G,1))%>%
        mutate(STL = round(STL/G,1))%>%
        mutate(BLK = round(BLK/G,1))
    } 
    DT::datatable(players,options = list(scrollX = TRUE))
  })
  output$CompCarNBAPlot <- renderPlotly({
    if (input$nbacomptype==1){
      df <- read.csv("NBACareersforapp")%>%
        filter(Name == input$nbacomp1 | Name==input$nbacomp2)
      if(input$nbacompcartype ==1){
        plot_ly(df, x = ~Name, y = ~Value, type = 'bar',color = ~Name,colors = c("blue","red"))
      }else{
        if(input$nbacompcartype ==2){
          plot_ly(df, x = ~Name, y = ~FloStrength, type = 'bar', color = ~Name,colors = c("blue","red"))
        }else{
          if(input$nbacompcartype ==3){
            plot_ly(df, x = ~Name, y = ~PTS, type = 'bar', color = ~Name,colors = c("blue","red"))
          }
        }
      }
    }else{
      df <- read.csv("NBAPlayersATforapp") %>%
        filter(Name == input$nbacomp1 | Name==input$nbacomp2) %>%
        group_by(Name) %>%
        mutate(exp = c(1:length(Value))) %>%
        ungroup()
      if(input$nbacomptype==2){
        if(input$nbacompcartype ==1){
          plot_ly(df, x = ~Age, y = ~Value, type = 'scatter', mode = "line", mode = 'markers', color = ~Name,colors = c("blue","red"))
        }else{
          if(input$nbacompcartype ==2){
            plot_ly(df, x = ~Age, y = ~FloStrength, type = 'scatter', mode = "line", mode = 'markers', color = ~Name,colors = c("blue","red"))
          }else{
            if(input$nbacompcartype ==3){
              plot_ly(df, x = ~Age, y = ~PTS, type = 'scatter', mode = "line", mode = 'markers', color = ~Name,colors = c("blue","red"))
            }
          }
        }
      }else{
        if(input$nbacompcartype ==1){
          plot_ly(df, x = ~exp, y = ~Value, type = 'scatter', mode = "line", mode = 'markers', color = ~Name,colors = c("blue","red"))
        }else{
          if(input$nbacompcartype ==2){
            plot_ly(df, x = ~exp, y = ~FloStrength, type = 'scatter', mode = "line", mode = 'markers', color = ~Name,colors = c("blue","red"))
          }else{
            if(input$nbacompcartype ==3){
              plot_ly(df, x = ~exp, y = ~PTS, type = 'scatter', mode = "line", mode = 'markers', color = ~Name,colors = c("blue","red"))
            }
          }
        }
        
      }}
  })
  output$nbaageplot <- renderPlotly({
    df <- read.csv("NBAPlayersATforapp")
    if(input$nbaagepos != "All"){
      df <- df %>%
        filter(Pos ==input$nbaagepos)
    }
    if(input$nbaagestat == 1){
      plot_ly(df, x = ~Age, y = ~Value, type = "box", text = ~paste('Name: ', Name))
    }else{
      plot_ly(df, x = ~Age, y = ~FloStrength, type = "box", text = ~paste('Name: ', Name))
    }
  })
  output$nbayearplot <- renderPlotly({
    df <- read.csv("NBAPlayersATforapp")
    if(input$nbayearpos != "All"){
      df <- df %>%
        filter(Pos ==input$nbayearpos)
    }
    if(input$nbayearstat == 1){
      plot_ly(df, x = ~yearID, y = ~Value, type = "box", text = ~paste('Name: ', Name))
    }else{
      plot_ly(df, x = ~yearID, y = ~FloStrength, type = "box", text = ~paste('Name: ', Name))
    }
  })
  output$nbayearcompplot<- renderPlotly({
    df <- read.csv("NBATeamsATforapp") %>%
      group_by(yearID) %>%
      mutate(Competitivness = 5.2*(.3-mean(sd(Wpct))))%>%
      mutate(PtsPG = mean(PTS))%>%
      mutate(Twos = mean(TwPA))%>%
      mutate(Threes = mean(ThPA))%>%
      slice(1) %>%
      ungroup()%>%
      mutate(TwoProp = Twos /(Twos+Threes))%>%
      mutate(ThreeProp = 1-TwoProp)
    if(input$nbacompyrstat==1){
      plot_ly(df, x = ~yearID, y = ~PtsPG, type = 'bar', color=~yearID)
    }else{
      if(input$nbacompyrstat==2){
        plot_ly(df, x = ~yearID, y = ~Competitivness, type = 'bar', color=~yearID)
      }else{
        df1 <- df %>%
          mutate(Prop = TwoProp)%>%
          mutate(Shot = "Twos") %>%
          select(yearID,Shot,Prop)
        df2 <- df %>%
          mutate(Prop = ThreeProp)%>%
          mutate(Shot = "Threes") %>%
          select(yearID,Shot,Prop)
        df <- rbind(df1,df2)
        plot_ly(df, x = ~yearID, y = ~Prop, type = 'scatter',mode="line", color=~Shot)
      }
    }
  })
  output$teamsNBA <- DT::renderDataTable({
    nbadf <- read.csv("NBATeamsATforapp") %>%
      filter(yearID >= input$yearnbateam[1]) %>%
      filter(yearID <= input$yearnbateam[2]) %>%
      mutate(Wpct = round(Wpct,3))%>%
      mutate(Scoring = round(Scoring,3))%>%
      mutate(Defense = round(Defense,3))%>%
      mutate(Passing = round(Passing,3))%>%
      mutate(Rebounding = round(Rebounding,3))%>%
      mutate(TeamFS = round(TeamFS,3))%>%
      mutate(SchedFS = round(SchedFS,3))%>%
      mutate(FloStrength = round(FloStrength,3))%>%
      mutate(PlayFS= (3+wpctplay)/6)%>%
      mutate(PlayFS = round(PlayFS,3)) %>%
      select(-wpctplay,-Passing1,-TwPA,-ThPA,-PTS)
    if (input$nbateam != "All"){
      nbadf<- nbadf%>%
        filter(Team == input$nbateam) 
    }
    DT::datatable(nbadf,options = list(scrollX = TRUE))
  })
  output$CarNBAPlot <- renderPlotly({
    nbadf <- read.csv("NBACareersforapp")
    if (input$nbaplaycar!= ""){
      nbadf <- nbadf %>%
        filter(Name == input$nbaplaycar)
    }
    plot_ly(nbadf, x = ~Seasons, y = ~Value, type = 'scatter', mode = 'markers', text = ~paste('Name: ', Name))%>%
      layout(xaxis= list(range = c(0,25)),
             yaxis= list(range = c(-50,400)))
  })
  output$careerNBA <- DT::renderDataTable({
    careers <- read.csv("NBACareersforapp")
    if (input$nbacarradio==1){
      careers<- careers %>% 
        select(-G,-PTS,-TRB,-AST,-STL,-BLK)
    } 
    if (input$nbacarradio==2){
      careers<- careers %>% 
        select(-FloStrength,-Defense,-Passing,-Rebounding,-Scoring)
    } 
    if (input$nbacarradio==3){
      careers<- careers %>% 
        select(-FloStrength,-Defense,-Passing,-Rebounding,-Scoring) %>%
        mutate(PTS = round(PTS/G,1))%>%
        mutate(TRB = round(TRB/G,1))%>%
        mutate(AST = round(AST/G,1))%>%
        mutate(STL = round(STL/G,1))%>%
        mutate(BLK = round(BLK/G,1))
    } 
    DT::datatable(careers,options = list(scrollX = TRUE))
  })
  output$corrNBAPlot <- renderPlotly({
    df <- read.csv("NBATeamsATforapp")
    fit <- lm(df[,input$nbavar2] ~ df[,input$nbavar1])
    plot_ly(df, x = df[,input$nbavar1], y=df[,input$nbavar2], type = 'scatter', mode = 'markers', text = ~paste('Team: ', str_c(Team,yearID, sep=", ")))%>% 
      add_lines(x = df[,input$nbavar1], y = fitted(fit)) %>%
      layout(showlegend = F)
  })
  output$corrNBA <- renderText({
    df <- read.csv("NBATeamsATforapp")
    str_c("Correlation = ",round(cor(df[,input$nbavar1], df[,input$nbavar2]),3))
  })
  ################################################
  ##############    NFL
  ################################################
  output$NFLSeasons<- DT::renderDataTable({
    qb<- read.csv("QBSeasonsforapp")
    rb<- read.csv("RBSeasonsforapp")
    wr<-read.csv("WRSeasonsforapp")
    def<- read.csv("DEFSeasonsforapp") 
    if(input$nflsznpos=="All"){
      qb<- qb %>%
        filter(GS >7&PassYds>2000) %>%
        select(Name,Pos,Age,Season,Tm, Value)
      rb<- rb %>%
        filter(GS >7& Att>120)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      wr<- wr %>%
        filter(GS >7& Rec>25)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      def<- def %>%
        filter(G >7& Solo >32)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      df<- rbind(qb,rb,wr,def)
    }else{
      if(input$nflsznpos=="QB"){
        df<- qb %>%
          filter(GS >7&PassYds>2000)
        if (input$nflsznradio==1){
          df<-df %>%
            mutate(Wpct = round(Wins/GS,3))%>%
            select(-Cmp,-Att,-CmpPer,-PassYds,-TD,-Int,-GWD,-RushAtt,-RushYds,-RushTD,-Fmb,-Wins,-Losses)
        }
        if (input$nflsznradio==2){
          df<-df%>%
            select(-PassFS,-PValue,-RushFS,-RValue)
        }
        if (input$nflsznradio==3){
          df<-df%>%
            mutate(Wpct = round(Wins/GS,3))%>%
            mutate(Cmp=Cmp/GS)%>%
            mutate(Att=Att/GS)%>%
            mutate(PassYds=PassYds/GS)%>%
            mutate(TD=TD/GS)%>%
            mutate(Int=Int/GS)%>%
            mutate(RushYds=RushYds/GS)%>%
            mutate(RushTD=RushTD/GS)%>%
            select(-PassFS,-PValue,-RushFS,-RValue,-Wins,-Losses)
        }
      }
      if(input$nflsznpos=="RB"){
        df<- rb %>%
          filter(GS >7& Att>120)
        if (input$nflsznradio==1){
          df<-df %>%
            select(-Att,YPA,-Yds,-TD,-Fmb,-RecYds,-Rec,-RecTD,-YPA)
        }
        if (input$nflsznradio==2){
          df<-df%>%
            select(-RecFS,-RecValue,-RushFS,-RValue)
        }
        if (input$nflsznradio==3){
          df<-df%>%
            mutate(Att=round(Att/G,1))%>%
            mutate(Yds=round(Yds/G,1))%>%
            mutate(TD=round(TD/G,1))%>%
            mutate(RecYds=round(RecYds/G,1))%>%
            mutate(RecTD=round(RecTD/G,1))%>%
            mutate(Rec=round(Rec/G,1))%>%
            mutate(Fmb=round(Fmb/G,1))%>%
            select(-RecFS,-RecValue,-RushFS,-RValue)
        }
      }
      if(input$nflsznpos=="WR/TE"){
        df<- wr%>%
          filter(GS >7& Rec>25)%>%
          arrange(-Value)%>%
          mutate(Value = round(Value,2))%>%
          mutate(FloStrength = round(FloStrength,3))
        if (input$nflsznradio==1){
          df<-df %>%
            select(-GS,-Rec,-Yds,-TD,-Fmb,-YPR)
        }
        if (input$nflsznradio==2){
          df<-df%>%
            select(-FloStrength,-GS)
        }
        if (input$nflsznradio==3){
          df<-df%>%
            mutate(Rec=round(Rec/G,1))%>%
            mutate(Yds=round(Yds/G,1))%>%
            mutate(TD=round(TD/G,1))%>%
            mutate(Fmb=round(Fmb/G,1))%>%
            select(-FloStrength,-GS)
        }
      }
      if(input$nflsznpos=="Defense"){
        df<- def %>%
          filter(G >7& Solo >32) %>%
          mutate(Rush = round(Rush,2))%>%
          mutate(Coverage = round(Coverage,2))%>%
          mutate(Tackle = round(Tackle,2))
        if (input$nflsznradio==1){
          df<-df %>%
            select(-Int,-PD,-FF,-Sk,-Comb,-Solo,-Ast,-TFL,-QBHits)
        }
        if (input$nflsznradio==2){
          df<-df%>%
            select(-DefFS,-Rush,-Tackle,-Coverage)
        }
        if (input$nflsznradio==3){
          df<-df%>%
            mutate(Int=round(Int/G,1))%>%
            mutate(PD=round(PD/G,1))%>%
            mutate(FF=round(FF/G,1))%>%
            mutate(Sk=round(Sk/G,1))%>%
            mutate(QBHits=round(QBHits/G,1))%>%
            mutate(Comb=round(Comb/G,1))%>%
            mutate(Solo=round(Solo/G,1))%>%
            mutate(Ast=round(Ast/G,1))%>%
            mutate(TFL=round(TFL/G,1))%>%
            select(-DefFS,-Rush,-Tackle,-Coverage)
        }
      }
    }
    df <- df %<>%
      arrange(-Value) %>%
      filter(Season>(input$nflsznyr[1]-1)&Season<(input$nflsznyr[2]+1))
    if (input$nflsznplayer !="All"){
      df<-df %>%
        filter(Name == input$nflsznplayer)
    }
    if (input$nflsznteam !="All"){
      df<-df %>%
        filter(Tm == input$nflsznteam)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NFLCareers<- DT::renderDataTable({
    qb<- read.csv("QBCareersforapp")
    rb<- read.csv("RBCareersforapp")
    wr<-read.csv("WRCareersforapp")
    def<- read.csv("DEFCareersforapp") 
    if(input$nflcarpos=="All"){
      qb<- qb %>%
        select(Name,Pos, Value)
      rb<- rb %>%
        select(Name,Pos, Value)
      wr<- wr %>%
        select(Name,Pos, Value)
      def<- def %>%
        select(Name,Pos, Value)
      df<- rbind(qb,rb,wr,def) %>%
        mutate(Value = round(Value,2))
    }else{
      if(input$nflcarpos=="QB"){
        df<- qb 
        if (input$nflcarradio==1){
          df<-df %>%
            mutate(Wpct = round(Wins/GS,3))%>%
            select(-Cmp,-Att,-CmpPer,-PassYds,-TD,-Int,-GWD,-RushAtt,-RushYds,-RushTD,-Fmb,-Wins,-Losses,-YPA)
        }
        if (input$nflcarradio==2){
          df<-df%>%
            select(-PassFS,-PValue,-RushFS,-RValue)
        }
        if (input$nflcarradio==3){
          df<-df%>%
            mutate(Wpct = round(Wins/GS,3))%>%
            mutate(Cmp=Cmp/GS)%>%
            mutate(Att=Att/GS)%>%
            mutate(PassYds=PassYds/GS)%>%
            mutate(TD=TD/GS)%>%
            mutate(Int=Int/GS)%>%
            mutate(RushYds=RushYds/GS)%>%
            mutate(RushTD=RushTD/GS)%>%
            select(-PassFS,-PValue,-RushFS,-RValue,-Wins,-Losses)
        }
      }
      if(input$nflcarpos=="RB"){
        df<- rb 
        if (input$nflcarradio==1){
          df<-df %>%
            select(-Att,YPA,-Yds,-TD,-Fmb,-RecYds,-Rec,-RecTD,-YPA)
        }
        if (input$nflcarradio==2){
          df<-df%>%
            select(-RecFS,-RecValue,-RushFS,-RValue)
        }
        if (input$nflcarradio==3){
          df<-df%>%
            mutate(Att=round(Att/GS,1))%>%
            mutate(Yds=round(Yds/GS,1))%>%
            mutate(TD=round(TD/GS,1))%>%
            mutate(RecYds=round(RecYds/GS,1))%>%
            mutate(RecTD=round(RecTD/GS,1))%>%
            mutate(Rec=round(Rec/GS,1))%>%
            mutate(Fmb=round(Fmb/GS,1))%>%
            select(-RecFS,-RecValue,-RushFS,-RValue)
        }
      }
      if(input$nflcarpos=="WR/TE"){
        df<- wr%>%
          mutate(Value = round(Value,2))%>%
          mutate(FloStrength = round(FloStrength,3))
        if (input$nflcarradio==1){
          df<-df %>%
            select(-GS,-Rec,-Yds,-TD,-Fmb,-YPR)
        }
        if (input$nflcarradio==2){
          df<-df%>%
            select(-FloStrength,-GS)
        }
        if (input$nflcarradio==3){
          df<-df%>%
            mutate(Rec=round(Rec/G,1))%>%
            mutate(Yds=round(Yds/G,1))%>%
            mutate(TD=round(TD/G,1))%>%
            mutate(Fmb=round(Fmb/G,1))%>%
            select(-FloStrength,-GS)
        }
      }
      if(input$nflcarpos=="Defense"){
        df<- def %>%
          filter(G >7& Solo >32) %>%
          mutate(Rush = round(Rush,2))%>%
          mutate(Coverage = round(Coverage,2))%>%
          mutate(Tackle = round(Tackle,2))
        if (input$nflcarradio==1){
          df<-df %>%
            select(-Int,-PD,-FF,-Sk,-Comb,-Solo,-Ast,-TFL,-QBHits)
        }
        if (input$nflcarradio==2){
          df<-df%>%
            select(-DefFS,-Rush,-Tackle,-Coverage)
        }
        if (input$nflcarradio==3){
          df<-df%>%
            mutate(Int=round(Int/G,1))%>%
            mutate(PD=round(PD/G,1))%>%
            mutate(FF=round(FF/G,1))%>%
            mutate(Sk=round(Sk/G,1))%>%
            mutate(QBHits=round(QBHits/G,1))%>%
            mutate(Comb=round(Comb/G,1))%>%
            mutate(Solo=round(Solo/G,1))%>%
            mutate(Ast=round(Ast/G,1))%>%
            mutate(TFL=round(TFL/G,1))%>%
            select(-DefFS,-Rush,-Tackle,-Coverage)
        }
      }
    }
    df <- df %>%
      arrange(-Value)
    if (input$nflcarplayer !="All"){
      df<-df %>%
        filter(Name == input$nflcarplayer)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NFLQBRank <- DT::renderDataTable({
    df <-read.csv("NFLQBRank")
    DT::datatable(df,filter = "top")
  })
  output$NFLRBRank <- DT::renderDataTable({
    df <-read.csv("NFLRBRank")
    DT::datatable(df,filter = "top")
  })
  output$NFLWRRank <- DT::renderDataTable({
    df <-read.csv("NFLWRRank")
    DT::datatable(df,filter = "top")
  })
  output$NFLDefRank <- DT::renderDataTable({
    df <-read.csv("NFLDefRank")
    DT::datatable(df,filter = "top")
  })
  output$NFLMVPRank <- DT::renderDataTable({
    qb <- read.csv("NFLQBRank") %>%
      select(Name, "Pos"=Position,Team,Value)
    rb <- read.csv("NFLRBRank") %>%
      select(Name, "Pos",Team,Value)
    wr <- read.csv("NFLWRRank") %>%
      select(Name, "Pos",Team,Value)
    def <- read.csv("NFLDefRank") %>%
      select(Name, "Pos",Team,Value)
    mvpnfl <- rbind(qb,rb,wr,def) %>%
      arrange(-Value) %>%
      slice(1:200)    
    DT::datatable(mvpnfl,filter = "top")
  })
  output$NFLPowerRank <- DT::renderDataTable({
    df <-read.csv("NFLPowerRank")
    DT::datatable(df,filter = "top")
  })
  output$teamsNFL <- DT::renderDataTable({
    nflteams<- read.csv("NFLTeamsATforapp")
    if (input$nflteam!="All"){
      nflteams<-nflteams%>%
        filter(Tm==input$nflteam)
    }
    DT::datatable(nflteams,options = list(scrollX = TRUE))
  })
  output$franNFL <- DT::renderDataTable({
    df <-read.csv("NFLFranRankforapp")
    DT::datatable(df,filter = "top")
  })
  output$corrNFLPlot <- renderPlotly({
    df <- read.csv("NFLTeamsATforapp")
    fit <- lm(df[,input$nflvar2] ~ df[,input$nflvar1])
    plot_ly(df, x = df[,input$nflvar1], y=df[,input$nflvar2], type = 'scatter', mode = 'markers', text = ~paste('Team: ', str_c(Tm,yearID, sep=", ")))%>% 
      add_lines(x = df[,input$nflvar1], y = fitted(fit)) %>%
      layout(showlegend = F)
  })
  output$corrNFL <- renderText({
    df <- read.csv("NFLTeamsATforapp")
    str_c("Correlation = ",round(cor(df[,input$nflvar1], df[,input$nflvar2]),3))
  })
  output$NFLCompPlot<- renderPlotly({
    if(input$nflcomppos=="QB"){
      df<- read.csv("QBSeasonsforapp")%>%
        arrange(Season)%>%
        group_by(Name)%>%
        mutate(exp = c(1:length(Name)))%>%
        ungroup()
    }else{
      if(input$nflcomppos=="RB"){
        df<- read.csv("RBSeasonsforapp")%>%
          arrange(Season)%>%
          group_by(Name)%>%
          mutate(exp = c(1:length(Name)))%>%
          ungroup()
      }
      if(input$nflcomppos=="WR/TE"){
        df<- read.csv("WRSeasonsforapp")%>%
          arrange(Season)%>%
          group_by(Name)%>%
          mutate(exp = c(1:length(Name)))%>%
          ungroup()
      }
      if(input$nflcomppos=="Defense"){
        df<- read.csv("DEFSeasonsforapp")%>%
          arrange(Season)%>%
          group_by(Name)%>%
          mutate(exp = c(1:length(Name)))%>%
          ungroup()
      }
    }
    df<- df %>%
      filter(Name == input$nflcomp1|Name==input$nflcomp2)
    if(input$nflcompradio==1){
      df<- df %>%
        group_by(Name) %>%
        mutate(Value = sum(Value))%>%
        slice(1)%>%
        ungroup()
      plot_ly(df, x = ~Name, y = ~Value, type = 'bar', color = ~Name,colors = c("blue","red"))
    }else{
      if(input$nflcompradio==2){
        plot_ly(df, x = ~Age, y = ~Value, type = 'scatter', mode = "line",color = ~Name,colors = c("blue","red"))
      }else{
        if(input$nflcompradio==3){
          plot_ly(df, x = ~exp, y = ~Value, type = 'scatter', mode = "line",color = ~Name,colors = c("blue","red"))
        }
      }
    }
  })
  output$NFLAgePlot<-renderPlotly({
    qb<- read.csv("QBSeasonsforapp")
    rb<- read.csv("RBSeasonsforapp")
    wr<-read.csv("WRSeasonsforapp")
    def<- read.csv("DEFSeasonsforapp") 
    if(input$nflsznpos=="All"){
      qb<- qb %>%
        filter(GS >7&PassYds>2000) %>%
        select(Name,Pos,Age,Season,Tm, Value)
      rb<- rb %>%
        filter(GS >7& Att>120)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      wr<- wr %>%
        filter(GS >7& Rec>25)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      def<- def %>%
        filter(G >7& Solo >32)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      df<- rbind(qb,rb,wr,def)
    }
    if(input$nflagepos != "All"){
      if (input$nflagepos == "QB"){
        df<- df %>%
          filter(Pos=="QB")
      }
      if (input$nflagepos == "RB"){
        df<- df %>%
          filter(Pos=="RB")
      }
      if (input$nflagepos == "WR/TE"){
        df<- df %>%
          filter(Pos=="WR")
      }
      if (input$nflagepos == "Defense"){
        df<- df %>%
          filter(Pos!= "QB")%>%
          filter(Pos!= "RB")%>%
          filter(Pos!= "WR")
      }
    }
    plot_ly(df, x = ~Age, y = ~Value, type = "box", text = ~paste('Name: ', Name))
  })
  output$NFLYrPlot<-renderPlotly({
    qb<- read.csv("QBSeasonsforapp")
    rb<- read.csv("RBSeasonsforapp")
    wr<-read.csv("WRSeasonsforapp")
    def<- read.csv("DEFSeasonsforapp") 
      qb<- qb %>%
        filter(GS >7&PassYds>2000) %>%
        select(Name,Pos,Age,Season,Tm, Value)
      rb<- rb %>%
        filter(GS >7& Att>120)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      wr<- wr %>%
        filter(GS >7& Rec>25)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      def<- def %>%
        filter(G >7& Solo >32)%>%
        select(Name,Pos,Age,Season,Tm, Value)
      df<- rbind(qb,rb,wr,def)
    if(input$nflyrpos != "All"){
      if (input$nflyrpos == "QB"){
        df<- df %>%
          filter(Pos=="QB")
      }
      if (input$nflyrpos == "RB"){
        df<- df %>%
          filter(Pos=="RB")
      }
      if (input$nflyrpos == "WR/TE"){
        df<- df %>%
          filter(Pos=="WR")
      }
      if (input$nflyrpos == "Defense"){
        df<- df %>%
          filter(Pos!= "QB")%>%
          filter(Pos!= "RB")%>%
          filter(Pos!= "WR")
      }
    }
    plot_ly(df, x = ~Season, y = ~Value, type = "box", text = ~paste('Name: ', Name))
  })
  output$NFLTrendPlot <- renderPlotly({
    df<- read.csv("NFLTeamsATforapp")%>%
      group_by(yearID)%>%
      mutate(Points = mean(PSPG)) %>%
      mutate(Competitiveness = 10*(.2-mean(abs(sd(FloStrength)))))%>%
      slice(1) %>%
      ungroup()
    if(input$nfltrendstat==1){
      plot_ly(df, x = ~yearID, y = ~Points, type = 'bar', color=~yearID)
    }else{
      if(input$nfltrendstat==2){
        plot_ly(df, x = ~yearID, y = ~Competitiveness, type = 'bar', color=~yearID)
      }else{
        plot_ly(df, x = ~yearID, y = ~Wpct, type = 'bar', color=~yearID)
      }
    }
  })
  ################################################
  ##############    NCAAMB
  ################################################
  
  
  
  output$NCAAMBTeams <- DT::renderDataTable({
    df <-read.csv("NCAAMBPowerRank")
    DT::datatable(df,filter = "top")
  })
  output$NCAAMBHistory <- DT::renderDataTable({
    df <-read.csv("NCAAMBHistoryforapp")
    DT::datatable(df,filter = "top")
  })
  
  
  ################################################
  ##############    NHL
  ################################################
  output$NHLTeams <- DT::renderDataTable({
    df <-read.csv("NHLPowerRank")
    DT::datatable(df,filter = "top",options = list(scrollX = TRUE))
  })
  output$NHLPlayers <- DT::renderDataTable({
    df <-read.csv("NHLMVP")
    DT::datatable(df,filter = "top")
  })
  output$NHLTeamsAT <- DT::renderDataTable({
    df <-read.csv("NHLTeamsATforapp") %>%
      filter(yearID>(input$yearnhlteam[1]-1)&yearID<(input$yearnhlteam[2]+1))%>%
      mutate(Wpct = round(WinPct,3)) %>%
      mutate(TeamFS = round(TeamScore,3)) %>%
      mutate(PlayerFS = round(PlayerScore,3)) %>%
      mutate(SchedFS = round(SchedScore,3))%>%
      mutate(FloStrength = round(FloStrength,3)) %>%
      select(Team,yearID, Wpct,GP,GS,GA, TeamFS, SchedFS, PlayerFS, FloStrength, SCChamp) %>%
      arrange(-FloStrength)
      if(input$nhlteam!="All"){
        df<-df%>%
         filter(Team==input$nhlteam)
      }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLSeasons <- DT::renderDataTable({
    df <-read.csv("NHLPlayersATforapp") %>%
      mutate(DefScore = round(DefScore,2))%>%
      mutate(ShotScore = round(ShotScore,2))%>%
      mutate(PtScore = round(PtScore,2))%>%
      mutate(FloStrength = round(FloStrength,2))%>%
      mutate(Value = round(Value,2))%>%
      filter(yearID>(input$yearnhlszn[1]-1)&yearID<(input$yearnhlszn[2]+1))%>%
      arrange(-Value)
    if(input$nhlsznname!="All"){
      df<-df%>%
        filter(Player==input$nhlsznname)
    }
    if(input$nhlsznteam!="All"){
      df<-df%>%
        filter(Tm==input$nhlsznteam)
    }
    if(input$nhlsznpos!="All"){
      df<-df%>%
        filter(Pos==input$nhlsznpos)
    }
    if(input$nhlsznstat==1){
      df<- df%>%
        select(Player, Age,Tm, yearID,Pos,GP,Value,ShotScore,PtScore,DefScore,FloStrength)
    }else{
      if(input$nhlsznstat==2){
        df<- df%>%
          select(Player, Age,Tm, yearID,Pos,GP,Value,G,A,S,SV)
      }else{
        df<- df%>%
          select(Player, Age,Tm, yearID,Pos,GP,Value,G,A,S,SV)%>%
          mutate(G = round(G/GP,1))%>%
          mutate(A = round(A/GP,1))%>%
          mutate(S = round(S/GP,1))%>%
          mutate(SV = round(SV/GP,1))
      }
    }
    df<- df %>% 
      filter(GP>42)
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLCareers <- DT::renderDataTable({
    df <-read.csv("NHLPlayerCarATforapp") %>%
      mutate(DefScore = round(DefScore,2))%>%
      mutate(ShotScore = round(ShotScore,2))%>%
      mutate(PtScore = round(PtScore,2))%>%
      mutate(FloStrength = round(FloStrength,2))%>%
      mutate(Value = round(Value,2))%>%
      arrange(-Value)
    if(input$nhlcarname!="All"){
      df<-df%>%
        filter(Player==input$nhlcarname)
    }
    if(input$nhlcarpos!="All"){
      df<-df%>%
        filter(Pos==input$nhlcarpos)
    }
    if(input$nhlcarstat==1){
      df<- df%>%
        select(Player,Pos,GP,Value,ShotScore,PtScore,DefScore,FloStrength)
    }else{
      if(input$nhlcarstat==2){
        df<- df%>%
          select(Player,Pos,GP,Value,G,A,S,SV)
      }else{
        df<- df%>%
          select(Player,Pos,GP,Value,G,A,S,SV)%>%
          mutate(G = round(G/GP,1))%>%
          mutate(A = round(A/GP,1))%>%
          mutate(S = round(S/GP,1))%>%
          mutate(SV = round(SV/GP,1))
      }
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLCompPlot<- renderPlotly({
    df<- read.csv("NHLPlayersATforapp") %>%
      filter(Player == input$nhlcomp1|Player==input$nhlcomp2)%>%
      arrange(yearID)%>%
      group_by(Player)%>%
      mutate(exp= c(1:length(yearID)))%>%
      ungroup()
    if(input$nhlcompradio==1){
      df<- df %>%
        group_by(Player) %>%
        mutate(Value = sum(Value))%>%
        slice(1)%>%
        ungroup()
      plot_ly(df, x = ~Player, y = ~Value, type = 'bar', color = ~Player,colors = c("blue","red"))
    }else{
      if(input$nhlcompradio==2){
        plot_ly(df, x = ~Age, y = ~Value, type = 'scatter', mode = "line",color = ~Player,colors = c("blue","red"))
      }else{
        if(input$nhlcompradio==3){
          plot_ly(df, x = ~exp, y = ~Value, type = 'scatter', mode = "line",color = ~Player,colors = c("blue","red"))
        }
      }
    }
  })
  output$NHLAgePlot <- renderPlotly({
    df <- read.csv("NHLPlayersATforapp")
    if(input$nhlagepos != "All"){
      df <- df %>%
        filter(Pos ==input$nhlagepos)
    }
    if(input$nhlagestat == 1){
      plot_ly(df, x = ~Age, y = ~Value, type = "box", text = ~paste('Name: ', Player))
    }else{
      plot_ly(df, x = ~Age, y = ~FloStrength, type = "box", text = ~paste('Name: ', Player))
    }
  })
  output$NHLYrPlot <- renderPlotly({
    df <- read.csv("NHLPlayersATforapp")
    if(input$nhlyearpos != "All"){
      df <- df %>%
        filter(Pos ==input$nhlyearpos)
    }
    if(input$nhlyearstat == 1){
      plot_ly(df, x = ~yearID, y = ~Value, type = "box", text = ~paste('Name: ', Player))
    }else{
      plot_ly(df, x = ~yearID, y = ~FloStrength, type = "box", text = ~paste('Name: ', Player))
    }
  })
  output$NHLCorrPlot <- renderPlotly({
    df <- read.csv("NHLTeamsATforapp")%>%
      mutate(GS=GS/GP)%>%
      mutate(GA=GA/GP)
    fit <- lm(df[,input$nhlvar2] ~ df[,input$nhlvar1])
    plot_ly(df, x = df[,input$nhlvar1], y=df[,input$nhlvar2], type = 'scatter', mode = 'markers', text = ~paste('Team: ', str_c(Team,yearID, sep=", ")))%>% 
      add_lines(x = df[,input$nhlvar1], y = fitted(fit)) %>%
      layout(showlegend = F)
  })
  output$NHLCorr <- renderText({
    df <- read.csv("NHLTeamsATforapp")%>%
      mutate(GS=GS/GP)%>%
      mutate(GA=GA/GP)
    str_c("Correlation = ",round(cor(df[,input$nhlvar1], df[,input$nhlvar2]),3))
  })
  output$nhlyearcompplot<- renderPlotly({
    df <- read.csv("NHLTeamsATforapp") %>%
      group_by(yearID) %>%
      mutate(Competitivness = 7*(.2-mean(sd(WinPct))))%>%
      mutate(GPG = mean(GS/GP))%>%
      slice(1) %>%
      ungroup()
    if(input$nhlcompyrstat==1){
      plot_ly(df, x = ~yearID, y = ~GPG, type = 'bar', color=~yearID)
    }else{
        plot_ly(df, x = ~yearID, y = ~Competitivness, type = 'bar', color=~yearID)
    }
  })
  
  ################################################
  ##############    MLB
  ################################################
  
  output$MLBBatters <- DT::renderDataTable({
    if ((input$mlbhit=="All"&input$mlbhitteam=="All")&(input$mlbhitpos=="All"&input$mlbhityrsing=="All")){
      df <-read.csv("MLBBattersATforapp") %>%
        filter(yearID >= input$mlbhityr[1]) %>%
        filter(yearID <= input$mlbhityr[2])%>%
        group_by(yearID) %>%
        filter(G>.8*max(G))%>%
        ungroup() %>%
        select(Name, yearID, teamID, POS,G,H,HR,RBI,SB,AVG,OBP,SLG, OffFS,DefFS, FloValue) %>%
        arrange(-FloValue)
    } else{
      if (input$mlbhityrsing!="All"){
        df <-read.csv("MLBBattersATforapp") %>%
          filter(yearID == input$mlbhityrsing) %>%
          select(Name, yearID, teamID, POS,G,H,HR,RBI,SB,AVG,OBP,SLG, OffFS,DefFS, FloValue) %>%
          arrange(-FloValue)
      }else{
        df <-read.csv("MLBBattersATforapp") %>%
          filter(yearID >= input$mlbhityr[1]) %>%
          filter(yearID <= input$mlbhityr[2])%>%
          select(Name, yearID, teamID, POS,G,H,HR,RBI,SB,AVG,OBP,SLG, OffFS,DefFS, FloValue) %>%
          arrange(-FloValue)
      }
    }
    
    if (input$mlbhit!="All"){
      df <-df %>%
        filter(Name == input$mlbhit) %>%
        arrange(yearID)
    }
    if (input$mlbhitteam!="All"){
      df <-df %>%
        filter(teamID == input$mlbhitteam) %>%
        arrange(-FloValue)
    }
    if (input$mlbhitpos!="All"){
      df <-df%>%
        filter(POS == input$mlbhitpos) %>%
        arrange(-FloValue)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$MLBPitchers <- DT::renderDataTable({
    if ((input$mlbpitcher=="All"&input$mlbpitchteam=="All")&(input$mlbpitchyrsing=="All")){
      df <-read.csv("MLBPitchersATforapp") %>%
        filter(yearID >= input$mlbpitchyr[1]) %>%
        filter(yearID <= input$mlbpitchyr[2])%>%
        group_by(yearID) %>%
        filter((GS>.7*max(GS))|(G>.7*max(G)))%>%
        ungroup() %>%
        select(Name, yearID, teamID,W,L,G,GS,SV,IP,SO,ERA,FloStrength,FloValue) %>%
        arrange(-FloValue) 
      
    } else{
      if (input$mlbpitchyrsing!="All"){
        df <-read.csv("MLBPitchersATforapp") %>%
          filter(yearID == input$mlbpitchyrsing) %>%
          select(Name, yearID, teamID,W,L,G,GS,SV,IP,SO,ERA,FloStrength,FloValue) %>%
          arrange(-FloValue)
      }else{
        df <-read.csv("MLBPitchersATforapp") %>%
          filter(yearID >= input$mlbpitchyr[1]) %>%
          filter(yearID <= input$mlbpitchyr[2])%>%
          select(Name, yearID, teamID,W,L,G,GS,SV,IP,SO,ERA,FloStrength,FloValue) %>%
          arrange(-FloValue)
      }
    }
    if (input$mlbpitcher!="All"){
      df <-df %>%
        filter(Name == input$mlbpitcher) %>%
        arrange(yearID)
    }
    if (input$mlbpitchteam!="All"){
      df <-df %>%
        filter(teamID == input$mlbpitchteam) %>%
        arrange(-FloValue)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$MLBTeamsAT <- DT::renderDataTable({
    df <-read.csv("MLBTeamsATforapp") %>%
      mutate(L = G-W) %>%
      mutate(RD = (R - RA)/G)%>%
      mutate(RD= round(RD,2))%>%
      mutate(Hitting= round(Hitting,3))%>%
      mutate(Pitching= round(Pitching,3))%>%
      mutate(Fielding= round(Fielding,3))%>%
      mutate(Value= round(Value,3))%>%
      mutate(PValue= round(PValue,3))%>%
      mutate(FValue= round(FValue,3))%>%
      mutate(FSPlay= round(FSPlay,3))%>%
      mutate(FSTeam= round(FSTeam,3))%>%
      mutate(Wpct= round(Wpct,3))%>%
      mutate(FloStrength= round(FloStrength,3))%>%
      select(teamID,yearID, Era,"Lg"="LgWin", "WS"="WSWin", Wpct, G, W,L,Hitting, Pitching,Fielding, "OV"="Value", "PV"="PValue", "FV"="FValue", FSPlay,FSTeam, FloStrength ) %>%
      arrange(-FloStrength)
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$MLBHOF <- DT::renderDataTable({
    df <-read.csv("MLBPlayersATforapp") %>%
      mutate(HOF = ifelse(is.na(HOF)==TRUE,"N",HOF))%>%
      arrange(-FloValue)
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$MLBBattersCar <- DT::renderDataTable({
    df <-read.csv("MLBBattersCarforapp") %>%
      select(Name,"RookYr"= "yearID", Seasons, Current,HOF,G,AB,H,HR,RBI,R,SB,AVG,OBP,SLG,OffFS,DefFS,FloValue) %>%
      arrange(-FloValue) %>%
      mutate(HOF = ifelse(HOF=="Y","Y","N"))
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$MLBPitchersCar <- DT::renderDataTable({
    df <-read.csv("MLBPitchersCarforapp")  %>%
      filter(playerID!="-9999")%>%
      select(Name, "RookYr"="yearID", Seasons, Current, HOF, G,GS,W,L,SV,SO,IP,WHIP,ERA,FloStrength,FloValue)%>%
      arrange(-FloValue) %>%
      mutate(HOF = ifelse(HOF=="Y","Y","N"))
    
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$MLBPlotCompHitter <- renderPlotly({
    df <-read.csv("MLBBattersATforapp") %>%
      mutate(Age = yearID - birthYear) %>%
      filter(Name == input$mlbcompp1 | Name == input$mlbcompp2) %>%
      group_by(Name) %>%
      mutate(Exp = c(1:length(yearID))) %>%
      ungroup
    if (input$mlbcompvar == "Age"){
      plot_ly(df, x = ~Age, y = ~FloValue, type = 'scatter', mode = 'markers', text = ~paste('Name: ', Name), color = ~Name,colors = c("blue","red"))%>%
        layout(xaxis= list(range = c(15,50)), yaxis= list(range = c(-10,20)))
    } else{
      if (input$mlbcompvar == "Experiance"){
        plot_ly(df, x = ~Exp, y = ~FloValue, type = 'scatter', mode = 'markers', text = ~paste('Name: ', Name), color = ~Name,colors = c("blue","red"))%>%
          layout(xaxis= list(range = c(0,25)), yaxis= list(range = c(-10,20)))
      } else{
        plot_ly(df, x = ~yearID, y = ~FloValue, type = 'scatter', mode = 'markers', text = ~paste('Name: ', Name), color = ~Name,colors = c("blue","red"))%>%
          layout(xaxis= list(range = c(1900,2025)), yaxis= list(range = c(-10,20)))
      }
    }
  })
  output$MLBPlotCompPitcher <- renderPlotly({
    df <-read.csv("MLBPitchersATforapp") %>%
      mutate(Age = yearID - birthYear) %>%
      filter(Name == input$mlbcomppit1 | Name == input$mlbcomppit2) %>%
      group_by(Name) %>%
      mutate(Exp = c(1:length(yearID))) %>%
      ungroup
    if (input$mlbcompvarp == "Age"){
      plot_ly(df, x = ~Age, y = ~FloValue, type = 'scatter', mode = 'markers', text = ~paste('Name: ', Name), color = ~Name,colors = c("blue","red"))%>%
        layout(xaxis= list(range = c(15,50)), yaxis= list(range = c(-10,20)))
    } else{
      if (input$mlbcompvarp == "Experiance"){
        plot_ly(df, x = ~Exp, y = ~FloValue, type = 'scatter', mode = 'markers', text = ~paste('Name: ', Name), color = ~Name,colors = c("blue","red"))%>%
          layout(xaxis= list(range = c(0,25)), yaxis= list(range = c(-10,20)))
      } else{
        plot_ly(df, x = ~yearID, y = ~FloValue, type = 'scatter', mode = 'markers', text = ~paste('Name: ', Name), color = ~Name,colors = c("blue","red"))%>%
          layout(xaxis= list(range = c(1900,2025)), yaxis= list(range = c(-10,20)))
      }
    }
  })
  output$MLBPlotAge <- renderPlotly({
    df <-read.csv("MLBBattersATforapp") %>%
      mutate(Age = yearID - birthYear) %>%
      select(Name, Age, POS, FloValue)
    df1 <-read.csv("MLBPitchersATforapp") %>%
      mutate(Age = yearID - birthYear) %>%
      mutate(POS = "P") %>%
      select(Name, Age, POS, FloValue)
    df <- rbind(df,df1)
    if (input$mlbagefilt != "All"){
      df <- df %>%
        filter(POS == input$mlbagefilt)
    }
    plot_ly(df, x = ~Age, y = ~FloValue, type = "box", text = ~paste('Name: ', Name))%>%
      layout(yaxis= list(range = c(-10,20)))
  })
  output$MLBPlotYear <- renderPlotly({
    df <-read.csv("MLBBattersATforapp") %>%
      select(Name, yearID, POS, FloValue)
    df1 <-read.csv("MLBPitchersATforapp") %>%
      mutate(POS = "P") %>%
      select(Name, yearID, POS, FloValue)
    df <- rbind(df,df1)
    if (input$mlbyrfilt != "All"){
      df <- df %>%
        filter(POS == input$mlbyrfilt)
    }
    plot_ly(df, x = ~yearID, y = ~FloValue, type = "box", text = ~paste('Name: ', Name))%>%
      layout(yaxis= list(range = c(-10,20)))
  })
  output$MLBPlotEras <- renderPlotly({
    df<- read.csv("MLBTeamsATforapp") %>%
      mutate(RPG = R/G) %>%
      mutate(HRPG = HR/G) %>%
      mutate(SOPG = SO/G) %>%
      mutate(HPG = H/G)%>%
      group_by(yearID) %>%
      mutate(Competitivness = 6.5*(.2-mean(sd(Wpct))))%>%
      mutate(RPG = mean(RPG))%>%
      mutate(HRPG = mean(HRPG))%>%
      mutate(SOPG = mean(SOPG))%>%
      mutate(HPG = mean(HPG))%>%
      slice(1) %>%
      ungroup() %>%
      mutate(Era = as.factor(Era))%>%
      mutate(Era = ifelse(Era==1,"Dead Ball",Era))%>%
      mutate(Era = ifelse(Era==2,"Live Ball",Era))%>%
      mutate(Era = ifelse(Era==3,"Integration",Era))%>%
      mutate(Era = ifelse(Era==4,"Expansion",Era))%>%
      mutate(Era = ifelse(Era==5,"Free Agency",Era))%>%
      mutate(Era = ifelse(Era==6,"Steroid",Era))%>%
      mutate(Era = ifelse(Era==7,"Modern",Era))
    if(input$mlberavar=="Competetiveness"){
      plot_ly(df, x = ~yearID, y = ~Competitivness, type = "bar",color=~Era)%>%
        layout(yaxis= list(range = c(0,1)))
    }else{
      if(input$mlberavar=="Runs"){
        plot_ly(df, x = ~yearID, y = ~RPG, type = "bar",color=~Era)
      }else{
        if(input$mlberavar=="HR"){
          plot_ly(df, x = ~yearID, y = ~HRPG, type = "bar",color=~Era)
        }else{
          if(input$mlberavar=="SO"){
            plot_ly(df, x = ~yearID, y = ~SOPG, type = "bar",color=~Era)
          }else{
            if(input$mlberavar=="Hits"){
              plot_ly(df, x = ~yearID, y = ~HPG, type = "bar",color=~Era)
            }}}}}
  })
  output$MLBPlotCorr <- renderPlotly({
    df <-read.csv("MLBTeamsATforapp") %>%
      mutate(R= R/G) %>%
      mutate(RA= RA/G) %>%
      mutate(HR = HR/G) %>%
      mutate(SO= SO/G) %>%
      mutate(H= H/G)
    fit <- lm(df[,input$mlbcorrvar2] ~ df[,input$mlbcorrvar1])
    plot_ly(df, x = df[,input$mlbcorrvar1], y=df[,input$mlbcorrvar2], type = 'scatter', mode = 'markers', text = ~paste('Team: ', str_c(teamID,yearID, sep=", ")))%>% 
      add_lines(x = df[,input$mlbcorrvar1], y = fitted(fit)) %>%
      layout(showlegend = F)  })
  output$MLBCorr <- renderText({
    df <- read.csv("MLBTeamsATforapp")%>%
      mutate(R= R/G) %>%
      mutate(RA= RA/G) %>%
      mutate(HR = HR/G) %>%
      mutate(SO= SO/G) %>%
      mutate(H= H/G)
    str_c("Correlation = ",round(cor(df[,input$mlbcorrvar1], df[,input$mlbcorrvar2]),3))
  })
  output$mlbhitterproj<- DT::renderDataTable({
    df<- read.csv("MLBhitpred23")%>%
      arrange(-PredVal)%>%
      select(Name,teamID,"FloPrediction"=PredVal)%>%
      mutate(FloPrediction = round(FloPrediction,3))
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$mlbpitcherproj<- DT::renderDataTable({
    df<- read.csv("MLBpitpred23")%>%
      arrange(-PredVal)%>%
      select(Name,teamID,"FloPrediction"=PredVal)%>%
      mutate(FloPrediction = round(FloPrediction,3))
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$mlbteamproj<- DT::renderDataTable({
    df<- read.csv("MLBteampred23")%>%
      select(teamID,"FloPrediction"=PredWpct,"Book OU"=ou)%>%
      mutate(FloPrediction = round(FloPrediction,3))
    DT::datatable(df,options = list(scrollX = TRUE))
  })

}

shinyApp(ui, server)