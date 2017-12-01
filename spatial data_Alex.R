library(readr)
library(rvest)
library(data.table)
data1516 <- read_csv("D:/Dropbox/UCLA/[Courses]/Stats Minor/STATS 199/15-16/[10-20-2015]-[06-20-2016]-combined-stats.csv")

mainpage1<-read_html("https://www.teamrankings.com/nba/player-stat/field-goal-percentage?season_id=212")
players_FGP<-mainpage1 %>% html_nodes(".text-left:nth-child(2) a") %>% html_text()
FGP_FGP<-mainpage1 %>% html_nodes(".text-right") %>% html_text()
FGP_FGP<-FGP_FGP[-1]
FGP_FGP<-as.numeric(gsub("%", "", FGP_FGP))
positions_FGP<-mainpage1 %>% html_nodes(".text-left:nth-child(4)") %>% html_text()
positions_FGP<-positions_FGP[-1]
hist(FGP_FGP)

match<-match(data1516$player,players_FGP)
data1516$rankingby_FGP<-match
Pos<-vector(length=length(data1516))
for(i in 1:nrow(data1516)){Pos[i]<-positions_FGP[match[i]]}
data1516$Pos<-Pos
FGP<-vector(length=length(data1516))
for(i in 1:nrow(data1516)){FGP[i]<-FGP_FGP[match[i]]}
data1516$FGP<-FGP
plot(FGP)


mainpage2<-read_html("https://www.teamrankings.com/nba/player-stat/three-point-field-goal-percentage?season_id=212")
players_FG3P<-mainpage2 %>% html_nodes(".text-left:nth-child(2) a") %>% html_text()
FG3P_FG3P<-mainpage2 %>% html_nodes(".text-right") %>% html_text()
FG3P_FG3P<-FG3P_FG3P[-1]
FG3P_FG3P<-as.numeric(gsub("%", "", FG3P_FG3P))
positions_FG3P<-mainpage1 %>% html_nodes(".text-left:nth-child(4)") %>% html_text()
positions_FG3P<-positions_FG3P[-1]

match2<-match(data1516$player,players_FG3P)
data1516$rankingby_FG3P<-match2
FG3P<-vector(length=length(data1516))
for(i in 1:nrow(data1516)){FG3P[i]<-FG3P_FG3P[match2[i]]}
data1516$FG3P<-FG3P
Pos2<-vector(length=length(data1516))
for(i in 1:nrow(data1516)){Pos2[i]<-positions_FG3P[match2[i]]}

