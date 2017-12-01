library(dplyr)
library(readr)
Final_nbadat <- read_csv("D:/Dropbox/UCLA/[Courses]/Stats Minor/STATS 199/199 Research/Final_nbadat.csv")
Final_nbadat<-Final_nbadat[,c(-1,-3)]
#Final_nbadat$FGP<-round(Final_nbadat$FGP,3)
nbadat_valid<-Final_nbadat[which(Final_nbadat$FGA>=300),]
library(plotly)
p <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = nbadat_valid$FGP[nbadat_valid$Positions=="PG"|nbadat_valid$Positions=="SG"],name='Guards') %>%
  add_histogram(x = nbadat_valid$FGP[nbadat_valid$Positions=="PF"|nbadat_valid$Positions=="SF"],name='Forwards') %>%
  add_histogram(x = nbadat_valid$FGP[nbadat_valid$Positions=="C"],name='Centers') %>%
  layout(barmode = "overlay")
p

library(ggplot2)
hist_C <- ggplot(nbadat_valid[nbadat_valid$Positions=="C",], aes(FGP)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(fill = "#ff4d4d", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) + 
  ggtitle("Density with Histogram Center")
hist_C <- ggplotly(hist_C)
hist_C

hist_SG <- ggplot(nbadat_valid[nbadat_valid$Positions=="SG",], aes(FGP)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(fill = "#ff4d4d", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) +
  ggtitle("Density with Histogram SG")
hist_SG <- ggplotly(hist_SG)
hist_SG

hist_PG <- ggplot(nbadat_valid[nbadat_valid$Positions=="PG",], aes(FGP)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(fill = "#ff4d4d", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) +
  ggtitle("Density with Histogram PG")
hist_PG <- ggplotly(hist_PG)
hist_PG

hist_PF <- ggplot(nbadat_valid[nbadat_valid$Positions=="PF",], aes(FGP)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(fill = "#ff4d4d", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) +
  ggtitle("Density with Histogram PF")
hist_PF <- ggplotly(hist_PF)
hist_PF

hist_SF <- ggplot(nbadat_valid[nbadat_valid$Positions=="SF",], aes(FGP)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(fill = "#ff4d4d", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) +
  ggtitle("Density with Histogram SF")
hist_SF <- ggplotly(hist_SF)
hist_SF
attach(nbadat_valid)
unique(nbadat_valid$player[which(FGP>=0.4904095 & Positions=="PG")])

library(dplyr)

FGP_top<-nbadat_valid %>% group_by(player) %>% select(player,FGP,Positions,FGP_rank) 
FGP_top<-unique(FGP_top)
C_top<-FGP_top[FGP_top$Positions=="C",]
C_top<-na.omit(C_top)
C_top<-C_top[order(C_top$FGP,decreasing = T),]
Ctop<-head(C_top,n=10)

PG_top<-FGP_top[FGP_top$Positions=="PG",]
PG_top<-na.omit(PG_top)
PG_top<-PG_top[order(PG_top$FGP,decreasing = T),]
PGtop<-head(PG_top,n=10)

SG_top<-FGP_top[FGP_top$Positions=="SG",]
SG_top<-na.omit(SG_top)
SG_top<-SG_top[order(SG_top$FGP,decreasing = T),]
SGtop<-head(SG_top,n=10)

SF_top<-FGP_top[FGP_top$Positions=="SF",]
SF_top<-na.omit(SF_top)
SF_top<-SF_top[order(SF_top$FGP,decreasing = T),]
SFtop<-head(SF_top,n=10)

PF_top<-FGP_top[FGP_top$Positions=="PF",]
PF_top<-na.omit(PF_top)
PF_top<-PF_top[order(PF_top$FGP,decreasing = T),]
PFtop<-head(PF_top,n=10)

data6<-rbind(Ctop,PGtop,PFtop,SFtop,SGtop)
data6<-data6[,-4]

library(stringr)
library(data.table)
library(rvest)
salary<-read_html("http://hoopshype.com/salaries/players/")
salary.name<-salary%>%html_nodes(".name a")%>% html_text()
salary.money<-salary%>%html_nodes("tbody .hh-salaries-sorted")%>% html_text()
salary.data<-data.frame(player=salary.name,salary=salary.money)

data1 <- read_csv("D:/Dropbox/UCLA/[Courses]/Stats Minor/STATS 199/new_jiahao.txt")
data1<-data1[-1,c(1:30)]
names(data1)[1]<-"SEASON_ID"
data1[,1]<-gsub("\\[\"", "", data1[,1])

data2<-data1 %>% select(PLAYER_NAME,DREB,OREB,AST,STL,BLK,TOV,PF)
data3<-data2 %>% group_by(PLAYER_NAME) %>% summarize(DREB=sum(DREB),OREB=sum(OREB),AST=sum(AST),STL=sum(BLK),TOV=sum(TOV),BLK=sum(BLK))
names(data3)[1]<-"player"

data4<-merge(data3,nbadat_valid,by="player",all.x=T)
data5<-nbadat_valid %>% select(player,FGP,Positions) %>% unique()
data7<-merge(data6,data3,by.x="player")
salary<- read_csv("D:/Dropbox/UCLA/[Courses]/Stats Minor/STATS 199/199 Research/salary.csv")
data7<-merge(data7,salary,by.x="player")
data7<-data7[,-10]
data7$REB<-data7$DREB+data7$OREB

data_PF<-data7[which(data7$Positions=="PF"),]
data_PF<-data_PF[order(data_PF$FGP,decreasing = T),]
data_PF$FGP_rank<-1:10
data_PF<-data_PF[order(data_PF$BLK,decreasing = T),]
data_PF$BLK_rank<-1:10
data_PF<-data_PF[order(data_PF$REB,decreasing = T),]
data_PF$REB_rank<-1:10


data_SG<-data7[which(data7$Positions=="SG"),]
data_SG<-data_SG[order(data_SG$FGP,decreasing = T),]
data_SG$FGP_rank<-1:10
data_SG<-data_SG[order(data_SG$AST,decreasing = T),]
data_SG$AST_rank<-1:10
data_SG<-data_SG[order(data_SG$DREB,decreasing = T),]
data_SG$DREB_rank<-1:10

data_C<-data7[which(data7$Positions=="C"),]
data_C<-data_C[order(data_C$FGP,decreasing = T),]
data_C$FGP_rank<-1:10
data_C<-data_C[order(data_C$REB,decreasing = T),]
data_C$REB_rank<-1:10
data_C<-data_C[order(data_C$BLK,decreasing = T),]
data_C$BLK_rank<-1:10

data_SF<-data7[which(data7$Positions=="SF"),]
data_SF<-data_SF[order(data_SF$FGP,decreasing = T),]
data_SF$FGP_rank<-1:10
data_SF<-data_SF[order(data_SF$DREB,decreasing = T),]
data_SF$DREB_rank<-1:10

data_PG<-data7[which(data7$Positions=="PG"),]
data_PG<-data_PG[order(data_PG$FGP,decreasing = T),]
data_PG$FGP_rank<-1:10
data_PG<-data_PG[order(data_PG$DREB,decreasing = T),]
data_PG$DREB_rank<-1:10
data_PG<-data_PG[order(data_PG$AST,decreasing = T),]
data_PG$AST_rank<-1:10

data_C$avg<-(data_C$FGP_rank+data_C$REB_rank+data_C$BLK_rank)/3
data_C<-data_C[order(data_C$avg),]
data_C$avg_rank<-1:10
plot(data_C$avg_rank,data_C$salary)
standardize<-function(x){(x)/(sd(x))}
data_C$salary<-standardize(data_C$salary)
data_C$avg<-standardize(data_C$avg)

data1_C<-data_C
data1_C$score<-data1_C$avgN/data1_C
data1_C<-data1_C[order(data1_C$score),]
data1_C$score_rank<-1:10
