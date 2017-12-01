library(dplyr)
dat_FGP <- read_csv("D:/Dropbox/UCLA/[Courses]/Stats Minor/STATS 199/15-16/[10-20-2015]-[06-20-2016]-combined-stats.csv")
dat3 <-filter(dat_FGP,event_type=="miss"|event_type=="shot")
dat2<-as.data.frame(dat3 %>% group_by(player) %>% summarise(FGA=n(),FGM=sum(!(event_type=="miss")),FGP=FGM/FGA))
dat2<-dat2[dat2$FGA>=300,]
hist(dat2$FGP,breaks=100)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
removed_FGP<-remove_outliers(dat2$FGP)
dat2$FGP_no_outliers<-removed_FGP
mean_FGP<-mean(dat2$FGP_no_outliers,na.rm=T)
sd_FGP<-sd(dat2$FGP_no_outliers,na.rm=T)
normal<-function(x){(1/(sqrt(2*pi)*sd_FGP))*exp(-(x-mean_FGP)^2)/(2*sd_FGP^2)}
removed_FGP<-removed_FGP[!is.na(removed_FGP)]
mean_FGP<-mean(dat2$FGP_no_outliers,na.rm=T)
sd_FGP<-sd(dat2$FGP_no_outliers,na.rm=T)
hist(removed_FGP,prob=TRUE，breaks=30)
abline(v=0.4922521)
length(removed_FGP[which(removed_FGP>=0.4922521)])/length(removed_FGP)
removed_FGP>=0.4922521
curve(normal,from=0,to=1,add=TRUE)
mean(dat2$FGP_no_outliers,na.rm=T)+sd(dat2$FGP_no_outliers,na.rm=T)
mean(dat2$FGP_no_outliers,na.rm=T)-sd(dat2$FGP_no_outliers,na.rm=T)
#add position of players
#add rebound to the data

write.csv(dat_FGP,"data.csv")
vec<-grep("Dunk",dat_FGP$type,ignore.case = T)
dat_nodunk<-dat_FGP[-vec,]
vec2<-grep("Free Throw",dat_nodunk$type,ignore.case = T)
dat_nono<-dat_nodunk[-vec2,]
table(dat_nono$type)
dat_freethrow<-dat_nodunk[vec2,]
dat4 <-filter(dat_nono,result=="missed"|result=="made")
library(dplyr)

dat_nonona<-dat_nono[!is.na(dat_nono$result),]
newdat1<-as.data.frame(dat_nonona %>% group_by(player) %>% summarise(FGA=n(),FGM=sum(!(result=="made")),FGP=FGM/FGA))
newdat1<-newdat1[newdat1$FGA>=300,]
hist(newdat1$FGP,breaks=100)
length(newdat1$FGP[which(newdat1$FGP>=0.4922521)])/length(newdat1$FGP)


table(dat_FGP$type)

library(rvest)
library(data.table)
mainpage_SG<-read_html("http://www.espn.com/nba/players/_/position/sg")
SG_name<-mainpage_SG %>% html_nodes("td:nth-child(1) a") %>% html_text()
positions_SG<-data.frame(Names=SG_name,Positions="SG")
mainpage_PG<-read_html("http://www.espn.com/nba/players/_/position/pg/league/nba")
PG_name<-mainpage_PG %>% html_nodes("td:nth-child(1) a") %>% html_text()
positions_PG<-data.frame(Names=PG_name,Positions="PG")
mainpage_SF<-read_html("http://www.espn.com/nba/players/_/position/sf/league/nba")
SF_name<-mainpage_SF %>% html_nodes("td:nth-child(1) a") %>% html_text()
positions_SF<-data.frame(Names=SF_name,Positions="SF")
mainpage_PF<-read_html("http://www.espn.com/nba/players/_/position/pf/league/nba")
PF_name<-mainpage_PF %>% html_nodes("td:nth-child(1) a") %>% html_text()
positions_PF<-data.frame(Names=PF_name,Positions="PF")
mainpage_C<-read_html("http://www.espn.com/nba/players/_/position/c/league/nba")
C_name<-mainpage_C %>% html_nodes("td:nth-child(1) a") %>% html_text()
positions_C<-data.frame(Names=C_name,Positions="C")
positions<-rbind(positions_C,positions_PF,positions_PG,positions_SF,positions_SG)

dat510_nona<-dat_FGP[which(!is.na(dat_FGP$player)),]
library(stringr)
switchn<-function(x) {
  a<-str_split(x,", ")[[1]][1]
  b<-str_split(x,", ")[[1]][2]
  paste(b,a)
}


positions$Names<-apply(as.data.frame(positions$Names),1,FUN=switchn)
names(positions)[1]<-"player"
data510<-merge(dat_FGP,positions,all.x=T)
data510<-data510[which(!is.na(data510$player)),]
vec<-grep("Dunk",data510$type,ignore.case = T)
data510<-data510[-vec,]
vec2<-grep("Free Throw",data510$type,ignore.case = T)
data510<-data510[-vec2,]
table(data510$type)
write.csv(data510,"data510.csv")N
data510<-data510[!is.na(data510$result),]
