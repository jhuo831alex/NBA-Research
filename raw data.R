library(readr)
data1 <- read_csv("D:/Dropbox/UCLA/[Courses]/Stats Minor/STATS 199/new_jiahao.txt")
data1<-data1[-1,c(1:30)]
names(data1)[1]<-"SEASON_ID"
data1[,1]<-gsub("\\[\"", "", data1[,1])
data1[,1]<-as.numeric(gsub("\"", "", data1[,1]))
data1<-data1[,c(-1,-4,-6)]

allstar <- read_excel("D:/Dropbox/UCLA/[Courses]/Stats Minor/STATS 199/allstar.xls")
library(lubridate)
second<-vector(length=nrow(allstar))
for(i in 1:nrow(allstar)){
  second[i]<-period_to_seconds(ms(allstar[i,7]))
}
library("tibble")
allstar<-add_column(allstar,seconds=second,.before=8)
for(i in 9:26){allstar[,i]<-as.numeric(allstar[[i]])}

kobe <- read_excel("D:/Dropbox/UCLA/[Courses]/Stats Minor/STATS 199/kobe.xls")
for(i in 13:30){kobe[,i]<-as.numeric(kobe[[i]])}
kobe[,5]<-as.numeric(kobe[[5]])
second_kobe<-vector(length=nrow(kobe))
for(i in 1:nrow(kobe)){
  second_kobe[i]<-period_to_seconds(ms(kobe[i,12]))
}
kobe<-add_column(kobe,seconds=second_kobe,.before=13)
plot(kobe$age,kobe$fgp)
cor<-cor(kobe[,c(13:31)])
corrplot::corrplot(cor)


