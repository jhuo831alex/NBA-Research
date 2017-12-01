library(dplyr)
datanew1<-nbadat_valid %>% group_by(player) %>% select(player,FGP,Positions) %>% unique()
datanew1<-merge(datanew1,salary,by.x="player")
datanew1<-datanew1[,-4]
datanew1<-merge(datanew1,data3,by.x="player")
datanew1$REB<-datanew1$DREB+datanew1$OREB

datanew1<-datanew1[order(datanew1$FGP,decreasing = T),]
datanew1$FGP_rank<-1:nrow(datanew1)
datanew1<-datanew1[order(datanew1$DREB,decreasing = T),]
datanew1$DREB_rank<-1:nrow(datanew1)
datanew1<-datanew1[order(datanew1$OREB,decreasing = T),]
datanew1$OREB_rank<-1:nrow(datanew1)
datanew1<-datanew1[order(datanew1$AST,decreasing = T),]
datanew1$AST_rank<-1:nrow(datanew1)
datanew1<-datanew1[order(datanew1$BLK,decreasing = T),]
datanew1$BLK_rank<-1:nrow(datanew1)
datanew1<-datanew1[order(datanew1$REB,decreasing = T),]
datanew1$REB_rank<-1:nrow(datanew1)
datanew1<-datanew1[order(datanew1$STL,decreasing = T),]
datanew1$STL_rank<-1:nrow(datanew1)
datanew1<-datanew1[order(datanew1$TOV,decreasing = T),]
datanew1$TOV_rank<-nrow(datanew1):1


datanew_C<-datanew1[which(datanew1$Positions=="C"),]
datanew_C$avg<-(datanew_C$FGP_rank+datanew_C$BLK_rank+datanew_C$REB_rank)/3
datanew_C<-datanew_C[order(datanew_C$avg),]
datanew_C$avg_rank<-1:nrow(datanew_C)
standardize<-function(x){(x-min(x))/(max(x)-min(x))}
datanew_C$avg<-standardize(datanew_C$avg)
datanew_C$salary<-standardize(datanew_C$salary)
datanew_C$dis<-(datanew_C$avg)^2+(datanew_C$salary)^2
datanew_C<-datanew_C[order(datanew_C$dis),]
datanew_C$dis_rank<-1:nrow(datanew_C)
View(head(datanew_C,n=10))


datanew_PF<-datanew1[which(datanew1$Positions=="PF"),]
datanew_PF$avg<-(datanew_PF$FGP_rank+datanew_PF$BLK_rank+datanew_PF$REB_rank)/3
datanew_PF<-datanew_PF[order(datanew_PF$avg),]
datanew_PF$avg_rank<-1:nrow(datanew_PF)
datanew_PF$avg<-standardize(datanew_PF$avg)
datanew_PF$salary<-standardize(datanew_PF$salary)
datanew_PF$dis<-(datanew_PF$avg)^2+(datanew_PF$salary)^2
datanew_PF<-datanew_PF[order(datanew_PF$dis),]
datanew_PF$dis_rank<-1:nrow(datanew_PF)
View(head(datanew_PF,n=10))

datanew_SG<-datanew1[which(datanew1$Positions=="SG"),]
datanew_SG$avg<-(datanew_SG$FGP_rank+datanew_SG$AST_rank+datanew_SG$DREB_rank)/3
datanew_SG<-datanew_SG[order(datanew_SG$avg),]
datanew_SG$avg_rank<-1:nrow(datanew_SG)
datanew_SG$avg<-standardize(datanew_SG$avg)
datanew_SG$salary<-standardize(datanew_SG$salary)
datanew_SG$dis<-(datanew_SG$avg)^2+(datanew_SG$salary)^2
datanew_SG<-datanew_SG[order(datanew_SG$dis),]
datanew_SG$dis_rank<-1:nrow(datanew_SG)
View(head(datanew_SG,n=10))

datanew_PG<-datanew1[which(datanew1$Positions=="PG"),]
datanew_PG$avg<-(datanew_PG$FGP_rank+datanew_PG$AST_rank+datanew_PG$DREB_rank)/3
datanew_PG<-datanew_PG[order(datanew_PG$avg),]
datanew_PG$avg_rank<-1:nrow(datanew_PG)
datanew_PG$avg<-standardize(datanew_PG$avg)
datanew_PG$salary<-standardize(datanew_PG$salary)
datanew_PG$dis<-(datanew_PG$avg)^2+(datanew_PG$salary)^2
datanew_PG<-datanew_PG[order(datanew_PG$dis),]
datanew_PG$dis_rank<-1:nrow(datanew_PG)
View(head(datanew_PG,n=10))

datanew_SF<-datanew1[which(datanew1$Positions=="SF"),]
datanew_SF$avg<-(datanew_SF$FGP_rank+datanew_SF$DREB_rank)/2
datanew_SF<-datanew_SF[order(datanew_SF$avg),]
datanew_SF$avg_rank<-1:nrow(datanew_SF)
datanew_SF$avg<-standardize(datanew_SF$avg)
datanew_SF$salary<-standardize(datanew_SF$salary)
datanew_SF$dis<-(datanew_SF$avg)^2+(datanew_SF$salary)^2
datanew_SF<-datanew_SF[order(datanew_SF$dis),]
datanew_SF$dis_rank<-1:nrow(datanew_SF)
View(head(datanew_SF,n=10))

library(ggplot2)
datanew_C$highlight <- ifelse(datanew_C$player == datanew_C$player[1], "highlight", "normal")
textdf <- datanew_C[datanew_C$player == datanew_C$player[1], ]
mycolours <- c("highlight" = "red", "normal" = "grey50")
ggplot(data = datanew_C, aes(x = avg, y = salary)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = mycolours) +
  geom_text(data = textdf, aes(x = avg * 1.02, y = salary, label = "my best player")) +
  theme(legend.position = "none") +
  theme()+
  labs(title="Center")

datanew_PF$highlight <- ifelse(datanew_PF$player == datanew_PF$player[1], "highlight", "normal")
textdf <- datanew_PF[datanew_PF$player == datanew_PF$player[1], ]
mycolours <- c("highlight" = "red", "normal" = "grey50")
ggplot(data = datanew_PF, aes(x = avg, y = salary)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = mycolours) +
  geom_text(data = textdf, aes(x = avg * 1.02, y = salary, label = "my best player")) +
  theme(legend.position = "none") +
  theme()+
  labs(title="Power Forward")

datanew_SG$highlight <- ifelse(datanew_SG$player == datanew_SG$player[1], "highlight", "normal")
textdf <- datanew_SG[datanew_SG$player == datanew_SG$player[1], ]
mycolours <- c("highlight" = "red", "normal" = "grey50")
ggplot(data = datanew_SG, aes(x = avg, y = salary)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = mycolours) +
  geom_text(data = textdf, aes(x = avg * 1.02, y = salary, label = "my best player")) +
  theme(legend.position = "none") +
  theme()+
  labs(title="Shooting Guard")

datanew_PG$highlight <- ifelse(datanew_PG$player == datanew_PG$player[1], "highlight", "normal")
textdf <- datanew_PG[datanew_PG$player == datanew_PG$player[1], ]
mycolours <- c("highlight" = "red", "normal" = "grey50")
ggplot(data = datanew_PG, aes(x = avg, y = salary)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = mycolours) +
  geom_text(data = textdf, aes(x = avg * 1.02, y = salary, label = "my best player")) +
  theme(legend.position = "none") +
  theme()+
  labs(title="Point Guard")

datanew_SF$highlight <- ifelse(datanew_SF$player == datanew_SF$player[1], "highlight", "normal")
textdf <- datanew_SF[datanew_SF$player == datanew_SF$player[1], ]
mycolours <- c("highlight" = "red", "normal" = "grey50")
ggplot(data = datanew_SF, aes(x = avg, y = salary)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = mycolours) +
  geom_text(data = textdf, aes(x = avg * 1.02, y = salary, label = "my best player")) +
  theme(legend.position = "none") +
  theme()+
  labs(title="Small Forward")


