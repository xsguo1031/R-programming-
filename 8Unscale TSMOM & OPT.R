## --------------- Check the difference in two strategies -------------------
setwd("~/Desktop/Paper2/Paper2 code&data for geometric return/")
source("~/Desktop/Paper2/Paper2 code&data for geometric return/0Rank.R")
source("~/Desktop/Paper2/Paper2 code&data for geometric return/1Time Series Momentum Strategy.R")
source("~/Desktop/Paper2/Paper2 code&data for geometric return/3scenario.R")
source("~/Desktop/Paper2/Paper2 code&data for geometric return/4Optimization.R")
source("~/Desktop/Paper2/Paper2 code&data for geometric return/7Risk Parity.R")

T = 12
F = 1
G = 12 # when using ewma or equal weight to calculate vol

# # load data and pre-process data from 2001
# monthdata <- read.csv("monthly return SP500.csv") #20010131 - 20191231
# 
# # check
# if (G !=12){
#   monthdata <- tail(monthdata, (G-12))
# }
# 
# monthdata$DATE <- floor_date(as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d"), "month")
# trbm <- read.csv("GS1M.csv") # 1-month treasury bill rate from 20010701-20191201
# 
# # check
# if (G !=12){
#   trbm <- tail(trbm, (G-12))
# }
# 
# 
# trbm <- trbm[!is.na(suppressWarnings(as.numeric(as.character(trbm$GS1M)))),]
# trbm$DATE <- floor_date(as.Date(trbm$DATE), "month")
# trbm$bill <- as.numeric(as.character(trbm$GS1M))
# temp_month <- inner_join(trbm, monthdata, by = "DATE")
# df_month <- subset(temp_month, select = c("DATE", "bill", "spindx"))
# 
# ##-------define treasury bill rate--------
# Bill_m <- df_month$bill/100/12
# B_m <- head(Bill_m,-1)
# #B_m <- rep(0,length(B_m))
# 
# ##------ Define index level S_m------
# S_m <- df_month$spindx
# S_m_no_head <- tail(S_m, -1)
# S_m_no_tail <- head(S_m, -1)
# S_m <- S_m_no_tail
# 
# ##-------define return r_m--------
# r_m <- ((S_m_no_head-S_m_no_tail)/S_m_no_tail)


# load data from 1998-12-01 (2001/01/01 - T-G-1)
monthdata <- read.csv("1926SP.csv")
monthdata$DATE <- as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d")
monthdata <- monthdata %>% filter( (DATE>as.Date("1998-12-01")) & (DATE<as.Date("2019-12-31")))
trbm <- read.csv("1926T.csv")
trbm$DATE <- as.Date(as.character.Date(trbm$caldt),format = "%Y %m %d")
trbm <- trbm %>% filter( (DATE>as.Date("1998-12-01")) & (DATE<as.Date("2019-12-31")))
C1926 <- inner_join(trbm, monthdata, by = "DATE")
df_month <- subset(C1926, select = c("DATE", "t30ret", "spindx"))

B_m <- head(df_month$t30ret,-1)

S_m <- df_month$spindx
S_m_no_head <- tail(S_m, -1)
S_m_no_tail <- head(S_m, -1)
S_m <- S_m_no_tail
r_m <- ((S_m_no_head-S_m_no_tail)/S_m_no_tail)


# TSMOM is equal weight average of historical excess returns
# OPT is exponential weight of historical excess returns 
# We let both strategies take only +1 or -1 and see the difference
# test <- TSMOM(freq = "monthly", ret=r_m[(G+2-F):length(r_m)], T=12, F=1, b=B_m[(G+2-F):length(r_m)], scale = 100)$position
# test2 <- TSMOM(freq = "monthly", logret=r_m[(G+2-F):length(r_m)], T=12, F=1, b=0, scale = stat[4], mom = "ewma")$position
long <- rep(1,length(r_m[(G+2-F):length(r_m)])) # buy and hold strategy 
test <- TSMOM_unscale(freq = "monthly", ret=r_m[(G+2-F):length(r_m)], T=12, F=1, b=B_m[(G+2-F):length(r_m)], mom = "ew")$position
#sum(test[(T+F-1):length(test)] != weight[[1]])/length(weight[[1]]) #difference in percentage 
test2 <- TSMOM_unscale(freq = "monthly", ret=r_m[(G+2-F):length(r_m)], T=12, F=1, b=B_m[(G+2-F):length(r_m)], mom = "wma")$position
test3 <- TSMOM_unscale(freq = "monthly", ret=r_m[(G+2-F):length(r_m)], T=12, F=1, b=B_m[(G+2-F):length(r_m)], mom = "ewma")$position

test4 <- TSMOM_unscale(freq = "monthly", ret=r_m[(G+2-F):length(r_m)], T=12, F=1, b=B_m[(G+2-F):length(r_m)], mom = "TSMOM")$position
real_sign <- ifelse((r_m[(T+G+1):length(r_m)]-B_m[(T+G+1):length(r_m)]) > 0, 1, -1)
#DIFF <- as.data.frame(cbind(test[(T+F-1):length(test)], test2[(T+F-1):length(test2)], long[(T+F-1):length(test2)]))
DIFF <- as.data.frame(cbind(test[(T+F-1):length(test)], test2[(T+F-1):length(test2)], test3[(T+F-1):length(test2)], test4[(T+F-1):length(test2)], real_sign))

colnames(DIFF) <- c("Simple moving average", "Weighted moving average", "Exponential moving average", "Time series momentum", "Actual excess return")
DIFF$DATE <- trbm$DATE[(T+G+1):length(r_m)]
head(DIFF)
D <- DIFF %>% gather(key = "Trading Signal", value = "w*", `Simple moving average`:`Actual excess return`)
D$`w*` <- as.factor(D$`w*`)
head(D)

D %>% ggplot(aes(x = DATE, y = `Trading Signal`, group = `Trading Signal`, colour = `w*`)) +
  geom_point() + 
  scale_color_manual(values = c("red", "green"))+
  ylab("Trading Signal")+
  theme(legend.text = element_text(size=10, face="bold"), 
        legend.title = element_text(size=10, face="bold"),
        axis.title=element_text(size=12,face="bold"),
        axis.text=element_text(size=11),
        strip.text.x = element_text(size = 11, face = "bold"),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(colour="w*") 

excess_sign <- as.data.frame((DIFF[,1:4]) * (r_m[(T+G+1):length(r_m)]-B_m[(T+G+1):length(r_m)])) # excess has more 1 means the strategy is better 
excess_sign$`Passive long` <- (r_m[(T+G+1):length(r_m)]-B_m[(T+G+1):length(r_m)])
absolute_sign <- as.data.frame(cumsum(log(1+excess_sign+B_m[(T+G+1):length(r_m)]))) 
# see which strategies can provide more positive excess returns 
count <- excess_sign %>% gather(key = "Trading Signal", value = "Excess Sign",  `Simple moving average`:`Passive long`) %>% 
  group_by(`Trading Signal`) %>%  
  summarise(postive_count = sum(`Excess Sign`>0)/(length(r_m)-T-G)*100,
            negative_count = sum(`Excess Sign`<0)/(length(r_m)-T-G)*100)
# plot which strategies can provide more positive absolute returns 
absolute_sign$DATE <- DIFF$DATE
E <- absolute_sign %>% gather(key = "Trading Signal", value = "Absolute Sign",  `Simple moving average`:`Passive long`)
E$`Trading Signal`[E$`Trading Signal` == "Passive long"] = "Buy-and-hold"
E %>% ggplot(aes(x = DATE, y = `Absolute Sign`, group = `Trading Signal`, colour = `Trading Signal`)) + 
  geom_line() + ylab("Cumulative Absolute Return (log scale)")+
  labs(color='Trading Signal') +
  theme(legend.text = element_text(size=10, face="bold"), 
        legend.title = element_text(size=10, face="bold"),
        axis.title=element_text(size=12,face="bold"),
        axis.text=element_text(size=11),
        strip.text.x = element_text(size = 11, face = "bold"),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
#ggtitle("Sign of Excess Return when w can only take +1 or -1") +
# scale_color_gradient2(low = "red", mid = "white", high = "green",
#midpoint=median(E$`Excess Sign`),breaks=seq(from=round(min(E$`Excess Sign`), digits = 2),to=round(max(E$`Excess Sign`), digits=2),by=round((max(E$`Excess Sign`)-min(E$`Excess Sign`))/10, digits = 2))) + 
#theme(legend.position="bottom", legend.key.height=unit(1,"line"), legend.key.width=unit(6,"line"))

# count the number of positive return rate 
sum((r_m[(T+G+1):length(r_m)]-B_m[(T+G+1):length(r_m)])>0)

DIFF$DATE[DIFF$`Simple moving average`!=DIFF$`Time series momentum`]
absolute_sign$DATE[absolute_sign$`Time series momentum` != absolute_sign$`Simple moving average`]
