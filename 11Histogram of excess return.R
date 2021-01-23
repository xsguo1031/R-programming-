# load data and pre-process data from 2001
monthdata <- read.csv("monthly return SP500.csv") #20010131 - 20191231
# floor the date so that when we calculate the real excess return, r_m and B_m can match
monthdata$DATE <- floor_date(as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d"), "month") 
trbm <- read.csv("GS1M.csv") # 1-month treasury bill rate from 20010701-20191201
trbm <- trbm[!is.na(suppressWarnings(as.numeric(as.character(trbm$GS1M)))),]
trbm$DATE <- floor_date(as.Date(trbm$DATE), "month")
trbm$bill <- as.numeric(as.character(trbm$GS1M))
#trbm <- trbm %>% filter(DATE<as.Date("2012-12-31")) # select the specific period to test
#trbm <- na.omit(trbm)
temp_month <- inner_join(trbm, monthdata, by = "DATE")
df_month <- subset(temp_month, select = c("DATE", "bill", "spindx"))

##-------define treasury bill rate--------
Bill_m <- df_month$bill/100/12
B_m <- head(Bill_m,-1)

##------ Define index level S_m------
S_m <- df_month$spindx
S_m_no_head <- tail(S_m, -1)
S_m_no_tail <- head(S_m, -1)
S_m <- S_m_no_tail

##-------define return r_m--------
r_m <- ((S_m_no_head-S_m_no_tail)/S_m_no_tail)


#-------define monthly excess return--------
e_m <- (r_m-B_m)
library(moments)
skewness(e_m)
d <- density(e_m)
plot(d,xlab="Monthly excess return",main=NA)
polygon(d, col="lightblue", border="black")
hist(e_m,
     xlim=c(min(e_m),max(e_m)), probability=T, nclass=10,
     col = "lightblue",
     xlab="Monthly excess return",main=NULL)
lines(density(e_m), col='red', lwd=3)

E_m <- as.data.frame(e_m)
E_m%>% 
  ggplot(aes(x=e_m)) +
  geom_density(fill="dodgerblue")+
  labs(x="Monthly excess return", y = "Density")+
  geom_vline(xintercept=mean(e_m), size=1.5, color="red")+
  geom_vline(xintercept=median(e_m), size=1.5, color="green")+
  geom_vline(xintercept=d$x[which.max(d$y)], size=1.5, color="gray")+
  geom_text(aes(x=mean(e_m)-0.01, y= 2, label=paste0("Mean\n",round(mean(e_m),4))),size=6)+
  geom_text(aes(x=median(e_m)+0.002, y =5, label=paste0("Median\n",round(median(e_m),4))),size=6)+
  geom_text(aes(x=d$x[which.max(d$y)]+0.01, y =2, label=paste0("Mode\n",round(d$x[which.max(d$y)],4))),size=6)+
  geom_text(aes(x = 0.08, y=15, label = paste0("Skewness = ",round(skewness(e_m),4))), size = 6)+
  theme_classic()+
  theme(axis.title=element_text(size=15,face="bold"),
        axis.text=element_text(size=12))
            
        


