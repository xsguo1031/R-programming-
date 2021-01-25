library(tidyverse)
library(PerformanceAnalytics)
library(xts)
#library(MFTSR) # for ewma vol
library(zoo)
library(data.table) # for combine the dataframe to get the solution
library(xts) # to calculate maximum drawdown
library(RiskPortfolios)
library(ggrepel)
library(RColorBrewer)
start_time <- Sys.time()

setwd("~/Desktop/")
source("/Users/guoxiaoshi/Desktop/Paper2/Paper2 code&data for geometric return/0Rank.R")
source("/Users/guoxiaoshi/Desktop/Paper2/Paper2 code&data for geometric return/1Time Series Momentum Strategy.R")
source("/Users/guoxiaoshi/Desktop/Paper2/Paper2 code&data for geometric return/3scenario.R")
source("/Users/guoxiaoshi/Desktop/Paper2/Paper2 code&data for geometric return/4Optimization.R")
source("/Users/guoxiaoshi/Desktop/Paper2/Paper2 code&data for geometric return/7Risk Parity.R")
source("/Users/guoxiaoshi/Desktop/Paper2/Paper2 code&data for geometric return/9Mean-Variance.R")

T = 12
F = 1
g = 10 # when using ewma or equal weight to calculate vol
G = 10
A = 0.99 # alpha for CVaR
if (A == 0.75){q = 1} else if (A == 0.90){q = 2} else if (A == 0.99){q = 3} # for TSMDR

##------------Calculate C* of TSMDR---------------------------------------
monthdata <- read.csv("1926SP.csv") #19260130 - 20191231
monthdata$DATE <- as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d")
monthdata <- monthdata %>% filter( (DATE>as.Date("1979-12-01")) & (DATE<as.Date("2000-12-31"))) #228

S_t <- monthdata$spindx
S_t_no_head <- tail(S_t, -1) 
S_t_no_tail <- head(S_t, -1)
S_t <- S_t_no_tail
r_m <- ((S_t_no_head-S_t_no_tail)/S_t_no_tail)

Trea1926 <- read.csv("1926T.csv")
Trea1926$DATE <- as.Date(as.character.Date(Trea1926$caldt),format = "%Y %m %d")
C1926 <- inner_join(Trea1926, monthdata, by = "DATE")
df_month1982 <- subset(C1926, select = c("DATE", "t30ret", "spindx"))
B_t <- head(df_month1982$t30ret,-1)

# G+2-F is corresponding to (T+G+1)-(T+F-1). Since OPT starts from T+G+1, TSMOM should start from G+2-F to be consisten
annual_vol <- sqrt(12)*sd(r_m[(G+2-F):length(r_m)]-B_t[(G+2-F):length(B_t)])  # use sample std
ANNUAL_r_mean <- ewma.filter((r_m[(G+2-F):length(r_m)]-B_t[(G+2-F):length(B_t)]), (1-3/4))
ANNUAL_vol <- sqrt(12*ewma.filter(((r_m[(G+2-F):length(r_m)]-B_t[(G+2-F):length(B_t)])-ANNUAL_r_mean)^2, (1-3/4))) # use ewma std
stat <- summary(ANNUAL_vol[2:length(ANNUAL_vol)]) # stat[3] is median, and stat[4] is mean

scen<- Scenario(monthdata = df_month1982, G = G, iteration = 20000, seed = 999, mov_average = "wma")
i = 0
Mstat <- list()
for (AV in c(0.75, 0.90, 0.99)){
  i = i+1
  Mstat[[i]] = summary(CVaR((-scen+B_t[(T+G+1):length(B_t)]), alpha = AV))
}



##-----------------------------Start to load data for generation and four strategies---------------------------------
# load data from 1998-12-01 = 2001-01-01-T-G-1
# the starting point should be changed according to G
monthdata <- read.csv("1926SP.csv")
monthdata$DATE <- as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d")
trbm <- read.csv("1926T.csv")
trbm$DATE <- as.Date(as.character.Date(trbm$caldt),format = "%Y %m %d")
if (G == 6){
  monthdata <- monthdata %>% filter( (DATE>as.Date("1999-06-01")) & (DATE<as.Date("2019-12-31")))
  trbm <- trbm %>% filter( (DATE>as.Date("1999-06-01")) & (DATE<as.Date("2019-12-31")))
} else if (G == 8){
  monthdata <- monthdata %>% filter( (DATE>as.Date("1999-04-01")) & (DATE<as.Date("2019-12-31")))
  trbm <- trbm %>% filter( (DATE>as.Date("1999-04-01")) & (DATE<as.Date("2019-12-31")))
} else if (G == 10){
  monthdata <- monthdata %>% filter( (DATE>as.Date("1999-02-01")) & (DATE<as.Date("2019-12-31")))
  trbm <- trbm %>% filter( (DATE>as.Date("1999-02-01")) & (DATE<as.Date("2019-12-31")))
} else if (G == 12){
  monthdata <- monthdata %>% filter( (DATE>as.Date("1998-12-01")) & (DATE<as.Date("2019-12-31")))
  trbm <- trbm %>% filter( (DATE>as.Date("1998-12-01")) & (DATE<as.Date("2019-12-31")))
}


C1926 <- inner_join(trbm, monthdata, by = "DATE")
df_month <- subset(C1926, select = c("DATE", "t30ret", "spindx"))

B_m <- head(df_month$t30ret,-1)

S_m <- df_month$spindx
S_m_no_head <- tail(S_m, -1)
S_m_no_tail <- head(S_m, -1)
S_m <- S_m_no_tail
r_m <- ((S_m_no_head-S_m_no_tail)/S_m_no_tail)



##----------------TSMOM-----------------------
# G+2-F is corresponding to (T+G+1)-(T+F-1)
test <- list()
excess_m <- list()
abs_m <- list()
annual_excess_return_m <- numeric()
annual_sd_m <- numeric()
annual_sharpe_ratio_m <- numeric()
annual_sortino_ratio_m  <- numeric()
annual_conditional_sharpe_m <- numeric()
mdd_m <- numeric()

s = 0
for (scale in c(stat[2], stat[3], stat[5], 100)){
  s = s+1
  test[[s]] <- TSMOM(freq = "monthly", 
                     ret=r_m[(G+2-F):length(r_m)], 
                     T=12, 
                     F=1, 
                     b=B_m[(G+2-F):length(r_m)], 
                     scale = scale)
  excess_m[[s]] <- test[[s]]$avg_excess 
  excess_m[[s]] <- na.omit(excess_m[[s]])
  abs_m[[s]] <- test[[s]]$avg_abs
  abs_m[[s]] <- na.omit(abs_m[[s]])
  
  ##-------calculate performance metric--------
  # Notice: since this is log excess return, we should not use cumulative product
  annual_excess_return_m[s] = 12*mean(excess_m[[s]])
  annual_sd_m[s] = sqrt(12)*sd(excess_m[[s]])
  annual_sharpe_ratio_m[s] = annual_excess_return_m[s]/annual_sd_m[s]
  annual_sortino_ratio_m[s] = sqrt(12)*SortinoRatio(abs_m[[s]], MAR = mean(B_m[(T+G+1):length(B_m)]))
  annual_conditional_sharpe_m[s] <- sqrt(12)*SharpeRatio(
    R = xts(excess_m[[s]], trbm$DATE[(T+G+1):length(r_m)]),
    Rf = 0,
    p = A,
    FUN = "ES"
  )
  
  # To find the maximum drawdown in a return series, 
  # we need to first calculate the cumulative returns and the maximum cumulative return to that point. 
  # Any time the cumulative returns dips below the maximum cumulative returns, it's a drawdown. Drawdowns are measured as a percentage of that maximum cumulative return, in effect, measured from peak equity.
  mdd_m[s] = maxDrawdown(xts(abs_m[[s]], trbm$DATE[(T+G+1):length(r_m)]),geometric=TRUE)
}




##------------ generate scenarios for MV, MCVAR, TSMDR------------------
scenario <- Scenario(monthdata = df_month, G = g, iteration = 20000, seed = 999, mov_average = "wma")



##------------MV-------------------------------
mv <- list()
excess_mv <- list()
abs_mv <- list()
annual_excess_return_mv <- numeric()
annual_sd_mv <- numeric()
annual_sharpe_ratio_mv <- numeric()
annual_sortino_ratio_mv  <- numeric()
annual_conditional_sharpe_mv <- numeric()
mdd_mv <- numeric()

s = 1
Lambda <- c(0.02, 0.04, 0.06, 0.08, 0.1)
for (lambda in Lambda){
  mv[[s]] <- MV3(scenario = scenario,
                B_t = B_m[(T+G+1):length(B_m)], # when using ewma or equal weight to calculate vol
                lambda =  lambda)
  
  excess_mv[[s]] <- mv[[s]] *as.vector((r_m[(T+G+1):length(r_m)] - B_m[(T+G+1):length(B_m)])) # get the excess return of each month 
  excess_mv[[s]] <- na.omit(excess_mv[[s]])
  abs_mv[[s]] <- excess_mv[[s]] + B_m[(T+G+1):length(B_m)]  # get the absolute return of each month
  abs_mv[[s]] <- na.omit(abs_mv[[s]])
  
  ##-------calculate performance metric--------
  # Notice: since this is log excess return, we should not use cumulative product
  annual_excess_return_mv[s] = 12*mean(excess_mv[[s]])
  annual_sd_mv[s] = sqrt(12)*sd(excess_mv[[s]])
  annual_sharpe_ratio_mv[s] = annual_excess_return_mv[s]/annual_sd_mv[s]
  annual_sortino_ratio_mv[s] = sqrt(12)*SortinoRatio(abs_mv[[s]], MAR = mean(B_m[(T+G+1):length(B_m)]))
  annual_conditional_sharpe_mv[s] <- sqrt(12)*SharpeRatio(
    R = xts(excess_mv[[s]], trbm$DATE[(T+G+1):length(r_m)]),
    Rf = 0,
    p = A,
    FUN = "ES"
  )
  
  # To find the maximum drawdown in a return series, 
  # we need to first calculate the cumulative returns and the maximum cumulative return to that point. 
  # Any time the cumulative returns dips below the maximum cumulative returns, it's a drawdown. Drawdowns are measured as a percentage of that maximum cumulative return, in effect, measured from peak equity.
  mdd_mv[s] = maxDrawdown(xts(abs_mv[[s]], trbm$DATE[(T+G+1):length(r_m)]), geometric=TRUE)
  s = s+1
}  





#------------Mean-CVaR-------------------------------
weight <- list()
abs_opt <- list()
excess_opt <- list()
annual_excess_return_opt <- numeric()
annual_sd_opt <- numeric()
annual_sharpe_ratio_opt <- numeric()
annual_sortino_ratio_opt <- numeric()
cum_OPT <- list()
mdd_OPT <- numeric()
annual_conditional_sharpe_OPT <- numeric()
i = 1
Lambda2 <- c(0, 0.02, 0.04, 0.06, 0.08, 0.1)
for (lambda in Lambda2){
  weight[[i]] <- OPT(r= scenario, f=B_m[(T+G+1):length(B_m)], lambda = lambda, alpha = A)  
  
  excess_opt[[i]] <- weight[[i]] *as.vector((r_m[(T+G+1):length(r_m)] - B_m[(T+G+1):length(B_m)])) # get the excess return of each month 
  abs_opt[[i]] <- excess_opt[[i]] + B_m[(T+G+1):length(B_m)]  # get the absolute return of each month 
  
  annual_excess_return_opt[i] = 12*mean(excess_opt[[i]])
  annual_sd_opt[i] = sqrt(12)*sd(excess_opt[[i]])
  annual_sharpe_ratio_opt[i] = annual_excess_return_opt[i]/annual_sd_opt[i]
  annual_sortino_ratio_opt[i] = sqrt(12)*SortinoRatio(abs_opt[[i]], MAR = mean(B_m[(T+G+1):length(B_m)]))
  cum_OPT[[i]] <- cumsum(log(1+abs_opt[[i]]))
  mdd_OPT[i] = maxDrawdown(xts(abs_opt[[i]], trbm$DATE[(T+G+1):length(r_m)]),geometric=TRUE)
  annual_conditional_sharpe_OPT[i] <- sqrt(12)*SharpeRatio(
    R = xts(excess_opt[[i]], trbm$DATE[(T+G+1):length(r_m)]),
    Rf = 0,
    p = A,
    FUN = "ES"
  )
  
  i= i + 1
  
}




###---------------------------TSMDR-----------------------------
result <- list()
Mtest <- list()
Mexcess_m <- list()
Mabs_m <- list()
Mannual_excess_return_m <- numeric()
Mannual_sd_m <- numeric()
Mannual_sharpe_ratio_m <- numeric()
Mannual_sortino_ratio_m  <- numeric()
Mmdd_m <- numeric()
Mannual_conditional_sharpe_m <- numeric()

s = 0
for (scale in c(Mstat[[q]][2], Mstat[[q]][3], Mstat[[q]][5])){
  s = s+1
  print(s)
  result[[s]] <- MCVaR(scenario=scenario, 
                       alpha = A, 
                       ret=r_m[(T+G+1):length(r_m)], 
                       b=B_m[(T+G+1):length(r_m)], 
                       scale = scale)
  Mtest[[s]] <- result[[s]]$position
  Mexcess_m[[s]] <- result[[s]]$avg_excess 
  Mexcess_m[[s]] <- na.omit(Mexcess_m[[s]])
  Mabs_m[[s]] <- result[[s]]$avg_abs
  Mabs_m[[s]] <- na.omit(Mabs_m[[s]])
  
  ##-------calculate performance metric--------
  # Notice: since this is log excess return, we should not use cumulative product
  Mannual_excess_return_m[s] = 12*mean(Mexcess_m[[s]])
  Mannual_sd_m[s] = sqrt(12)*sd(Mexcess_m[[s]])
  Mannual_sharpe_ratio_m[s] = Mannual_excess_return_m[s]/Mannual_sd_m[s]
  Mannual_sortino_ratio_m[s] = sqrt(12)*SortinoRatio(Mabs_m[[s]], MAR = mean(B_m[(T+G+1):length(B_m)]))
  
  # To find the maximum drawdown in a return series, 
  # we need to first calculate the cumulative returns and the maximum cumulative return to that point. 
  # Any time the cumulative returns dips below the maximum cumulative returns, it's a drawdown. Drawdowns are measured as a percentage of that maximum cumulative return, in effect, measured from peak equity.
  McData <- xts(Mabs_m[[s]], trbm$DATE[(T+G+1):length(r_m)])
  Mmdd_m[s] = maxDrawdown(McData,geometric=TRUE)
  Mannual_conditional_sharpe_m[s] <- sqrt(12)*SharpeRatio(
    R = xts(Mexcess_m[[s]], trbm$DATE[(T+G+1):length(r_m)]),
    Rf = 0,
    p = A,
    FUN = "ES"
  )
}




#----------------- Time series plot for cumulative absolute returns -------------
cum_TSMOM <- list()
cum_MCVAR <- list()
cum_MV <- list()
for (s in 1:4){
  cum_TSMOM[[s]] <- cumsum(log(1+abs_m[[s]]))
}
for (s in 1:length(Lambda)){
  cum_MV[[s]] <- cumsum(log(1+abs_mv[[s]]))
}
for (s in 1:3){
  cum_MCVAR[[s]] <- cumsum(log(1+Mabs_m[[s]]))
} 

Cum_MCVAR <- (as.data.frame(cum_MCVAR))
Cum_TSMOM <- (as.data.frame(cum_TSMOM))
Cum_MV <- (as.data.frame(cum_MV))
Cum_OPT <- (as.data.frame(cum_OPT)) #convert list to dataframe
Cum_Return <- cbind(Cum_TSMOM, Cum_MCVAR, Cum_MV, Cum_OPT)
#Cum_Return$Long <- cumsum(log(1+r_m[(T+G+1):length(r_m)]))
colnames(Cum_Return) <- c("C at Q1","C at Q2", "C at Q3", "Unscaled TSMOM",
                          "C* at Q1","C* at Q2", "C* at Q3",
                          paste("MV","\u03BB",  " = 0.02"),
                          paste("MV","\u03BB",  " = 0.04"),
                          paste("MV","\u03BB",  " = 0.06"),
                          paste("MV","\u03BB",  " = 0.08"),
                          paste("MV","\u03BB",  " = 0.1"),
                          paste("\u03BB",  " = 0"),
                          paste("MC","\u03BB",  " = 0.02"),
                          paste("MC","\u03BB",  " = 0.04"),
                          paste("MC","\u03BB",  " = 0.06"),
                          paste("MC","\u03BB",  " = 0.08"),
                          paste("MC","\u03BB",  " = 0.1")) # use the lambda value as the column name
# all.equal(cum_OPT[[1]],Cum_Return$`lambda = 0`) # to verify the list is unlisted correctly
Cum_Return$date <- trbm$DATE[(T+G+1):length(r_m)]
Cum <- Cum_Return %>% 
  gather(key = "Strategy", value = "cumulative absolute returns", "C at Q1": paste("MC","\u03BB",  " = 0.1"))

Cum$mysize <- rep(1.5,nrow(Cum))
Cum$mysize[Cum$Strategy=="C at Q1"|Cum$Strategy=="C at Q2"|Cum$Strategy=="C at Q3"] <- 1

Cum$Group[Cum$Strategy=="C at Q1"|Cum$Strategy=="C at Q2"|Cum$Strategy=="C at Q3"] <- "TSMOM"
Cum$Group[Cum$Strategy=="C* at Q1"|Cum$Strategy=="C* at Q2"|Cum$Strategy=="C* at Q3"] <- "TSMDR"
Cum$Group[ Cum$Strategy== paste("MC","\u03BB",  " = 0.02")|Cum$Strategy== paste("MC","\u03BB",  " = 0.04")| Cum$Strategy== paste("MC","\u03BB",  " = 0.06")| Cum$Strategy==  paste("MC","\u03BB",  " = 0.08") | Cum$Strategy== paste("MC","\u03BB",  " = 0.1")] <- "Mean-CVaR"
Cum$Group[Cum$Strategy== "Unscaled TSMOM"|Cum$Strategy== paste("\u03BB",  " = 0")] <- "Risk-Neutral"
Cum$Group[ Cum$Strategy== paste("MV","\u03BB",  " = 0.02") |Cum$Strategy== paste("MV","\u03BB",  " = 0.04")| Cum$Strategy== paste("MV","\u03BB",  " = 0.06")| Cum$Strategy== paste("MV","\u03BB",  " = 0.08")| Cum$Strategy== paste("MV","\u03BB",  " = 0.1")] <- "Mean-Variance"

# reorder the display of each plot
Cum$GROUP = factor(Cum$Group, levels=c("TSMDR","Mean-CVaR","Mean-Variance","TSMOM", "Risk-Neutral"))
# haha <- Cum %>% filter(Group=="TSMDR"|Group=="TSMOM")
# haha %>% ggplot(aes(x=date, y=`cumulative absolute returns`, 
#                    group = Strategy, 
#                    colour = Strategy,
#                    #size=mysize, 
#                    #linetype = Strategy
# )) + 
#   #geom_point() + 
#   geom_line()+
#   scale_size(range = c(1, 1.5), guide = "none") +
#   #ggtitle(paste("\u03b1 =", A)) + 
#   ylab("Cumulative absolute returns (log scale)") + 
#   #scale_linetype_manual(values = c(rep(1,3), rep(2,3),rep(3,6), rep(4,5), rep(5,2))) +
#   scale_color_manual(values = c(brewer.pal(8, "Set2"), 
#                                 brewer.pal(8, "Set1"), 
#                                 brewer.pal(3, "Dark2")),
#                      guide=guide_legend(nrow=2)
#   ) +
#   #facet_grid(~Group)+
#   facet_grid(cols = vars(GROUP))+
#   #theme(legend.position="bottom")+
#   theme(legend.position = "bottom", 
#         legend.text = element_text(size=10, face="bold"), 
#         legend.title = element_text(size=10, face="bold"),
#         #axis.title=element_text(size=12,face="bold"),
#         axis.text=element_text(size=11),
#         strip.text.x = element_text(size = 11, face = "bold"),
#         axis.title.x=element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         #panel.grid.major = element_line(size = 0.05, linetype = 'solid',colour = "gray")
#         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray")
#   )+
#   guides(fill=guide_legend(nrow=2))


Cum %>% ggplot(aes(x=date, y=`cumulative absolute returns`, 
                   group = Strategy, 
                   colour = Strategy,
                   #size=mysize, 
                   #linetype = Strategy
                   )) + 
  #geom_point() + 
  geom_line()+
  scale_size(range = c(1, 1.5), guide = "none") +
  #ggtitle(paste("\u03b1 =", A)) + 
  ylab("Cumulative absolute returns (log scale)") + 
  #scale_linetype_manual(values = c(rep(1,3), rep(2,3),rep(3,6), rep(4,5), rep(5,2))) +
  scale_color_manual(values = c(brewer.pal(8, "Set2"), 
                                brewer.pal(8, "Set1"), 
                                brewer.pal(3, "Dark2")),
                     guide=guide_legend(nrow=2)
                     ) +
  #facet_grid(~Group)+
  facet_grid(cols = vars(GROUP))+
  #theme(legend.position="bottom")+
  theme(legend.position = "bottom", 
        legend.text = element_text(size=10, face="bold"), 
        legend.title = element_text(size=10, face="bold"),
        #axis.title=element_text(size=12,face="bold"),
        axis.text=element_text(size=11),
        strip.text.x = element_text(size = 11, face = "bold"),
        axis.title.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        #panel.grid.major = element_line(size = 0.05, linetype = 'solid',colour = "gray")
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray")
        )+
  guides(fill=guide_legend(nrow=2))




##------------- calculate the performance metric of passive long---------------- 
excess_l <- r_m[(T+G+1):length(r_m)]-B_m[(T+G+1):length(B_m)]
abs_l <- r_m[(T+G+1):length(r_m)]
annual_excess_return_l = 12*mean(excess_l)
annual_sd_l = sqrt(12)*sd(abs_l)
annual_sharpe_ratio_l = annual_excess_return_l/annual_sd_l
annual_sortino_ratio_l = sqrt(12)*SortinoRatio(abs_l, MAR = mean(B_m[(T+G+1):length(B_m)]))
annual_conditional_sharpe_l <- sqrt(12)*SharpeRatio(
  R = xts(excess_l, trbm$DATE[(T+G+1):length(r_m)]),
  Rf = 0,
  p = A,
  FUN = "ES"
)
mdd_l= maxDrawdown(xts(abs_l, trbm$DATE[(T+G+1):length(r_m)]),geometric=TRUE)



##----------------- save the performance metric in a table ----------------
pf_m <- rbind(annual_excess_return_m, annual_sd_m, annual_sharpe_ratio_m, annual_sortino_ratio_m, mdd_m, annual_conditional_sharpe_m)
pf_mv <- rbind(annual_excess_return_mv, annual_sd_mv, annual_sharpe_ratio_mv, annual_sortino_ratio_mv, mdd_mv, annual_conditional_sharpe_mv)
pf_opt <- rbind(annual_excess_return_opt, annual_sd_opt, annual_sharpe_ratio_opt, annual_sortino_ratio_opt, mdd_OPT, annual_conditional_sharpe_OPT)
pf_mcvar <- rbind(Mannual_excess_return_m, Mannual_sd_m, Mannual_sharpe_ratio_m, Mannual_sortino_ratio_m, Mmdd_m, Mannual_conditional_sharpe_m)
pf_l <- rbind(annual_excess_return_l, annual_sd_l, annual_sharpe_ratio_l, annual_sortino_ratio_l, mdd_l, annual_conditional_sharpe_l)

pf <- as.data.frame(cbind(pf_mv, pf_opt, pf_m, pf_mcvar))
colnames(pf) <- c("MV lambda = 0.02", "MV lambda = 0.04", "MV lambda = 0.06", "MV lambda = 0.08", "MV lambda = 0.1",
                  "lambda = 0", "MC lambda = 0.02", "MC lambda = 0.04", "MC lambda = 0.06", "MC lambda = 0.08", "MC lambda = 0.1", 
                  paste("C = ", round(stat[2], 3)),paste("C = ", round(stat[3], 3)), paste("C = ", round(stat[5], 3)), "Unscaled TSMOM",
                  paste("C* = ", round(Mstat[[q]][2], 3)), paste("C* = ", round(Mstat[[q]][3], 3)), paste("C* = ", round(Mstat[[q]][5], 3)))
pf <- t(pf)