library(expm)
library(matlib)
library(MTDrh)
#library(goftest)
library(abind)
library(ggplot2)
library("readr")
library(RVAideMemoire)
library(DescTools)
#library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(lubridate)
library(zoo)
library(dplyr)
#library(RTseries)
library(dgof)
library(tidyverse)
library(data.table)

sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()

setwd("~/Desktop/Paper2/Paper2 code&data for geometric return")
monthdata <- read.csv("1926SP.csv") #19260130 - 20191231
monthdata$DATE <- as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d")
monthdata <- monthdata %>% filter( (DATE>as.Date("1979-12-01")) & (DATE<as.Date("2000-12-31"))) #228
# monthdata <- monthdata %>% filter( (DATE>as.Date("2001-01-01")) & (DATE<as.Date("2019-12-31"))) #227 G=8 

T = 12 # number of monthly data we use to calculate each momentum
F = 1 # Forecast horizon in terms of month
#G > 1 number of monthly data used to calculate each volatility
iteration <- 50 # No. of generated scenarios 


##------ Define index level S_i------
S_t <- monthdata$spindx
S_t_no_head <- tail(S_t, -1) 
S_t_no_tail <- head(S_t, -1)
S_t <- S_t_no_tail

##-------define return r_m--------
r_m <- ((S_t_no_head-S_t_no_tail)/S_t_no_tail)


#---------define date-----------
date <- head(monthdata,-1)$caldt


##---------main code----------------

###-------Generate new momentum and returns and stock index levels------------
#To avoid overwritten: Declare your matrix outside the loop, and just fill in one column per loop iteration.
M <- numeric()
sigma <- numeric()
generated_r <- matrix(NA, nrow=length(r_m), ncol=iteration)


a <- c(1,3,4,5,10,12,19,23,25, 34, 42,62, 66,103, 123, 145, 192, 200,267, 452, 567, 576, 578, 765,789,987,999,1892,1903,3001)
CVM <- matrix(NA, ncol = 6, nrow = length(a))
# p_value <- matrix(NA, ncol = 10, nrow = length(a))
s = 0 # Counter for seeds 
j = 0 # counter for G

for (N in a){
  s = s+1
  print(s)
  set.seed(N)
  for (G in c(2,	4,	6,	8,	10, 12)){
    if ((j+1) %% 6 ==0){
      j = 6
    }
    else{
      j = (j+1)%%6
    }
    
    #make the starting point of the scenario the same
    if (G < 12){
      r_t <- tail(r_m,G-12) 
      }

    #get the momentum from month T+1 til the end
    for (i in c((T+1):(length(r_t)))){
      
      # ewma 
      # M[i] = 0
      # for (d in c(1:T)) {M[i] <- (M[i] + (exp(1)-1)*exp(-d)*r_t[i-d])}
      
      # wma
      # M[i] = 0
      # for (d in c(1:T)) {M[i] <- (M[i] + (T-d+1)*r_t[i-d])}
      # M[i] <- M[i]/((1+T)*T/2)
      
      # ew
      M[i] = 0
      for (d in c(1:T)) {M[i] <- mean(r_t[(i-T): (i-1)])}
    }
    
    
  
    
    
    K <- length(seq(from = (T+G+1), to = length(r_t), by = F)) # number of non-overlapped instances
    r_gen <- array(NA,dim = c(F,iteration, K))
    
    #get the daily observation up to month T to calculate the first momentum
    for (i in (1:(T+G))){
      generated_r[i,] <- r_t[i]
    }

    # start to generate scenarios from month T+G+1
    for (i in seq(from = (T+G+1), to = length(r_t), by = F)){
      
      # get the daily observation on month i 
      sigma[i] <- sd(r_t[(i-G):(i-1)] - M[(i-G):(i-1)]) # equal weight to calculate sigma
      # EWMA_r_mean <- ewma.filter((r_t[(T+1):(i-1)]-M[(T+1):(i-1)]), 1/(1+G))
      # EWMA_vol <- sqrt(ewma.filter(((r_t[(T+1):(i-1)]-M[(T+1):(i-1)])-EWMA_r_mean)^2, 1/(1+G))) # use ewma std
      # sigma[i] <- EWMA_vol[i-T-1] # ewma to calculate sigma
      
      generated_r[i,] <- rnorm(n = iteration, mean=M[i], sd=sigma[i])
    
      
      k = ((i+F)-(T+G+1))/F
      r_gen[,,k] <- generated_r[(i):(i+F-1),] # non-overlapped instances of scenarios 
    } # end of the for loop of i 



    # Non-overlapped instances of realizations
    r_o <- array(NA,dim = c(F, K))
    for (i in seq(1,(length(r_t)-T-G), by = F)){
      k = (i-1+F)/F
      r_o[,k] <- r_t[(T+G+i):(T+G+i+F-1)]
    }
    
    rank41 <- Rank0(r_gen, r_o)
    #rank41 <- round(A(r_gen, r_o, prob = NULL, debias = F, transformation = F))
    CVM[s,j]<- round(dgof::cvm.test(rank41,ecdf(1:(iteration+1)), type='W2') $statistic, digits =4)
    #p_value[s,j] <- round(dgof::cvm.test(rank41,ecdf(1:(iteration+1)), type='W2') $p.value, digits =4)
  }
}


m = colMeans(CVM)
sd = apply(CVM, 2, sd)
# t-statistics confidence interval 
#C <- apply(CVM, 2, function(X)t.test(X, y = NULL, alternative = "two.sided", conf.level = .95)$conf.int)

GCVM <- rbind(m, sd)
print(GCVM)

##-------calculate the parameter, scale, of TSMOM-------
Trea1926 <- read.csv("1926T.csv")
Trea1926$DATE <- as.Date(as.character.Date(Trea1926$caldt),format = "%Y %m %d")
C1926 <- inner_join(Trea1926, monthdata, by = "DATE")
df_month1982 <- subset(C1926, select = c("DATE", "t30ret", "spindx"))
B_t <- head(df_month1982$t30ret,-1)

G = 10

# G+2-F is corresponding to (T+G+1)-(T+F-1). Since OPT starts from T+G+1, TSMOM should start from G+2-F to be consisten
annual_vol <- sqrt(12)*sd(r_m[(G+2-F):length(r_m)]-B_t[(G+2-F):length(B_t)])  # use sample std
ANNUAL_r_mean <- ewma.filter((r_m[(G+2-F):length(r_m)]-B_t[(G+2-F):length(B_t)]), (1-3/4))
ANNUAL_vol <- sqrt(12*ewma.filter(((r_m[(G+2-F):length(r_m)]-B_t[(G+2-F):length(B_t)])-ANNUAL_r_mean)^2, (1-3/4))) # use ewma std
stat <- summary(ANNUAL_vol[2:length(ANNUAL_vol)]) # stat[3] is median, and stat[4] is mean

##-------calculate the parameter, scale, of MCVAR-------
source("~/Desktop/Paper2/Paper2 code&data for geometric return/3scenario.R")
source("~/Desktop/Paper2/Paper2 code&data for geometric return/4Optimization.R")
scenario <- Scenario(monthdata = df_month1982, G = G, iteration = 20000, seed = 999, mov_average = "ew")
i = 0
Mstat <- list()
# for (A in c(0.50, 0.55, 0.60)){
#   i = i+1
#   Mstat[[i]] = summary(CVaR((-scenario+B_t[(T+G+1):length(B_t)]), alpha = A))
# }
for (A in c(0.75, 0.90, 0.99)){
  i = i+1
  Mstat[[i]] = summary(CVaR((-scenario+B_t[(T+G+1):length(B_t)]), alpha = A))
}

