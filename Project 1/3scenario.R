library(expm)
library(matlib)
library(MTDrh)
#library(goftest)
library(abind)
library("readr")
library(RVAideMemoire)
library(DescTools)
# library(forecast)
# library(plotly)
# library(ggfortify)
# library(gridExtra)
# library(docstring)
# library(readr)
library(here)
library(lubridate)
library(zoo)
library(dplyr)
library(dgof)
library(tidyverse)
library(data.table)

setwd("~/Desktop")
# scenario is built when T = 12, F = 1
Scenario <- function(monthdata, G, iteration, seed, mov_average = "ew"){
  # monthdata <- read.csv("monthly return SP500.csv") #20010131 - 20191231  
  T = 12 # number of monthly data we use to calculate each momentum
  F = 1 # Forecast horizon in terms of month
  # G>1 number of monthly data used to calculate each volatility
  
  ##------ Define index level S_i------
  S_t <- monthdata$spindx
  S_t_no_head <- tail(S_t, -1) 
  S_t_no_tail <- head(S_t, -1)
  S_t <- S_t_no_tail
  
  ##-------define return r_i--------
  r_t <- ((S_t_no_head-S_t_no_tail)/S_t_no_tail) # geometric return
  # r_t <- log(S_t_no_head/S_t_no_tail) # log return
  
  #---------define date-----------
  date <- head(monthdata,-1)$caldt
  
  
  ##---------main code----------------
  
  ###-------Generate new momentum and returns and stock index levels-----------
  set.seed(seed)
  generated_r <- matrix(NA, nrow=length(r_t), ncol=iteration)
  M <- numeric()
  sigma <- numeric()
  K <- length(seq(from = (T+G+1), to = (length(S_t)), by = F)) # number of non-overlapped instances
  r_gen <- array(NA,dim = c(F,iteration, K))
  
  #get the daily observation up to day T to calculate the first momentum
  for (i in (1:(T+G))){
    generated_r[i,] <- r_t[i]
  }
  
  #get the momentum from day T+1 til the end
  if (mov_average == "ewma"){
    for (i in c((T+1):length(S_t))){
      M[i] = 0
      for (d in c(1:T)) {M[i] <- (M[i] + (exp(1)-1)*exp(-d)*r_t[i-d])}
    }
  } else if (mov_average == "ew") {
    for (i in c((T+1):(length(S_t)))){
      M[i] <- mean(r_t[(i-T): (i-1)])
    }
  }
  else if (mov_average == "wma") {
    c = ((1+T)*T/2)
    for (i in c((T+1):(length(S_t)))){
      M[i] = 0
      for (d in c(1:T)) {M[i] <- (M[i] + (T-d+1)*r_t[i-d])}
    }
    M <- M/c
  }

  
  # start to generate scenarios from day T+G+1
  for (i in seq(from = (T+G+1), to = length(S_t), by = F)){
    # get the daily observation on day i 
    sigma[i] <- sd(r_t[(i-G):(i-1)] - M[(i-G):(i-1)]) # Equal weight to calculate sigma
    # EWMA_r_mean <- ewma.filter((r_t[(T+1):(i-1)]-M[(T+1):(i-1)]), 1/(1+G))
    # EWMA_vol <- sqrt(ewma.filter(((r_t[(T+1):(i-1)]-M[(T+1):(i-1)])-EWMA_r_mean)^2, 1/(1+G))) # use ewma std
    # sigma[i] <- EWMA_vol[i-T-1] # ewma to calculate sigma
    
    generated_r[i,] <- rnorm(n = iteration, mean=M[i], sd=sigma[i])
    
    k = ((i+F)-(T+G+1))/F
    r_gen[,,k] <- generated_r[(i):(i+F-1),] # non-overlapped instances of scenarios 
  } # end of the for loop of i 
  
  R_gen <- matrix(NA, nrow = iteration, ncol = K)
  R_gen <- r_gen[1,,]
  
  return(R_gen)
}
