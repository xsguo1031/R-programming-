library(RiskPortfolios)
library(zoo) # calculate rolling horizon 

setwd("~/Desktop")
source("~/Desktop/Paper2/Paper2 code&data for geometric return/3scenario.R")

##------------------Mean-Variance matrix----------------
# Mean Variance model of excess returns across scenarios at each time t
MV3 <- function(scenario, 
                B_t, 
                lambda # new_lambda = 2*lambda/(1-lambda)
                ){
  

  # load data from 2001-01-01
  # monthdata <- read.csv("1926SP.csv")
  # monthdata$DATE <- as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d")
  # monthdata <- monthdata %>% filter( (DATE>as.Date("2001-01-01")) & (DATE<as.Date("2019-12-31")))
  # trbm <- read.csv("1926T.csv")
  # trbm$DATE <- as.Date(as.character.Date(trbm$caldt),format = "%Y %m %d")
  # trbm <- trbm %>% filter( (DATE>as.Date("2001-01-01")) & (DATE<as.Date("2019-12-31")))
  # C1926 <- inner_join(trbm, monthdata, by = "DATE")
  # df_month <- subset(C1926, select = c("DATE", "t30ret", "spindx"))
  # 
  # B_t <- head(df_month$t30ret,-1)
  # 
  # S_m <- df_month$spindx
  # S_m_no_head <- tail(S_m, -1)
  # S_m_no_tail <- head(S_m, -1)
  # S_m <- S_m_no_tail
  # r_t <- ((S_m_no_head-S_m_no_tail)/S_m_no_tail)
  # 
  # 
  # ##--------generate scenarios------------
  # scenario <- Scenario(monthdata = df_month, 
  #                      G = G, 
  #                      iteration = 20000, 
  #                      seed = 999, 
  #                      mov_average = "ew")
  
  
  ##------------------- Estimate mean: mean(f_t-r_{t,j})------------------------
  a <- colMeans(B_t-scenario)
  
  ##------------------- Estimate variance: var(r_{t,j}-f_t)-----------------------
  b <- apply(scenario, 2, var) # delete the first B_t and the last r_t
  
  ##------------------- Calculate the derivative -----------------------
  w <- (lambda-1)*a/(2*lambda*b)
  
  ##------------------- Solve the mean-variance model -----------------------
  W <- numeric()
  excess_return <- numeric()
  abs_return <- numeric()
  for (i in c(1:length(w))){
    if (w[i] < -1){
      W[i] = -1
    } else if (w[i] > 1){
      W[i] = 1
    } else {
      W[i] = w[i]
    }
   
  }
  
  return(W)
}







##------------------Mean-Variance matrix----------------
# Mean Variance model of excess returns from time t-G to time t-1
MV2 <- function(T = 12,
                G, # when using ewma or equal weight to calculate vol
                lambda, # new_lambda = 2*lambda/(1-lambda)
                mov_average = "wma"){
  # load data and pre-process data from 2001
  monthdata <- read.csv("Paper2/monthly return SP500.csv") #20010131 - 20191231
  monthdata$DATE <- floor_date(as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d"), "month")
  trbm <- read.csv("Paper2/GS1M.csv") # 1-month treasury bill rate from 20010701-20191201
  trbm <- trbm[!is.na(suppressWarnings(as.numeric(as.character(trbm$GS1M)))),]
  trbm$DATE <- floor_date(as.Date(trbm$DATE), "month")
  trbm$bill <- as.numeric(as.character(trbm$GS1M))
  #trbm <- trbm %>% filter(DATE<as.Date("2012-12-31")) # select the specific period to test
  #trbm <- na.mit(trbm)
  temp_month <- inner_join(trbm, monthdata, by = "DATE")
  df_month <- subset(temp_month, select = c("DATE", "bill", "spindx"))
  
  ##-------define treasury bill rate--------
  Bill_m <- df_month$bill/100/12
  B_t <- head(Bill_m,-1) # delete the last element of Bill_m
  
  ##------ Define index level S_t------
  S_t <- df_month$spindx
  S_t_no_head <- tail(S_t, -1)
  S_t_no_tail <- head(S_t, -1)
  S_t <- S_t_no_tail
  
  ##-------define return r_m--------
  r_t <- ((S_t_no_head-S_t_no_tail)/S_t_no_tail)
  
  
  
  ##------------------- Estimate mean------------------------
  B <- numeric()
  M <- numeric()
  if (mov_average == "ewma"){
    for (i in c((T+1):length(S_t))){
      B[i] <- mean(B_t[(i-T+1): i])
      M[i] = 0
      for (d in c(1:T)) {M[i] <- (M[i] + (exp(1)-1)*exp(-d)*r_t[i-d])}
    }
  } else if (mov_average == "ew") {
    for (i in c((T+1):(length(S_t)))){
      B[i] <- mean(B_t[(i-T+1): i])
      M[i] <- mean(r_t[(i-T): (i-1)])    
    }
  } else if (mov_average == "wma") {
    c = ((1+T)*T/2)
    for (i in c((T+1):(length(S_t)))){
      B[i] <- mean(B_t[(i-T+1): i])
      M[i] = 0
      for (d in c(1:T)) {M[i] <- (M[i] + (T-d+1)*r_t[i-d])}
    }
    M <- M/c
  }
  # mu is calculated from T+1
  a <- (M-B_t)
  #a <- (M-B)
  
  ##------------------- Estimate var(r_{t-1}-f_t)-----------------------
  rB <- cbind(head(r_t,-1),tail(B_t,-1)) # delete the first B_t and the last r_t
  e <- rB[,1] - rB[,2]
  # e <- rB[,1]
  b <- numeric()
  # Sigma is calculated from G+1-1
  for (i in (G+1):(length(S_t))){
    b[i]<- var(e[(i-G):(i-1)])
  }
  
  ##------------------- Calculate the derivative -----------------------
  w <- (1-lambda)*a[(T+G+1):length(S_t)]/(2*lambda*b[(T+G+1):length(S_t)])
  
  ##------------------- Solve the mean-variance model -----------------------
  W <- numeric()
  excess_return <- numeric()
  abs_return <- numeric()
  for (i in c(1:length(w))){
    if (w[i] < -1){
      W[i] = -1
    } else if (w[i] > 1){
      W[i] = 1
    } else {
      W[i] = w[i]
    }
    excess_return[i] <- (W[i]*(r_t[i]-B_t[i]))
    abs_return[i] <- excess_return[i] + B_t[i]
  }
  
  
  result <- list("avg_excess" = excess_return, "avg_abs" = abs_return, "position" = W)
  return(result)
}








##------------------Mean-Covariance matrix----------------
# two assests are considered seperately, so we have a covariance matrix in this model
MV <- function(T = 12,
               F = 1,
               G, # when using ewma or equal weight to calculate vol
               new_lambda, # new_lambda = 2*lambda/(1-lambda)
               mov_average = "wma"){
  # load data and pre-process data from 2001
  monthdata <- read.csv("monthly return SP500.csv") #20010131 - 20191231
  monthdata$DATE <- floor_date(as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d"), "month")
  trbm <- read.csv("GS1M.csv") # 1-month treasury bill rate from 20010701-20191201
  trbm <- trbm[!is.na(suppressWarnings(as.numeric(as.character(trbm$GS1M)))),]
  trbm$DATE <- floor_date(as.Date(trbm$DATE), "month")
  trbm$bill <- as.numeric(as.character(trbm$GS1M))
  #trbm <- trbm %>% filter(DATE<as.Date("2012-12-31")) # select the specific period to test
  #trbm <- na.mit(trbm)
  temp_month <- inner_join(trbm, monthdata, by = "DATE")
  df_month <- subset(temp_month, select = c("DATE", "bill", "spindx"))
  
  ##-------define treasury bill rate--------
  Bill_m <- df_month$bill/100/12
  B_t <- head(Bill_m,-1) # delete the last element of Bill_m
  
  ##------ Define index level S_t------
  S_t <- df_month$spindx
  S_t_no_head <- tail(S_t, -1)
  S_t_no_tail <- head(S_t, -1)
  S_t <- S_t_no_tail
  
  ##-------define return r_m--------
  r_t <- ((S_t_no_head-S_t_no_tail)/S_t_no_tail)
  
  
  
  ##------------------- Estimate mean------------------------
  B <- numeric()
  M <- numeric()
  if (mov_average == "ewma"){
    for (i in c((T+1):length(S_t))){
      M[i] = 0
      for (d in c(1:T)) {M[i] <- (M[i] + (exp(1)-1)*exp(-d)*r_t[i-d])}
      B[i] <- mean(B_t[(i-T+1): i])
    }
  } else if (mov_average == "ew") {
    for (i in c((T+1):(length(S_t)))){
      M[i] <- mean(r_t[(i-T): (i-1)])
      B[i] <- mean(B_t[(i-T+1): i])
    }
  } else if (mov_average == "wma") {
    c = ((1+T)*T/2)
    for (i in c((T+1):(length(S_t)))){
      M[i] = 0
      for (d in c(1:T)) {M[i] <- (M[i] + (T-d+1)*r_t[i-d])}
      B[i] <- mean(B_t[(i-T+1): i])
    }
    M <- M/c
  }
  # mu is calculated from T+1
  mu <- cbind(M, B)
  
  
  ##------------------- Estimate covariance cov(B_t, r_{t-1})-----------------------
  rB <- cbind(head(r_t,-1),tail(B_t,-1)) # delete the first B_t and the last r_t
  Sigma <- list()
  # Sigma is calculated from G+1-1
  for (i in (G+1):(length(S_t))){
    Sigma[[i]] <- cov(rB[(i-G):(i-1),1:2])
  }
  
  
  ##------------------- Solve the mean-variance model -----------------------
  w <- matrix(NA, nrow = length(S_t), ncol = 2)
  excess_return <- numeric()
  abs_return <- numeric()
  for (i in (T+G+1):length(S_t)){
    w[i,] <- optimalPortfolio(mu = mu[i,1:2], Sigma = Sigma[[i]],
                              control = list(type = 'mv', 
                                             constraint = 'user', 
                                             LB = c(-1, 0), UB = c(1, 2), 
                                             gamma = new_lambda))
    excess_return[i] <- (w[i,1]*(r_t[i]-B_t[i]))
    abs_return[i] <- excess_return[i] + B_t[i]
  }
  
  result <- list("avg_excess" = excess_return, "avg_abs" = abs_return, "position" = w[,1])
  return(result)
}









