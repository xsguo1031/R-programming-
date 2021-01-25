library(tidyverse)
require(reshape2) # for melt the columns into a new reformed dataframe

CVaR <- function(X, alpha){ 
  eta <- numeric()
  for (i in 1:ncol(X)){
    eta[i] <- quantile(X[,i], alpha)
  } 
  loss <- sweep(X,2,eta) # dim(loss) = # of scenarios * # of instances
  loss_positive <- replace(loss, loss < 0, 0)
  avg_loss_positive <- apply(loss_positive, 2, mean)
  cvar <- (eta + avg_loss_positive/(1-alpha))
  return(cvar)
}

# # to verify CVaR when w = 0.5
# cvar_0.5 <- cvar(eta0.5,-X0.5, 0.75)
# CVaR_0.5 <- CVaR(-0.5*scenario, 0.75)
# cbind(cvar_0.5, CVaR_0.5)
# # to verify CVaR when w = -0.5
# cvar_n0.5 <- cvar(etan0.5,-Xn0.5, 0.75)
# CVaR_n0.5 <- CVaR(0.5*scenario, 0.75)
# cbind(cvar_n0.5, CVaR_n0.5)

# OPT is the model with the objective function of (1-lambda)*E(-X)+lambda*CVAR(-X)
OPT <- function(r, f, lambda, alpha){
  E <- numeric()
  E <- apply(r, 2, mean)
  w <- numeric()
  coef1 = (lambda-1)*E+f+lambda*CVaR(-r,alpha)
  coef2 = (lambda-1)*E+f-lambda*CVaR(r,alpha)
  for (i in c(1:length(coef1))){
    if (coef2[i] > 0 & coef1[i] > -coef2[i]){
      w[i] = -1
    } else if (coef1[i] >= 0 & coef2[i] <= 0){
      w[i] = 0
    } else {
      w[i] = 1
      }
  }
  
  return(w)
}

#---------------------- to verify function OPT ------------------
# T = 12
# F = 1
# G = 4
# 
# # load data and pre-process data
# monthdata <- read.csv("monthly return SP500.csv") #20010131 - 20191231  
# monthdata$DATE <- floor_date(as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d"), "month") # change the date to the beginning of each month
# 
# trbm <- read.csv("GS1M.csv") # 1-month treasury bill rate from 20010701-20191201
# trbm <- trbm[!is.na(suppressWarnings(as.numeric(as.character(trbm$GS1M)))),]
# trbm$DATE <- floor_date(as.Date(trbm$DATE), "month")
# trbm$bill <- as.numeric(as.character(trbm$GS1M))
# #trbm <- trbm %>% filter(DATE<as.Date("2012-12-31")) # select the specific period to test
# #trbm <- na.omit(trbm)
# temp_month <- inner_join(trbm, monthdata, by = "DATE")
# df_month <- subset(temp_month, select = c("DATE", "bill", "spindx"))
# 
# ##-------define treasury bill rate--------
# Bill_m <- df_month$bill/100/12
# B_m <- head(Bill_m,-1)
# 
# scenario <- Scenario(G = 4, iteration = 50, seed = 999)
# # scenario1 <- read.csv("~/Desktop/model50/R_gen50.csv")
# # all.equal(scenario, scenario1)
# 
# lambda0 <- read.csv("/Users/xiaoshiguo/Desktop/model50/OBJ50 lambda=0.csv")
# OBJ0 <- lambda0$OBJ
# W0 <- lambda0$WEIGHT
# w0 <- OPT(r=scenario, f=B_m[(T+G+2):length(B_m)], lambda = 0, alpha = 0.75)  
# sum(w0 == round(W0, digits = 2)) 
# 
# lambda0.3 <- read.csv("/Users/xiaoshiguo/Desktop/model50/OBJ50 lambda=0.3.csv")
# OBJ0.3 <- lambda0.3$OBJ
# W0.3 <- round(lambda0.3$WEIGHT,digits = 2)
# w0.3 <- OPT(r= scenario, f=B_m[(T+G+2):length(B_m)], lambda = 3/13, alpha = 0.75)  
# sum(w0.3 == round(W0.3, digits = 2)) 
# 
# lambda0.5 <- read.csv("/Users/xiaoshiguo/Desktop/model50/OBJ50 lambda=0.5.csv")
# OBJ0.5 <- lambda0.5$OBJ
# W0.5 <- lambda0.5$WEIGHT
# w0.5 <- OPT(r= scenario, f=B_m[(T+G+2):length(B_m)], lambda = 1/3, alpha = 0.75)  
# sum(w0.5 == round(W0.5, digits = 2)) 
# 
# w1 <- OPT(r= scenario, f=B_m[(T+G+2):length(B_m)], lambda = 0.5, alpha = 0.75) 
# lambda1 <- read.csv("/Users/xiaoshiguo/Desktop/model50/OBJ50 lambda=1.csv")
# OBJ1 <- lambda1$OBJ
# W1 <- round(lambda1$WEIGHT,digits = 2)
# sum(w1 == round(W1, digits = 2)) 