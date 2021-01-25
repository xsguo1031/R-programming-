library(dplyr)
library(PerformanceAnalytics)
library(xts)
#library(MFTSR) # for ewma vol
library(zoo)
library(data.table) # for combine the dataframe to get the solution
source("~/Desktop/Paper2/Paper2 code&data for geometric return/4Optimization.R")

# Function of exponential weighted moving average
# From https://stackoverflow.com/questions/42774001/fast-r-implementation-of-an-exponentially-weighted-moving-average
ewma.filter <- function (x, ratio) {
  c(stats::filter(x * ratio, 1 - ratio, "recursive", init = x[1]))
}

# Time Series Momentum Strategy 
MCVaR <- function(scenario, alpha, ret, b, scale, obj = "cvar"){
  sig <- numeric()
  avgexcess <- numeric()
  avgabs <- numeric()
  
  # get excess return 
  r_excess_month <- (scenario-b)
  
  # calculate var
  if (obj == "var"){
    exvol <- numeric()
    for (t in c(1: ncol(r_excess_month))){
      exvol[t] <- quantile(-r_excess_month[,t], alpha)
    }
  } else {
    # calculate the cvar 
    exvol <- CVaR(-r_excess_month, alpha)
  }
  
  # calculate momentum signal using equal weight
  for (t in c(1: ncol(r_excess_month))){
    sig[t] = sign(mean(r_excess_month[,t]))
  }
  
  abs_position = pmin(abs(scale/exvol),1) # compare the absolute position with 1 
  position = sig*abs_position # sig[t]*abs_p[t] position at time t is the scaled 
  # position = sig # without scale of position
  
  # calculate the average log absolute return of the momentum strategy 
  for (t in c(1: ncol(r_excess_month))){
    avgexcess[t] <- position[t] *(ret[t]-b[t])
    avgabs[t] <- avgexcess[t]+b[t]
  }
  
  result <- list("avg_excess" = avgexcess, "avg_abs" = avgabs, "position" = position)
  return(result)
}



