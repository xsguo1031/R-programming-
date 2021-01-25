library(dplyr)
library(PerformanceAnalytics)
library(xts)
#library(MFTSR) # for ewma vol
library(zoo)
library(data.table) # for combine the dataframe to get the solution

# Function of exponential weighted moving average
# From https://stackoverflow.com/questions/42774001/fast-r-implementation-of-an-exponentially-weighted-moving-average
ewma.filter <- function (x, ratio) {
  c(stats::filter(x * ratio, 1 - ratio, "recursive", init = x[1]))
  }

# Time Series Momentum Strategy 
TSMOM <- function(freq, ret, T, F, b, scale){
  sig <- numeric()
  avgexcess <- numeric()
  avgabs <- numeric()
  
  if (freq == "monthly") {
    
    r_abs_month <- ret
    
    # get excess return 
    r_excess_month <- (ret-b)
    
    # calculate the ex-ante volatility 
    delta <- 3/4 # unit is month -> com = 3 trading months
    ewmamean <- ewma.filter(r_excess_month, (1-delta))
    exvol <- sqrt(12*ewma.filter((r_excess_month-ewmamean)^2, (1-delta)))
    
    
    # way 1 uses past T month excess return to calculate momentum signal in month t using simple return
    # for (t in c((T+1): length(r_excess_month))){
    #   sig[t] = sign(mean(r_excess_month[(t-T):(t-1)]))
    # }
    # way 1 uses past T month excess return to calculate momentum signal in month t using log return
    for (t in c((T+1): length(r_excess_month))){
      sig[t] = sign(cumprod(ret[(t-T):(t-1)]+1)[T]-cumprod(b[(t-T):(t-1)]+1)[T])
    }
    
    
    #abs_position = head(abs(scale/exvol),-1) # no limit on position
    abs_position = head(pmin(abs(scale/exvol),1),-1) # compare the absolute position with 1 and delete the last element as the last exvol always has data leakeage
    position = sig[2:length(sig)]*abs_position # sig[t+1]*abs_p[t] position at time d is the scaled 
   # calculate the average log absolute return of the momentum strategy 
    for (t in c((T+F): length(r_excess_month))){
      avgexcess[t] <- mean(position[(t-F): (t-1)]) *r_excess_month[t]
      avgabs[t] <- avgexcess[t]+b[t]
    }
    
  }
  
  result <- list("avg_excess" = avgexcess, "avg_abs" = avgabs, "position" = position)
  return(result)
}




# Time Series Momentum Strategy 
TSMOM_unscale <- function(freq, ret, T, F, b, mom = "ew"){
  sig <- numeric()
  avgexcess <- numeric()
  avgabs <- numeric()
  
  if (freq == "monthly") {
    
    r_abs_month <- ret
    
    # get excess return 
    r_excess_month <- (ret-b)
    
    # way 1 uses equal weight to calculate momentum signal in month t
    if (mom == "ew") {
      for (t in c((T+1): length(r_excess_month))){
        sig[t] = sign(mean(r_excess_month[(t-T):(t-1)]))
      }
    }
    
    else if (mom == "ewma"){
      # way 2 uses ewma to calculate momentum signal in month t
      M <- numeric()
      for (t in c((T+1):length(r_excess_month))){
        M[t] = 0
        for (d in c(1:T)) {M[t] <- (M[t] + (exp(1)-1)*exp(-d)*r_excess_month[t-d])}
      }
      sig <- sign(M)
    } 
    
    else if (mom == "wma"){
      M <- numeric()
      c = ((1+T)*T/2)
      for (t in c((T+1):length(r_excess_month))){
        M[t] = 0
        for (d in c(1:T)) {M[t] <- (M[t] + (T-d+1)*r_excess_month[t-d])}
      }
      M <- M/c
      sig <- sign(M)
    } 
    
    else if (mom == "TSMOM"){
      for (t in c((T+1): length(r_excess_month))){
        sig[t] = sign(cumprod(ret[(t-T):(t-1)]+1)[T]-cumprod(b[(t-T):(t-1)]+1)[T])      
        }
    }
    
    #abs_position = head(abs(scale/exvol),-1) # no limit on position
    position = sig[2:length(sig)] # without scale of position
    # calculate the average log absolute return of the momentum strategy 
    for (t in c((T+F): length(r_excess_month))){
      avgexcess[t] <- mean(position[(t-F): (t-1)]) *r_excess_month[t]
      avgabs[t] <- avgexcess[t]+b[t]
    }
    
  }
  
  result <- list("avg_excess" = avgexcess, "avg_abs" = avgabs, "position" = position)
  return(result)
}

