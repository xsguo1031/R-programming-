### Function: Get Measurement & Rebalance Dates  --------------------------
get_measurement_dates <- function(
  period_start = NA,
  period_end = NA,
  rebalance_weeks = 24,
  table = factor_weekly) {
  
  period_start = ifelse(is.na(period_start),min(table$period_formatted),as.Date(period_start))
  period_end = ifelse(is.na(period_end),max(table$period_formatted),as.Date(period_end))
  
  dates <- unique(table$period_formatted)
  dates <- sort(dates)
  dates <- dates[dates>=period_start & dates<=period_end]
  
# Code added now that horizons measured in weeks
  results <- data.frame(date=dates,rebalance=rep(FALSE,times=length(dates)))
  rebalance_num = floor((length(dates)-1) / rebalance_weeks) # Subtract 1 b/c rebalance measured from 1st initial date
  rebalance_indices = c(1,seq(1:rebalance_num)*rebalance_weeks+1) #Add 1 b/c rebalance measured from 1st initial date
  results$rebalance[rebalance_indices] = TRUE
  
#  Code below previously used when horizons measured in months
#  rebalance_dates = dates[1]
#  max_date = dates[length(dates)]
#  next_date = (rebalance_dates[length(rebalance_dates)] - 6) %m+% months(rebalance_months) # subtract 6 days to get 1st day of weekly period
#  
#  while (next_date < max_date) {
#    
#    rebalance_dates = c(rebalance_dates,
#                        dates[Position(function(x) x >(next_date), dates,right=FALSE)]
#    )
#    
#    next_date = (rebalance_dates[length(rebalance_dates)] - 6) %m+% months(rebalance_months) # subtract 6 days to get 1st day of weekly period
#  }
#  
#  results <- data.frame(date=dates)
#  results$rebalance <- ifelse(is.na(match(results$date,rebalance_dates)),FALSE,TRUE)
  
  return(results)
}
