### Function: Run Models for All Rebalance Dates --------------------------
run_models <- function (
  portfolios,
  expected_returns,
  historic_returns,
  expected_returns_pca,
  dates,
  models) {

  # Markowitz Model needs the D matrix to be positive definite.  It is not positive definite when rows used for the covariance matrix
  # are fewer than the number of portfolios plus 1.  We can only start the Markowitz calculations after we have enough historical observations
  # for the covariance matrix with a gap between these observations and the measurement date equal to the return horizon.
  
  # For example, assume we are using 28 portfolios and future 8 week returns. Future 8 week returns measured at each Week from 1 to 28 are used 
  # for covariance calculation passed to Markowitz model.  The future 8 week return on week 28 captures returns for week 28-35. Thus we cannot 
  # start measuring the markowitz model until week 36 (i.e. > 28 + 8 - 1).
  
  # The 1st measurement dates may not coincide with 1st date we have historic returns. So we reduce (Num Portoflios + Future Weeks -1) 
  # requirement by historical months that exist prior to 1st measurement date.
  if (portfolios$aggregation[1]=="Static" & portfolios$type[1]=="Excess") {
    horizon <- as.numeric(sub("w_excess_ret","",sub("fut_","",portfolios$return[1])))
  } else {
    horizon <- as.numeric(sub("w_ret","",sub("fut_","",portfolios$return[1])))
  }

  markowitz_start = min(dates$date[index(dates$rebalance)>            
                                     ( 
                                       (dim(portfolios)[1] + 1 + horizon - 1) -    # Num Portfolios + Future Weeks Used for Returns - 1
                                        (as.numeric(min(dates$date))-as.numeric(min(historic_returns$period_formatted)))/7)
                                   & dates$rebalance==TRUE])
  
  model_weights <- data.frame()
  for (i in 1:length(models)) {
    if (exists("model")) {rm(model)} # Remove when change to new model so not accidentally used
    for (date_test in dates$date) {
      if (dates$rebalance[dates$date==date_test]) {# 1st Date in List is Always a Rebalance Date

        # Run model to get new portfolio allocations
        if (models[[i]][1]=="baseline") {
          model <- get_baseline(portfolios=portfolios,date_test=date_test)
          model_weights <- rbind(model_weights,model)
        } else if (models[[i]][1]=="markowitz") {
          if(date_test >= markowitz_start) {
            model <- get_Markowitz(lamda=as.numeric(models[[i]][2]), expected_returns = expected_returns, historic_returns = historic_returns, date_test=date_test, horizon=horizon, portfolios=portfolios)
            model_weights <- rbind(model_weights,model)
          }
        } else if (models[[i]][1]=="pca") {
          model <- get_pca(expected_returns = expected_returns_pca, approach = models[[i]][2], date_test=date_test, horizon=horizon, portfolios=portfolios, which.pc=models[[i]][3])
          model_weights <- rbind(model_weights, model)
        }
      } else if (exists("model")) {# Check if exists because Markowitz may ignore initial dates which are needed for historical covariance calcualtion
        model$date = as.Date(date_test) # Use Last Model Weights at Non-Rebalance Date | Update Date from Prior Model Results
        model_weights <- rbind(model_weights,model)
      }
    }
  }
  
  return(model_weights)
}