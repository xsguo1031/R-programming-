### Load R Libraries ------------------------------------------------------------------------------------------------------------------
library(plyr)
library(tidyverse) # Access to ggplot2, dplyr, tidyr
library(zoo) # Used for rollapply
library(quadprog) # Used for solve.QP for markowitz model
library(ggplot2) # Used to plot results

### Load Supporting Files ------------------------------------------------------------------------------------------------------------------
#source("~/Downloads/1_Load_Principal_Data.R") # Run if data tables not loaded (see script for specific required names)
source("~/Downloads/2_Define_Portfolios.R")
source("~/Downloads/3_Measurement_Dates.R")
source("~/Downloads/4_Portfolio_Functions.R")
source("~/Downloads/5_Run_Models.R")
source("~/Downloads/6_Models.R")
source("~/Downloads/7_Model_Returns.R")

### Merge Factor Table & Actural Returns Table -------------------------------------------------------------------------------------------------
combined <- inner_join(factor_weekly,returns_weekly, by=c("identifier","period_formatted"))
combined_excess <- inner_join(factor_weekly,returns_excess_weekly, by=c("identifier","period_formatted"))

### Assumptions ------------------------------------------------------------------------------------------------------------------
return_type = "Absolute"                  # Absolute or Excess
return_horizon = "fut_12w_ret"   # Any column from Principal returns_weekly table (or the returns_excess_weekly table if Static & Excess)
return_aggregation = "Dynamic"           # Dynamic or Static
rebalance_period = 12                   # Number of weeks between rebalancing

### Get Factor Portfolios & Measurement Dates --------------------------------------------------------------------------------------------------
portfolios <- define_portfolios(input_type = return_type, input_return = return_horizon, input_aggregation = return_aggregation)
measurement_dates <- get_measurement_dates(rebalance_weeks = rebalance_period) # Default uses all available dates | Option exists to specify start and/or end dates

### Get Returns for Markowitz ----------------------------------------------------------------------------------------------------
# Currently Using Actual Historical Returns
portfolios_returns_inputs <- apply(portfolios, 1, function(x) get_portfolio(ID=x["ID"],aggregation=x["aggregation"],factor=x["factor"], decile=x["decile"],return=x["return"],return_type=x["type"],metric=x["metric"]))
portfolios_returns_inputs <- Reduce(function(x,y) merge(x=x,y=y,by="period_formatted"), portfolios_returns_inputs)
portfolios_returns_inputs <- portfolios_returns_inputs[-which(is.na(portfolios_returns_inputs$portfolio01)),] # Remove NA Rows - i.e dates from which there's not enough weekly returns to calculate return horizon
anyNA(portfolios_returns_inputs) # Ensure all NA's Removed
measurement_dates <- measurement_dates[measurement_dates$date<=max(portfolios_returns_inputs$period_formatted),] # Remove dates with no expected returns

### Get Returns for PCA -----------------------------------------------------------------------------------------------------------
# Currently Using Actual Historical Returns
# Always Uses Future 1 Week Returns (Static Calculation)
# portfolios_returns_inputs_PCA <- apply(portfolios, 1, function(x) get_portfolio(ID=x["ID"],aggregation=c(rep("Static",n=dim(portfolios)[1])),factor=x["factor"],decile=x["decile"],return=c(rep(ifelse(return_type=="Excess","fut_1w_excess_ret","fut_1w_ret"),n=dim(portfolios)[1])),return_type=x["type"],metric=x["metric"])) 
# portfolios_returns_inputs_PCA <- Reduce(function(x,y) merge(x=x,y=y,by="period_formatted"), portfolios_returns_inputs_PCA)
# portfolios_returns_inputs_PCA <- portfolios_returns_inputs_PCA[-which(is.na(portfolios_returns_inputs_PCA$portfolio01)),] # Remove NA Rows - i.e dates from which there's not enough weekly returns to calculate return horizon
# anyNA(portfolios_returns_inputs_PCA) # Ensure all NA's Removed
# measurement_dates <- measurement_dates[measurement_dates$date<=max(portfolios_returns_inputs_PCA$period_formatted),] # Remove dates with no expected returns

### Get Weekly Asbolute Returns for Performance Measurements ----------------------------------------------------------------------
# Always Uses Future 1 Week Returns, Absolute, Static Returns
portfolios_returns_act <- apply(portfolios, 1, function(x) get_portfolio(ID=x["ID"],aggregation=c(rep("Static",n=dim(portfolios)[1])),factor=x["factor"],decile=x["decile"],return=c(rep("fut_1w_ret",n=dim(portfolios)[1])),return_type=c(rep("Absolute",n=dim(portfolios)[1])),metric=x["metric"])) 
portfolios_returns_act <- Reduce(function(x,y) merge(x=x,y=y,by="period_formatted"), portfolios_returns_act)
portfolios_returns_act <- portfolios_returns_act[-which(is.na(portfolios_returns_act$portfolio01)),] # Remove NA Rows - i.e dates from which there's not enough weekly returns to calculate return horizon
anyNA(portfolios_returns_act) # Ensure all NA's Removed
measurement_dates <- measurement_dates[measurement_dates$date<=max(portfolios_returns_act$period_formatted),] # Remove dates with no expected returns


### Define Models to Run ------------------------------------------------------------------------------------------------------------------
# models <- list(c("baseline"), # Naive Baseline using equal weights
#                c("markowitz",0.3), # Run Markowitz with Lamda = X, X is 2nd parameter
#                c("markowitz",0.9))

# models <- list(c("baseline"),
#                c("pca","noneg","best"),
#                c("pca","noneg","PC1"),
#                c("pca","abs","best"),
#                c("pca","abs","PC1"))
# 
# models <- list(c("baseline"),
#                c("pca","noneg","best"),
#                c("pca","noneg","PC1"),
#                c("pca","act","best"),
#                c("pca","act","PC1"))
# 
# models <- list(c("pca","noneg","all"))
# 
#models <- list(c("baseline"))
models <- list(c("markowitz",0.9))
models <- list(c("baseline"),
               c("markowitz",0.1),
               c("markowitz",0.3),
               c("markowitz",0.6),
               c("markowitz",0.9),
               c("markowitz",10),
               c("markowitz",20))

### Run Models to Get Weights ---------------------------------------------------------------------------------------------------------------
#models_weights <- run_models(portfolios=portfolios, expected_returns=portfolios_returns_inputs, historic_returns=portfolios_returns_inputs, 
#                             expected_returns_pca = portfolios_returns_inputs_PCA, dates=measurement_dates, models=models)

models_weights <- run_models(portfolios=portfolios, expected_returns=portfolios_returns_inputs, historic_returns=portfolios_returns_inputs, 
                              dates=measurement_dates, models=models)

### Get Actual Returns for Recommended Weights by models ------------------------------------------------------------------------------------
model_results_cumulative <- get_model_returns(weights = models_weights, returns_act = portfolios_returns_act)

### Plot Results ---------------------------------------------------------------------------------------------------------------------------
# Select Start & End Dates for Plots (must be valid measurement_dates)
plot.start = as.Date("2013-11-01")
plot.stop = as.Date("2015-07-15")

# 2000-?-?   - 2019-02-08
# 2013-11-01 - 2018-11-27

# Select Subset Results to Graph & Adjust Cumulative Returns for Starting Date
plot.start = plot.start - 7 # Need 1 Week Prior to Adjust Cumulative Returns
plot.results <- model_results_cumulative[model_results_cumulative$date>=plot.start & model_results_cumulative$date<=plot.stop,]
plot.results <- plot.results %>% group_by(model) %>% mutate(cumreturn_plus_1=(1+cumulative_return/100)) %>%
  mutate_at(vars(cumreturn_plus_1),funs(./first(.))) %>% mutate(cumulative_return=(cumreturn_plus_1-1)*100) %>%
  select(model,date,return,cumulative_return)
plot.start = plot.start + 7 # Go Back to Original Start Date & Remove 1st Row
plot.results <- plot.results[plot.results$date>=plot.start & plot.results$date<=plot.stop,]


# Select Subset Results to Graph & Adjust Cumulative Returns for Starting Date
plot.results$mysize <- rep(0.5,nrow(plot.results))
plot.results$mysize[plot.results$model=="baseline"] <- 1.5
ggplot(data=plot.results, aes(x=date, y=cumulative_return, group=model, size=mysize)) +
  geom_line(aes(color=model)) + 
  labs(title=paste0(plot.start," to ", plot.stop,"  Input: ","Future ",as.numeric(sub("w_ret","",sub("fut_","",return_horizon)))," Week ",return_type, " Returns & ",return_aggregation," aggregation & ",rebalance_period," week rebalancing"), 
       x="Date", y="Cumulative Return (%)") +
  scale_x_date(date_breaks="3 months",date_labels="%b-%y") + 
  theme(axis.text.x = element_text(angle=90)) + scale_size(range = c(0.5, 1.5), guide="none")

# Timeseries plots showing portfolios selected for a given model
levels(models_weights$ID) <- c("beta_1y_D1", "beta_3y_D1", "sales_g_D1", "prev_12m_ret_D1", "prev_ret12m_l1m_D1", "earning_risk_D1", "roe_D1","mcap_D1", "bk_p_D1","div_yid_D1","ep_ltm_D1", "fcf_p_D1","sales_p_D1","12mvt_D1",
                      "beta_1y_D10", "beta_3y_D10","sales_g_D10", "prev_12m_ret_D10", "prev_ret12m_l1m_D10", "earning_risk_D10", "roe_D10","mcap_D10", "bk_p_D10","div_yid_D10","ep_ltm_D10", "fcf_p_D10","sales_p_D10","12mvt_D10")

ggplot(data=models_weights[models_weights$model=="markowitz_0.9_lamda",], aes(x=date, y=ID, group=weights)) +
  geom_point(aes(color=weights)) + 
  labs(title=paste0("Input: ","Future ",as.numeric(sub("w_ret","",sub("fut_","",return_horizon)))," Week ",return_type, " Returns & ",return_aggregation," aggregation & ",rebalance_period," week rebalancing"), 
       x="Date", y="Factor Portfolio") +
                    scale_x_date(date_breaks="6 months",date_labels="%b-%y") + 
                    theme(axis.text.x = element_text(angle=90)) +
                    scale_color_gradient2(low = "white", mid = "blue",
                                          high = "red",midpoint=0.5,breaks=seq(from=0,to=1,by=0.1))
                    




