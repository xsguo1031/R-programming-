### Function: Define Factor Portfolios  --------------------------

# Create porfolios using all possible combinations of inputs below 
define_portfolios <- function(
  input_type = "Absolute", # Excess or Absolute
  input_return = "fut_24w_ret", # Any column from Principal table of returns
  input_aggregation = "Dynamic", # Dynamic or Static
  input_deciles = c("D1","D10"), # Any value in {D1,D2,...,D10}
  input_factors = c("bk_p","fcf_p","ep_ltm","sales_p","prev_12m_ret","prev_ret12m_l1m","div_yid","earning_risk","roe",
                     "12mvt","mcap","beta_3y","beta_1y","sales_g") # Any column from Principal factor_weekly table
  ) {
  
  input_metric <- c("mean") # Code only supports mean currently
  
  inputs <- list(factor=input_factors,
                 decile=input_deciles,
                 metric=input_metric,
                 type=input_type,
                 return=input_return,
                 aggregation=input_aggregation)
  
  portfolios <- data.frame(expand.grid(inputs))
  portfolios <- data.frame(ID=paste("portfolio",ifelse(nchar(1:dim(portfolios)[1])==1,"0",""),1:dim(portfolios)[1],sep=""),portfolios)
  
  # Add Factor Groupings to Portfolio Definitions
  group <- matrix(nrow=62,ncol=2)
  group[1:2,1] <- c("fy1_3mchg","fy2_3mchg")
  group[1:2,2] <- c(rep("Analyst_Sent",2))
  group[3:4,1] <- c("beta_1y","beta_3y")
  group[3:4,2] <- c(rep("beta",2))
  group[5:6,1] <- c("hist_eps_1yg","sales_g")
  group[5:6,2] <- c(rep("growth",2))
  group[7:30,1] <- c("ir_sen_2y","ir_sen_10y","ir_spread_sen","ted_sen","crb_com_sen","oil_sen","usd_idx_sen",
                       "us3mo","us2yr","us10yr","pairwise_avg_corr_index_30day","dxy_z","gold",
                       "us_govt_yield_-_3_mo","us_govt_yield_-_1_yr","us_govt_yield_-_2_yr","us_govt_yield_-_5_yr",
                       "us_govt_yield_-_10_yr","fed_fund_target_rate","usd_jpy","usd_cny","eur_usd","gbp_usd",
                       "citi_economic_surprise_index_united_states")
  group[7:30,2] <- c(rep("macro",24))
  group[31:36,1] <- c("prev_1m_ret","prev_3m_ret","prev_6m_ret","prev_9m_ret","prev_12m_ret","prev_ret12m_l1m")
  group[31:36,2] <- c(rep("momentum",6))
  group[37:42,1] <- c("roe","roa","asset_tover","std_eps","earning_risk","gross_mgn")
  group[37:42,2] <- c(rep("quality",6))
  group[43:44,1] <- c("mcap","si_outshs")
  group[43:44,2] <- c(rep("size",2))
  group[45:48,1] <- c("1m_6musdvol_chg","1m_6mvol_chg","p_volume_1w","p_volume_1m")
  group[45:48,2] <- c(rep("techincal",4))
  group[49:58,1] <- c("cfo_p","fcf_p","net_cfo_p","ep_ltm","bk_p","sales_p","div_yid",
                        "fy1_p","fy2_p","eps_p_ntm")
  group[49:58,2] <- c(rep("value",10))
  group[59:62,1] <- c("1mvt","3mvt","6mvt","12mvt")
  group[59:62,2] <- c(rep("volatility",4))
  
  portfolios$group <- mapvalues(portfolios$factor,group[,1],group[,2],warn_missing = FALSE)
  
  portfolios <- portfolios[order(as.character(portfolios$decile),     # Sort Portfolios
                                 as.character(portfolios$group),
                                 as.character(portfolios$factor)),] 
  portfolios$ID <- sort(portfolios$ID,decreasing = TRUE) # Reassign Portfolio ID to align with Sort ORder.
  return(portfolios)
}
