### Function: PCA Model  --------------------------
get_pca <- function(
  scenario_name = NULL,
  expected_returns,
  approach = "noneg", # Valid Values are: act, noneg, abs
  date_test,
  horizon,
  portfolios,
  which.pc="best") {
  
  stopifnot((length(which.pc)==1 | which.pc=="all" | which.pc=="best"))
  stopifnot((approach=="noneg" | approach=="act" | approach=="abs"))
  
  n = dim(portfolios)[1]
  if(is.null(scenario_name)) {
    scenario_name = paste0("pca_",approach,"_",which.pc)  
  }
  
  index.start = which(expected_returns$period_formatted==date_test)
  
  data.cor<-cor(expected_returns[index.start:(index.start+horizon-1),2:length(expected_returns)], use = "complete.obs")
  data_pca_cor<-prcomp(data.cor, scale. = TRUE)
  pca_cor_aw<-as.data.frame(data_pca_cor$rotation)
  
  if(approach== "act") {
    pca_final <- pca_cor_aw %>% mutate_if(is.numeric, funs(./sum(.)))
  } else if(approach== "noneg") {
    pca_cor_nn <- as.data.frame(replace(pca_cor_aw, pca_cor_aw<0, 0))
    pca_final <- pca_cor_nn %>% mutate_if(is.numeric, funs(./sum(.)))
  } else if(approach=="abs") {
    pca_cor_ab<-as.data.frame(abs(pca_cor_aw))
    pca_final <- pca_cor_ab %>% mutate_if(is.numeric, funs(./sum(.)))
  }
  
  if (which.pc!="all" & which.pc!="best") {
    results <- data.frame(ID=portfolios$ID,model=rep(scenario_name,times=n),date=rep(as.Date(date_test),times=n),weights=round(pca_final[[which.pc]],9))
  } else if (which.pc=="best") {
    cum_returns_portfolio = (apply(expected_returns[index.start:(index.start+horizon-1),2:length(expected_returns)]/100+1,MARGIN=2,FUN=prod)-1)*100
    cum_returns_PC = pca_final %>% apply(MARGIN=2,FUN=function(x) x*cum_returns_portfolio) %>% 
      apply(MARGIN=2, FUN=sum)
    best_pc = names(cum_returns_PC[cum_returns_PC==max(cum_returns_PC)])
    results <- data.frame(ID=portfolios$ID,model=rep(scenario_name,times=n),date=rep(as.Date(date_test),times=n),weights=round(pca_final[[best_pc]],9))
  } else {
    results <- pca_final
    results$ID <- as.factor(paste0("portfolio",ifelse(nchar(rownames(pca_final))==1,"0",""),rownames(pca_final)))
    results$date= rep(as.Date(date_test),times=n)
    results <- gather(results,model,weights,-ID,-date)    
    results$model = paste(sep="","pca_",approach,"_",results$model)
  }

  return(results)
  
}


### Function: Markowitz Model  --------------------------
get_Markowitz <- function (
  scenario_name = NULL, 
  expected_returns,
  historic_returns,
  date_test, 
  lamda=0.5,
  horizon,
  portfolios) {

  n = dim(portfolios)[1]
  if(is.null(scenario_name)) {
    scenario_name = paste0("markowitz_",lamda,"_lamda")
  }
  
  dvec <- as.numeric(expected_returns[expected_returns$period_formatted==date_test,2:length(expected_returns)])
  Dmat <- 2*lamda*cov(historic_returns[1:(which(historic_returns$period_formatted==date_test)-horizon),2:length(expected_returns)])
  Amat <- cbind(rep(1, length(dvec)), diag(1, length(dvec)))
  bvec <- c(1, rep(0, length(dvec)))

  sol = solve.QP(Dmat,dvec,Amat,bvec,meq=1)
  
  results <- data.frame(ID=portfolios$ID,model=rep(scenario_name,times=n),date=rep(as.Date(date_test),times=n),weights=round(sol$solution,9))
  
  return(results)
}


### Function: Naive Baseline Weights  --------------------------
get_baseline <- function (
  scenario_name ="baseline", 
  portfolios,
  date_test) {
  
  n = dim(portfolios)[1]
  results <- data.frame(ID=portfolios$ID,model=rep(scenario_name,times=n),date=rep(as.Date(date_test),times=n),weights=rep(1/n,times=n))
  
  return(results)
}
