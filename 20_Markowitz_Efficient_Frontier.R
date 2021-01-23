### This Script is just used to test the efficient frontier at a specific measurement date.

# Before Running this code you need to use "0_Main_Script.R" to generate variables up to and including "portfolios_returns_act"

date_test="2018-08-31" # Some valid measurement date from measurement_dates which
temp_horizon <- as.numeric(sub("w_ret","",sub("fut_","",portfolios$return[1])))

results <- NULL
for (lamda in seq(from=0.1,to=2,by=0.2)) {

  dvec <- as.numeric(portfolios_returns_act[portfolios_returns_act$period_formatted==date_test,2:length(portfolios_returns_act)])
  Dmat <- 2*lamda*cov(portfolios_returns_act[1:(which(portfolios_returns_act$period_formatted==date_test)-temp_horizon),2:length(portfolios_returns_act)])
  Amat <- cbind(rep(1, length(dvec)), diag(1, length(dvec)))
  bvec <- c(1, rep(0, length(dvec)))
  
  sol = solve.QP(Dmat,dvec,Amat,bvec,meq=1)

  expectation <- t(dvec) %*% sol$solution
  covariance <- (sol$value + expectation)/lamda
  volatility <- sqrt(covariance)
  
  results <- rbind(results,c(lamda,expectation,volatility))
}

plot(x=results[,3], y=results[,2], xlab = "Optimal Volatility", ylab= "Optimal Expected Returns", main = "Performance of risk-aversion parameter from 0.1 to 2 on 2018-08-31",cex.main=1, cex.lab=1, col="red",pch=18, cex=2)
#text(results[,3]~results[,2],labels=seq(0.1,1,by=0.1),cex=1.2, color=2, font=2, adj=NULL)
text(results[,3]~results[,2],labels=seq(0.1,2,by=0.2),cex=1.2, font=2, adj=1.2)
#rm(Amat,covariance,Dmat,expectation,results,sol,volatility,bvec,date_test,dvec,lamda,temp_horizon)