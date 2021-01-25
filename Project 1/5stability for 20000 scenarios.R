library(dplyr)
library(PerformanceAnalytics)
library(xts)
library(MFTSR) # for ewma vol
library(zoo)
library(data.table) # for combine the dataframe to get the solution


# after Algorithm for month seed.R, we know that G=10 can provide the best result
T = 12
F = 1
G = 10


# load data from 1998-12-01
monthdata <- read.csv("1926SP.csv")
monthdata$DATE <- as.Date(as.character.Date(monthdata$caldt),format = "%Y %m %d")
monthdata <- monthdata %>% filter( (DATE>as.Date("1998-12-01")) & (DATE<as.Date("2019-12-31")))
Trea1926 <- read.csv("1926T.csv")
Trea1926$DATE <- as.Date(as.character.Date(Trea1926$caldt),format = "%Y %m %d")
C1926 <- inner_join(Trea1926, monthdata, by = "DATE")
df_month <- subset(C1926, select = c("DATE", "t30ret", "spindx"))
B_m <- head(df_month$t30ret,-1)


# build a function to check the stability 
Stability <- function(Nseq, Iteration, Lambda, Alpha){
  
  # random generate Nseq seeds 
  SEED <- floor(runif(Nseq, min=0, max=5000)) 
  
  # get the weight of the Nseq sequences 
  weight <- list()
  i = 0
  for(a in SEED){
    i = i+1
    scenario <- Scenario(monthdata = monthdata, G = G, iteration = Iteration, seed = a, mov_average = "wma")
    weight[[i]] <- OPT(r = scenario, f = B_m[(T+G+1):length(B_m)], lambda = Lambda, alpha = Alpha)
  }
  
  # compare the weights 
  K = length(weight[[1]]) # number of instances or length of each sequence 
  similarity <- matrix(NA, ncol=Nseq, nrow=Nseq)
  for (i in 1:Nseq){
    for (j in c(i:Nseq)){
      similarity[i,j] = sum(weight[[i]] == weight[[j]])/K
    }
  }
  
  # calculate the average for the upper-diagonal entries 
  Sim <- mean(similarity[row(similarity)!=col(similarity)],na.rm=T)
  
  return(Sim)
}


stability <- matrix(NA, nrow = 6, ncol = 3)
i = 1
for (lambda in c(0, 0.02, 0.04, 0.06, 0.08, 0.1)){
  j = 1
  for (alpha in c(0.75, 0.90, 0.99)){
    stability[i,j] <- Stability(Nseq = 30, Iteration = 20000, Lambda = lambda, Alpha = alpha)
    j = j+1
    print(j)
  }
  i = i+1
  print(i)
}  
  

