library(matrixStats)

# Rank0 is used when the first dim of r_instance is  1
Rank0 <- function(r_instance, r_obs){
  # calculate the distance between the J scenarios and observation for K instances
  MTD0 <- numeric()
  for (k in 1:dim(r_instance)[3]){
    MTD0[k] <- sqrt(sum((r_instance[,,k]-r_obs[,k])^2))
  }
  
  
  # calculate the distance between the J-1 scenarios + observation and scenario i for K instances
  MTD <- matrix(NA, nrow = dim(r_instance)[2], ncol= dim(r_instance)[3])
  for (k in 1:dim(r_instance)[3]){
    for (i in 1:dim(r_instance)[2]){
      re <- r_instance
      o <- r_obs
      re[,i,k] <- r_obs[,k] 
      o[,k] <- r_instance[,i,k]
      MTD[i,k] <- sqrt(sum((re[,,k]-o[,k])^2))
    }
  }

  dist_combine <- -rbind(MTD, MTD0)
  # Sort the rank from the largest to smallest
  rank0 = (colRanks(dist_combine, ties.method ="average"))[,dim(dist_combine)[1]] 
  return(rank0)
}

# Didem's code 
A <- function (scenarios, observation, prob = NULL, debias = FALSE, 
               transformation = FALSE) 
{
  n.instance <- dim(observation)[2]
  n.scen <- dim(scenarios)[2]
  n.length <- dim(observation)[1]
  if (is.null(prob)) {
    prob <- array(rep(1/n.scen, times = n.scen * n.instance), 
                  dim = c(n.scen, n.instance))
  }
  if (all(apply(prob, 2, sum) < 1.0001 & apply(prob, 2, sum) > 
          0.9999)) {
    if (dim(observation)[1] == dim(scenarios)[1] && dim(observation)[2] == 
        dim(scenarios)[3] && dim(prob)[1] == dim(scenarios)[2] && 
        dim(prob)[2] == dim(scenarios)[3]) {
      if (debias) {
        z <- array(, dim = c(n.instance, n.length))
        for (i in 1:n.instance) {
          a <- rbind(observation[, i], t(scenarios[, 
                                                   , i]))
          for (h in 1:n.length) {
            z[i, h] <- mean(as.matrix(a)[-1, h]) - as.matrix(a)[1, 
                                                                h]
          }
        }
      }
      combine <- as.matrix(matrix(nrow = (n.scen + 1), 
                                  ncol = n.length))
      distances <- array(, dim = c(n.scen + 1, n.scen + 
                                     1, n.instance))
      moving.cost <- matrix(, n.scen + 1, n.instance)
      rank.actual <- vector(, n.instance)
      for (i in 1:n.instance) {
        if (n.length == 1) {
          a <- t(cbind(observation[, i], t(scenarios[, 
                                                     , i])))
          combine <- a
        }
        else {
          a <- rbind(observation[, i], t(scenarios[, 
                                                   , i]))
          combine <- a
        }
        if (debias) {
          combine <- a
          for (s in 2:(n.scen + 1)) {
            for (h in 1:n.length) {
              combine[s, h] <- as.matrix(a)[s, h] - mean(z[, 
                                                           h])
            }
          }
        }
        if (transformation) {
          ctr <- colMeans(combine)
          S <- cov(combine)
          Seig <- eigen(S)
          if (sum(Seig$values < 0) == 0) {
            sqrtD <- sqrt(Seig$values)
            if (n.length == 1) {
              SsqrtInv <- Seig$vectors %*% (1/sqrtD) %*% 
                t(Seig$vectors)
            }
            else {
              SsqrtInv <- Seig$vectors %*% diag(1/sqrtD) %*% 
                t(Seig$vectors)
            }
            ctr_matrix <- matrix(nrow = n.scen + 1, ncol = n.length)
            for (s in 1:(n.scen + 1)) {
              ctr_matrix[s, ] <- ctr
            }
            Xdot <- as.matrix(combine) - as.matrix(ctr_matrix)
            combine <- t(SsqrtInv %*% t(Xdot))
          }
        }
        distances[, , i] <- as.matrix(dist(combine, method = "euclidean", 
                                           diag = FALSE, upper = FALSE, p = 2))
        for (s in 1:(n.scen + 1)) {
          if (s == 1) {
            moving.cost[1, i] <- sum(distances[, s, i] * 
                                       c(0, prob[, i]))
            next
          }
          moving.cost[s, i] <- sum(distances[, s, i] * 
                                     c(prob[s - 1, i], prob[, i]))
        }
        rank.actual[i] <- (n.scen + 1) - rank(moving.cost[, 
                                                          i])[1] + 1
      }
      # hist(rank.actual, breaks = c(0:(n.scen + 1)), xlab = "bin", 
      #      ylab = "frequency", col = "gray", main = "MTD rh", 
      #      ylim = c(0, n.instance/2))
      return(rank.actual)
    }
    else {
      print("The number of scenario and observation instances in and the dimension of scenarios and observation should be equal.")
    }
  }
  else {
    print("The sum of the probabilities of scenarios should equal 1 for each instance.")
  }
}

# Rank is used when the first dim of r_instance is larger than  1
# calculate the distance between the J scenarios and the observation for K instances
Rank <- function(r_instance, r_obs){
  d.ob.gen <- matrix(NA, nrow = dim(r_instance)[2], ncol = dim(r_instance)[3])
  for (k in 1:dim(r_instance)[3]){
    d.ob.gen[,k] <- apply(r_instance[,,k],2,function(x)sqrt(sum((x-r_obs[,k])^2)))
  }
  MTD0 <- colMeans(d.ob.gen)
  
  # calculate the distance between the J-1 scenarios + observation and scenario i for K instances
  d.gen <- matrix(NA, nrow = dim(r_instance)[2], ncol = dim(r_instance)[3])
  MTD <- array(NA, dim = c(dim(r_instance)[3], dim(r_instance)[2]))
  for (i in 1:dim(r_instance)[2]){
    re <- r_instance
    o <- r_obs
    re[,i,] <- r_obs[,]
    o[,] <- r_instance[,i,]
    for (k in 1:dim(r_instance)[3]){
      d.gen[,k] <- apply(re[,,k],2,function(x)sqrt(sum((x-o[,k])^2)))
    }
    MTD[,i] <- colMeans(d.gen)
  }
  
  dist_combine <- -cbind(MTD, MTD0)
  rank0 = (rowRanks(dist_combine, method ="first"))[,(dim(r_instance)[2]+1)]
  return(rank0)
}





# Method 2
# library(philentropy)
# Rank_2 <- function(r_instance, r_obs){
#   for (i in 1:K){
#     gen <- t(cbind(r_instance[,,i], r_obs[,i]))
#     D <- distance(as.data.frame(gen), method = "euclidean")
#     D_bar <- as.numeric(rowMeans(D))
#     Rank_Matrix[i] <- rank(-D_bar)[iteration+1]
#   }
#   return(Rank_Matrix)
# }
# Rank_2(r_instance, r_obs)
# Rank_2(S_instance, S_obs)


##---------------De-bias Wilks----------------------
debias <- function(r_instance, r_obs){
  debiased_r <- array(NA, dim=dim(r_instance))
  for (t in c(1:dim(r_instance)[3])){
    debiased_r[,,t] <- (r_instance[,,t] - (rowMeans(r_instance[,,t])- r_obs[,t]))
  }
  return(debiased_r)
}



##---------------De-bias Sari----------------------
debias_S <- function(r_instance,r_obs){
  debiased_r <- array(NA, dim=dim(r_instance))
  for (i in c(1:dim(r_instance)[1])){
    debiased_r[i,,] <- (r_instance[i,,] - sum(colMeans(r_instance[i,,])- r_obs[i,])/dim(r_instance)[3])
  }
  return(debiased_r)
}


##-------------------------way 2 to do the transform--------------------
transform <- function(r_instance, r_obs){
  
  r_mean <-  array(NA,dim = dim(r_obs))
  r_cov <-  array(NA,dim = c(dim(r_obs)[1], dim(r_obs)[1], dim(r_obs)[2]))
  transformed_r_obs <- array(NA,dim = dim(r_obs))
  transformed__r_generated <- array(NA, dim = dim(r_instance))
  result <- list()
  
  r_instance <- debias_S(r_instance, r_obs)
  combine_r <- abind(r_instance, r_obs, along = 2)
  
  for (j in c(1:dim(r_instance)[3])){
    r_mean[,j] <- rowMeans(combine_r[,,j])
    r_cov[,,j] <- cov(t(combine_r[,,j]))
  }
  
  for (j in c(1:dim(r_instance)[3])){
    #for (k in c(1:dim(r_instance)[2])){
    transformed_r_obs[,j] <- (sqrtm(inv(r_cov[,,j])) %*% (r_obs[,j]-r_mean[,j]))
    transformed__r_generated[,,j] <- (sqrtm(inv(r_cov[,,j])) %*% (r_instance[,,j]-r_mean[,j]))
    #}
  }
  
  result <- list("r_instance" = transformed__r_generated, "r_obs" = transformed_r_obs)
  return(result)
}





