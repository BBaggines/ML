#Expectation Maximization Problem
library(mvtnorm)

Mu_initialization <- function(data, k){
  mu <- matrix(0,ncol = ncol(data),nrow = k)
  dist_from_centroids <- matrix(0, nrow = nrow(data), ncol = k)
  probability <- matrix(0,nrow = nrow(data), ncol = 1)
  
  #first randomly initialized centroid
  mu[1,] <- as.matrix(data[sample(nrow(data), 1),])
  
  #computing next set of centroids using the D(x)^2
  for (i in 2:k){
    colnames(mu) <- colnames(data)
    x <- rbind(data,mu[1:i-1,])
    x <- as.matrix(dist(x))
    for (j in 1:i-1){
      dist_from_centroids[,j] <- x[nrow(data)+j,1:nrow(data)]
    }
    probability <- apply(dist_from_centroids, 1, function(x) max(x))
    probability <- (probability^2)/sum(probability^2)
    new_centroid <- which.max(probability)
    mu[i,] <- as.matrix(data[new_centroid,])
  }
  
  return(mu)
}


EM_algo_new <- function(data, k, threshold_value, update = "Y"){
  # initializing vectors
  sig <- list()
  p <- vector()
  mu <- t(Mu_initialization(data, k))
  
  # initializing mean and st deviation
  # mu <- t(data[sample(nrow(data), k),])
  
  for(i in 1:k){
    sig[[i]] <- diag(ncol(data))
    p<-c(p,1/k)
  }
  
  # E M Algorithm
  dist_mu <- list()
  iter <- 0
  repeat{
    iter <- iter +1
    # Expectation step
    cond_prob <- matrix(0,nrow = nrow(data), ncol = k)
    
    for(j in seq(1,k)){
      cond_prob[,j] <- dmvnormDF(x = data, mean = matrix(mu[,j]),
                           sigma = sig[[j]])
    }
    cond_prob <- t(apply(cond_prob,1,function(x) x/sum(x)))
    #sum(apply(cond_prob, 1, function(x) sum(x)))
    if(sum(log(cond_prob)) < threshold_value){
      break
    }
    
    # Maximization step
    old_mu <- mu
    if(update == "Y"){
      p <- apply(cond_prob, 2, mean)
    }
    
    singular <- list()
    for(j in seq(1,k)){
      # Check whether the condition probablities are all zero i.e the covariance matrix 
      # was singular for a particular cluster
      if(any(cond_prob[,j]<=0) || sum(cond_prob[,j] == 0)){
        singular <- c(singular, j )
        next
      }
      temp <- cov.wt(x = data, wt = cond_prob[,j])
      mu[,j] <- temp$center
      if(update == "Y"){
      sig[[j]] <- temp$cov
      }
    }
    
    for(j in singular){
      mu[,j] <- t(data[sample(nrow(data), 1),])
      sig[[j]] <- diag(ncol(data))
    }
    
    curr_dist <- distance_mu(old_mu, mu, k)
    print(curr_dist)
    dist_mu <- c(dist_mu,curr_dist)
    if(dist_mu[length(dist_mu)] <= threshold_value){
      break 
    }
  }
  
  # returns the cluster
  res <- list(iterations <- iter,
  cluster <- apply(cond_prob, 1, function(x) which.max(x))
  )
  return(res)
}

filename <- "ionosphere.csv"
data <- read.csv(filename)
category <- data[,35]
data <- data[,-c(1)]
data <- data[,-c(1,34)]

error_1 <- matrix(0,nrow = 20, ncol = 5)
error_2 <-  matrix(0,nrow = 20, ncol = 5)
iteration_1 <- matrix(0, nrow = 20, ncol = 5)
iteration_2 <- matrix(0, nrow = 20, ncol = 5)

for(k in 2:5){
  threshold_value <- 1/10000
  for(j in 1:20){
    #initial centroids
    initial_centroids <- data[sample(nrow(data), k),]
    
    # EM old
    res <- EM_algo(data, k , threshold_value, update = 'Y',mu = t(initial_centroids))
    iteration_2[j, k] <- res[[1]]
    x <- combine_cluster(cbind(data,category,center = res[[2]]))
    error_2[j, k] <- sum(error_rateDF(x))
    
    # EM new
    res <- EM_algo_new(data, k, threshold_value)
    iteration_1[j, k] <- res[[1]]
    x <- combine_cluster(cbind(data,category,center = res[[2]]))
    error_1[j, k] <- sum(error_rateDF(x))
    
  }
}


iteration_2 <- iteration_2[,-1]
error_2 <- error_2[,-1]

colnames(error_2) <- c("k2","k3","k4","k5")
error_2 <- data.frame(melt(error_2))
error_2$label <- "EM old"
colnames(error_2) <- c("Run Number","Error","value","label")

colnames(iteration_2) <- c("k2","k3","k4","k5")
iteration_2 <- melt(iteration_2)
iteration_2$label <- "EM old"
colnames(iteration_2) <- c("Run Number","Iteration","value","label")


iteration_1 <- iteration_1[,-1]
error_1 <- error_1[,-1]

colnames(error_1) <- c("k2","k3","k4","k5")
error_1 <- melt(error_1)
error_1$label <- "EM new"
colnames(error_1) <- c("Run Number","Error","value","label")

colnames(iteration_1) <- c("k2","k3","k4","k5")
iteration_1 <- melt(iteration_1)
iteration_1$label <- "EM new"
colnames(iteration_1) <- c("Run Number","Iteration","value","label")

# Plot whisker plot 
ggplot(data = rbind(error_1,error_2), aes(x=Error, y=value)) + geom_boxplot(aes(fill=label))
ggplot(data = rbind(iteration_1,iteration_2), aes(x=Iteration, y=value)) + geom_boxplot(aes(fill=label))


