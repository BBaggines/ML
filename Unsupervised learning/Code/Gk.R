#Expectation Maximization Problem
library(mvtnorm)

# calculate distance between previous and latest mu
distance_mu <- function(old_mu, new_mu, k)
{
  dist_center <- 0
  # sum of squares of distance between means for each center 
  for (i in 1:k){
    dist_center <- dist_center + (dist(rbind(old_mu[[i]],new_mu[[i]]))[1])^2
  }
  return(dist_center)
}


combine_cluster <- function(x){
  # x <- cbind(data,category,center = res[[2]])
  true_clusters <- aggregate(x[,-c(ncol(x),ncol(x)-1)], 
                             list(x$category), mean)
  predicted_clusters <- aggregate(x[,-c(ncol(x),ncol(x)-1)], 
                                  list(x$center), mean)
  dist_tc <- data.frame()
  if(nrow(predicted_clusters)>2){
    for(i in 1:nrow(predicted_clusters)){
      dist_tc[i,1] <- dist(rbind(true_clusters[1,-1],predicted_clusters[i,-1]))
      dist_tc[i,2] <- dist(rbind(true_clusters[2,-1],predicted_clusters[i,-1]))
    }
    dist_tc$final_cluster <- (apply(dist_tc, 1, function(x) which.min(x)))
    for(i in predicted_clusters$Group.1 ){
      x[x$center == i,]$center <- dist_tc[i,3]
    }
  }
  return(x)
}


dmvnormDF <- 
  function (x, mean, sigma) 
  {
    if (is.vector(x)) {
      x <- matrix(x, ncol = length(x))
    }
    if (missing(mean)) {
      mean <- rep(0, length = ncol(x))
    }
    if (missing(sigma)) {
      sigma <- diag(ncol(x))
    }
    if (ncol(x) != ncol(sigma)) {
      stop("x and sigma have non-conforming size")
    }
    if (nrow(sigma) != ncol(sigma)) {
      stop("sigma meanst be a square matrix")
    }
    if (length(mean) != nrow(sigma)) {
      stop("mean and sigma have non-conforming size")
    }
    
    # retval <- exp(-mahalanobis(x, center = mean, cov = sigma)/2)
    retval <- as.matrix(sweep(x,2,mean))%*%pseudoinverse(sigma)%*%t(sweep(x,2,mean))
    retval <- exp(-diag(retval)/2)
    
    det <- prod(eigen(sigma, sym = TRUE)$values)
    retval <- retval/(sqrt(det) * sqrt(2 * pi)^ncol(x))
    retval
  }



EM_algo <- function(data, k, threshold_value, update = "Y", mu){
  # initializing vectors
  sig <- list()
  p <- vector()
  #mu <- matrix(0,nrow = ncol(data), ncol =k)
  
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
    
    # Maximization step
    old_mu <- mu
    if(update == "Y"){
      p <- apply(cond_prob, 2, mean)
    }
    
    singular <- list()
    for(j in seq(1,k)){
      # Check whether the condition probablities are all zero i.e the covariance matrix 
      # was singular for a particular cluster
      if(any(cond_prob[,j]< 0) || sum(cond_prob[,j] == 0)){
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
  res <- list(
    iterations <- iter,
    cluster <- apply(cond_prob, 1, function(x) which.max(x))
  )
  return(res)
}

