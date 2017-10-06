# K means lloylds algorithms

# calculates the distance between each data point of df1, df2 
distanceDF <- function(df1,df2){
  dfx <- rbind(df1,df2)
  dist <- as.matrix(dist(dfx), label=true)
  dist <- dist[(nrow(df1)+1):(nrow(dfx)),1:nrow(df1)]
  return(dist)
}

# Returns boolean whether the list of centroids are within threshold
# by calculating the sum of distance between each centroid
thresholdDF <- function(old_center, new_center, threshold_value)
{
  dist_center <- 0
  for(i in 1:k){ 
      temp <- 0
      for(j in 1:ncol(new_center)){
        temp <- temp + (new_center[i,j]-old_center[i,j])^2
      }
      dist_center <- dist_center + sqrt(temp)
  }
  print(dist_center)
  if (dist_center <= threshold_value){
    return(TRUE)
  }else
    return(FALSE)
}

#calculates the clusters
clusterDF<- function(df,dist_center){
  namevector <- c("center")
  df[,namevector] <- NA
  for(i in seq(1:nrow(df))){
    df[i,]$center <- which.min(dist_center[,i])
  }
  return(df)
}

#calculates error rate according to number of b and g data points
error_rateDF <- function(data){
  error <- c()
  tot_error <- c()
  n_predicted_cluster <- length(table(data$center))
  for (i in seq(1:n_predicted_cluster)){
    no_of_g <- as.matrix(table(data[data$center==i,]$category))[2]
    no_of_b <- as.matrix(table(data[data$center==i,]$category))[1]
    
    if(is.na(no_of_b)){
      no_of_b <- 0
    }
    else if (is.na(no_of_g)){
      no_of_g <- 0
    }
    
    if (no_of_b > no_of_g){
      error <- c(error,no_of_g/(no_of_b + no_of_g))
    }else{
      error <- c(error,no_of_b/(no_of_b+no_of_g))
    }
    
    tot_error <- c(tot_error,error[i])
  }
  return(tot_error)
}

#calculates the new centers based on clustered data
new_centerDF<- function(data){
  n_center <- aggregate(data[,1:(ncol(data)-1)], list(data$center), mean)
  n_center <- n_center[,2:ncol(n_center)]
  return(n_center)
}

kmeans_algo <- function(data,k,threshold_value,old_center){
  data <- data.frame(data)
  
  #initialize center
  data$center <- NA
  
  #random selection of first centroids
  #old_center <- data[sample(nrow(data),k),-ncol(data)]
  
  #intialize threshold
  threshold <- FALSE
  
  iter <- 0
  #Re calculating centroid till the centroids remain same
  while(!threshold){
    iter <- iter+1
    
    #remove the old clusters
    data <- data[,-ncol(data)]
    
    #calculate distance of old centers from all the data points
    distance_from_center <- distanceDF(data,old_center) 
    
    #cluster the data on basis of the center
    data <- clusterDF(data,distance_from_center)
    
    #calculate new centers on basis of clusters formed
    new_center <- new_centerDF(data)
    
    #calculate if threshold is met
    threshold <- thresholdDF(old_center, new_center, threshold_value)
    
    old_center <- new_center
    
  }
  result <- list(iter, data$center)
  return (result)
}
