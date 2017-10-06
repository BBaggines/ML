# Principal component Analysis

setwd("C:\\Users\\shrad\\Desktop\\AML\\Assignment 1")

file_name_1 <- "ringnorm_data.csv"
file_name_2 <- "ionosphere.csv"

ringnorm_data <- read.csv(file_name_1)
ionosphere_data <- read.csv(file_name_2)

#removed the category data
category_ring <- ringnorm_data[,1]
ringnorm_data <- ringnorm_data[,-1]
# ALso, removing constant column from ionosphere data
category_ionosphere <- ionosphere_data[,35]
ionosphere_data <- ionosphere_data[,-c(35,2)]


#Principal component analysis function
pc_ringnorm <- prcomp(ringnorm_data, scale. = TRUE)
pc_ionosphere <- prcomp(ionosphere_data, scale. = TRUE)

names(pc_ringnorm)

# center i.e mean of the data
  pc_ringnorm$center
  pc_ionosphere$center

#principal loadings
  pc_ringnorm$rotation
  pc_ionosphere$rotation

#Scree plots of the principal loadings
  #RINGNORM
  
  #standard deviation of the data
  pc_ringnorm$sdev
  pc_ringnorm$var <- pc_ringnorm$sdev^2
  
  # proportion of variance
  pc_ringnorm$pve <- (pc_ringnorm$sdev^2/sum(pc_ringnorm$var))
  plot(pc_ringnorm$pve, xlab = "Principal Component", ylab = "Proportion of Variance",
       type = 'b')
  plot(cumsum(pc_ringnorm$pve), xlab = "Cumulative Principal Component", 
       ylab = "Proportion of Variance", type = 'b')
  
  #IONOSPHERE
  #standard deviation of the data
  pc_ionosphere$sdev
  pc_ionosphere$var <- pc_ionosphere$sdev^2
  
  # proportion of variance
  pc_ionosphere$pve <- (pc_ionosphere$sdev^2/sum(pc_ionosphere$var))
  plot(pc_ionosphere$pve, xlab = "Principal Component Ionosphere",
       ylab = "Proportion of Variance", type = 'b')
  plot(cumsum(pc_ionosphere$pve*100), xlab = "Cumulative Principal Component Ionosphere",
       ylab = "Proportion of Variance", type = 'b')
  
#First two principal componenets
  plot(pc_ringnorm$x[,1], pc_ringnorm$x[,2],
       main="First 2 PC : Ringnorm Data", xlab = "PC1", ylab = "PC2")
  plot(pc_ionosphere$x[,1],pc_ionosphere$x[,2],
       main="First 2 PC : Ionosphere Data",  xlab = "PC1", ylab = "PC2")
  
# keeping 90% variance in data
  # RINGNORM
  # Removing the last two principal componenet loadings which attribute to 4.5% and 4.3%
  ringnorm_data <- as.data.frame(pc_ringnorm$x[,-c(ncol(pc_ringnorm$x),
                                                   ncol(pc_ringnorm$x)-1)])
  
  data <- ringnorm_data
  
  error_1 <-matrix(0,nrow = 20, ncol = 5)
  error_2 <-  matrix(0,nrow = 20, ncol = 5)
  iteration_1 <- matrix(0, nrow = 20, ncol = 5)
  iteration_2 <- matrix(0, nrow = 20, ncol = 5)
  
  for(k in 2:5){
    threshold_value <- (k-1)/100
    for(j in 1:20){
      #initial centroids
      initial_centroids <- data[sample(nrow(data), k),]
      
      #EM
      res <- EM_algo(data, k , threshold_value, mu = t(initial_centroids))
      iteration_2[j, k] <- res[[1]]
      x <- combine_cluster(cbind(data, category_ring ,center = res[[2]]))
      colnames(x)[ncol(data)+1] <- "category"
      error_2[j, k] <- sum(error_rateDF(x))
      
      # kmeans
      res <- kmeans_algo(data, k , threshold_value, old_center = initial_centroids)
      iteration_1[j, k] <- res[[1]]
      x <- combine_cluster(cbind(data, category_ring, center = res[[2]]))
      colnames(x)[ncol(data)+1] <- "category"
      error_1[j, k] <- sum(error_rateDF(x))
      
      
    }
  }
  
  iteration_2 <- iteration_2[,-1]
  error_2 <- error_2[,-1]
  
  colnames(error_2) <- c("k2","k3","k4","k5")
  error_2 <- data.frame(melt(error_2))
  error_2$label <- "EM"
  colnames(error_2) <- c("Cluster","Error","value","label")
  
  colnames(iteration_2) <- c("k2","k3","k4","k5")
  iteration_2 <- data.frame(melt(iteration_2))
  iteration_2$label <- "EM"
  
  iteration_1 <- iteration_1[,-1]
  error_1 <- error_1[,-1]
  
  colnames(error_1) <- c("k2","k3","k4","k5")
  error_1 <- data.frame(melt(error_1))
  error_1$label <- "kmeans"
  colnames(error_1) <- c("Cluster","Error","value","label")
  
  
  colnames(iteration_1) <- c("k2","k3","k4","k5")
  iteration_1 <- data.frame(melt(iteration_1))
  iteration_1$label <- "kmeans"
  colnames(error_1) <- c("Cluster","Error","value","label")
  
  
  # Plot whisker plot 
  ggplot(data = rbind(error_1,error_2), aes(x=variable, y=value)) + geom_boxplot(aes(fill=label))
  ggplot(data = rbind(iteration_1,iteration_2), aes(x=variable, y=value)) + geom_boxplot(aes(fill=label))
  
  
  #IONOSPHERE
  # Removing the last 14 PCs 
  ionosphere_data <- as.data.frame(pc_ionosphere$x[,seq(1,19)])
  
  data <- ionosphere_data
  
  
  error_1 <- matrix(0,nrow = 20, ncol = 5)
  error_2 <-  matrix(0,nrow = 20, ncol = 5)
  iteration_1 <- matrix(0, nrow = 20, ncol = 5)
  iteration_2 <- matrix(0, nrow = 20, ncol = 5)
  
  for(k in 2:5){
    threshold_value <- (k-1)/100
    for(j in 1:20){
      #initial centroids
      initial_centroids <- data[sample(nrow(data), k),]
      
      # EM
      res <- EM_algo(data, k , threshold_value, mu = t(initial_centroids))
      iteration_2[j, k] <- res[[1]]
      x <- cbind(data,category_ionosphere ,center = res[[2]])
      colnames(x)[ncol(data)+1] <- "category"
      x <- combine_cluster(x)
      error_2[j, k] <- sum(error_rateDF(x))
      
      # kmeans
      res <- kmeans_algo(data, k , threshold_value, old_center = initial_centroids)
      iteration_1[j, k] <- res[[1]]
      x <- combine_cluster(cbind(data, category_ionosphere, center = res[[2]]))
      colnames(x)[ncol(data)+1] <- "category"
      error_1[j, k] <- sum(error_rateDF(x))
    }
  }
  
  iteration_2 <- iteration_2[,-1]
  error_2 <- error_2[,-1]
  
  colnames(error_2) <- c("k2","k3","k4","k5")
  error_2 <- data.frame(melt(error_2))
  error_2$label <- "EM"
  colnames(error_2) <- c("Cluster","Error","value","label")
  
  
  colnames(iteration_2) <- c("k2","k3","k4","k5")
  iteration_2 <- data.frame(melt(iteration_2))
  iteration_2$label <- "EM"
  colnames(iteration_2) <- c("Run Number","Iteration","value","label")
  
  iteration_1 <- iteration_1[,-1]
  error_1 <- error_1[,-1]
  
  colnames(error_1) <- c("k2","k3","k4","k5")
  error_1 <- data.frame(melt(error_1))
  error_1$label <- "kmeans"
  colnames(error_1) <- c("Cluster","Error","value","label")

  colnames(iteration_1) <- c("k2","k3","k4","k5")
  iteration_1 <- data.frame(melt(iteration_1))
  iteration_1$label <- "kmeans"
  colnames(iteration_1) <- c("Run Number","Iteration","value","label")
  
  
  # Plot whisker plot 
  ggplot(data = rbind(error_1,error_2), aes(x=Error, y=value)) + geom_boxplot(aes(fill=label))
  ggplot(data = rbind(iteration_1,iteration_2), aes(x=Iteration, y=value)) + geom_boxplot(aes(fill=label))
  
  

rm(list = ls() )
