setwd("C:\\Users\\shrad\\Desktop\\AML\\Assignment 1")
library(reshape2)
library(ggplot2)
rm(list = ls())

filename <- "ionosphere.csv"
data <- read.csv(filename)
category <- data[,35]
data <- data[,-c(1)]
data <- data[,-c(1,34)]


filename <- "ringnorm_data.csv"
data <- read.csv(filename)
category <- data[,1]
data <- data[,-c(1)]

error_1 <- matrix(0,nrow = 20, ncol = 5)
error_2 <-  matrix(0,nrow = 20, ncol = 5)
iteration_1 <- matrix(0, nrow = 20, ncol = 5)
iteration_2 <- matrix(0, nrow = 20, ncol = 5)

for(k in 2:5){
  threshold_value <- 1/10
  print(k)
  for(j in 1:20){
    #initial centroids
    initial_centroids <- data[sample(nrow(data), k),]
    
    #EM
    res <- EM_algo(data, k , threshold_value, update = 'Y',mu = t(initial_centroids))
    iteration_2[j, k] <- res[[1]]
    x <- combine_cluster(cbind(data,category,center = res[[2]]))
    error_2[j, k] <- sum(error_rateDF(x))
    
    # kmeans
    res <- kmeans_algo(data, k , threshold_value, old_center = initial_centroids)
    iteration_1[j, k] <- res[[1]]
    x <- combine_cluster(cbind(data,category,center = res[[2]]))
    error_1[j, k] <- sum(error_rateDF(x))
    
  }
}


iteration_2 <- iteration_2[,-1]
error_2 <- error_2[,-1]

colnames(error_2) <- c("k2","k3","k4","k5")
error_2 <- data.frame(melt(error_2))
error_2$label <- "EM"
colnames(error_2) <- c("Run Number","Error","value","label")

colnames(iteration_2) <- c("k2","k3","k4","k5")
iteration_2 <- melt(iteration_2)
iteration_2$label <- "EM"
colnames(iteration_2) <- c("Run Number","Iteration","value","label")


iteration_1 <- iteration_1[,-1]
error_1 <- error_1[,-1]

colnames(error_1) <- c("k2","k3","k4","k5")
error_1 <- melt(error_1)
error_1$label <- "kmeans"
colnames(error_1) <- c("Run Number","Error","value","label")

colnames(iteration_1) <- c("k2","k3","k4","k5")
iteration_1 <- melt(iteration_1)
iteration_1$label <- "kmeans"
colnames(iteration_1) <- c("Run Number","Iteration","value","label")

# Plot whisker plot 
ggplot(data = rbind(error_1,error_2), aes(x=Error, y=value)) + geom_boxplot(aes(fill=label))
ggplot(data = rbind(iteration_1,iteration_2), aes(x=Iteration, y=value)) + geom_boxplot(aes(fill=label))
