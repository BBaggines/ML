#heirarchial clustering

setwd("C:\\Users\\shrad\\Desktop\\AML\\Assignment 1")
file_name_2 <- "ionosphere.csv"
ionosphere_data <- read.csv(file_name_2)

#take 50 random datapoints from data
set.seed(100)
I50 <- ionosphere_data[sample(nrow(ionosphere_data), 50),]
category <- I50[35]
names(category)<- "category"


#removed the category data
# ALso, removing constant column from ionosphere data
I50 <- I50[,-c(35,2)]

#heirarchial clusters
hc_ionosphere <- hclust(d = dist(I50), method = "complete")
plot(hc_ionosphere, main = "Complete linkage")
center <- cutree(hc_ionosphere, 2)


table(category)
table(center)
x <- cbind(center,category)

#Total error
err <- sum(error_rateDF(x))
err    #0.3061224


#PCA on I50
pc_i50 <- prcomp(I50, scale. = TRUE)
pc_i50$sdev
pc_i50$var <- pc_i50$sdev^2
pc_i50$pve <- pc_i50$var/sum(pc_i50$var)
cumsum(pc_i50$pve*100)

plot(pc_i50$pve, xlab = "Principal Component", ylab = "Proportion of Variance",
     type = 'b')
plot(cumsum(pc_i50$pve), xlab = "Cumulative Principal Component", 
     ylab = "Proportion of Variance", type = 'b')


I50 <- as.data.frame(pc_i50$x[,seq(1,9)])

#heirarchial clusters
hc_ionosphere <- hclust(d = dist(I50), method = "complete")
plot(hc_ionosphere, main = "Complete linkage")
center <- cutree(hc_ionosphere, 2)

table(category)
table(center)
x <- cbind(center,category)

#Total error
err <- sum(error_rateDF(x))
err #0.2916667

rm(list = ls())
