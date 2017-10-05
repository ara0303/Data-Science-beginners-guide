setwd("E:/Fall 2017/data analytics/homework1")
irisdata <- read.csv("iris.csv", header = FALSE, stringsAsFactors = F)
head(irisdata)
irisdata$V5 <- gsub("Iris-setosa",1,irisdata$V5) #replaces categorical variables with numbers
irisdata$V5 <- gsub("Iris-versicolor",2,irisdata$V5)
irisdata$V5 <- gsub("Iris-virginica",3,irisdata$V5)
unique(irisdata$V5, incomparables = F)
colnames(irisdata) <- c("slength", "swidth", "plength", "pwidth", "class")
head(irisdata)
plot(irisdata) #2D matrix of data
colors <- c("#00FF00","#FF0000","#0000FF")
colors <-  colors[as.numeric(irisdata$class)]
scatterplot3d(irisdata[,c(1,2,4)], pch = 16, color = colors,  main = "3D Scatter Plot") #3D scatter plot 
visual <- data.matrix(irisdata[,1:4])
filled.contour(visual, color = topo.colors, main = "Visualization of Matrix") #Visualization of feature Matrix
iris1 <- irisdata[grep("1", irisdata$class),]
iris1df <- data.frame(iris1[,-5])
hist(iris1df$slength , col = "red", xlim = c(0,6),ylim = c(0,40), main = "Histogram of Setosa", xlab = "irisdf1", ylab = "Frequency")
hist(iris1df$swidth , col = rgb(0,0,1,0.5), add = T)#histogram for setosa
hist(iris1df$plength , col = "green", add = T)
hist(iris1df$pwidth , col = "yellow", add =T)
legend("topright", c("slength", "swidth", "plength", "pwidth"), fill = c("red",rgb(0,0,1,0.5),"green","yellow"))
iris2 <- irisdata[grep("2", irisdata$class),]
iris2df <- data.frame(iris2[,-5])
hist(iris2df$slength , col = "red", xlim = c(0,6),ylim = c(0,40), main = "Histogram of Versicolour", xlab = "irisdf1", ylab = "Frequency")
hist(iris2df$swidth , col = "blue", add = T)#histogram for versicolour
hist(iris2df$plength , col = rgb(0,1,0,0.5), add = T)
hist(iris2df$pwidth , col = "yellow", add =T)
legend("topleft", c("slength", "swidth", "plength", "pwidth"), fill = c("red","blue",rgb(0,1,0,0.5),"yellow"))
iris3 <- irisdata[grep("3", irisdata$class),]
iris3df <- data.frame(iris3[,-5])
hist(iris3df$slength , col = "red", xlim = c(0,9),ylim = c(0,30), main = "Histogram of Virginica", xlab = "irisdf1", ylab = "Frequency")
hist(iris3df$swidth , col = "blue", add = T)#histogram for virginica
hist(iris3df$plength , col = rgb(0,1,0,0.5), add = T)
hist(iris3df$pwidth , col = "yellow", add =T)
legend("topleft", c("slength", "swidth", "plength", "pwidth"), fill = c("red","blue",rgb(0,1,0,0.5),"yellow"))
boxplot(iris1df, main = "boxplot of Setosa")#boxplot of setosa
boxplot(iris2df, main = "boxplot of versicolor") #boxplot of versicolor
boxplot(iris3df, main = "boxplot of Virginica") #boxplot of virginica
irisdf <- data.frame(irisdata[,-5])
st <- cor(irisdf) #correlation matrix for 4 variables
corrplot(st, main = "Visualization of correlation matrix") #correlation plot visualization for 4 variables
parcoord(irisdf, col = c("red","blue",rgb(0,1,0,0.5),"yellow"), main = " Parallel Coordinates Plot")#parallet coordinates plot
#minkowski#
minkowski <- function(A,B,r){ # minkowski function
  {matres <- matrix(c(0), nrow = 1, ncol = 1)}
  for(i in 1:nrow(A)){
    expt <- 0
    for(k in 1:ncol(A)){
      mul <- abs(A[i,k]-B[1,k])
      pwr <- mul^r
      expt <- expt + pwr
    }
    MKD <- expt ^ (1/r)
    s <- as.matrix(MKD)
    matres <- rbind(s,matres)
    i = i+1
  }
  print(data.matrix(matres))
}

B <- matrix(c(1,2,3,4), nrow = 1, ncol = 4)#Testing the function using an example
minkowski(irisdf,B,1)
#T-Distance#
tstat <- function(A,B){
  expected <- abs(mean(A) - mean(B))
  varr <- var(A) + var(B)
  std <- sqrt(varr)
  tstat<- expected/std
  print(tstat)
}
#Mahalanobis distance#
N <- cov(irisdf)
mahalanobisd <- function(A,B,M){  
  for(i in 1:nrow(A)){
    r = A[i,] - B[1,]
    t <- matrix(r, nrow = 1)
    u <- MASS::ginv(M)
    md <- u %*% t(t) 
    mdt <- as.matrix(t) %*% md
  }
  i = i + 1
  print(mdt)
}

#Applying minkowski and mahalanobis#
#MINKOWSKI
C <- matrix(c(5.0000, 3.5000, 1.4600, 0.2540), nrow = 1, ncol = 4)
x<- 1:150
minkowski(irisdf,C,1) #r=1
plot(1:151, as.vector(minkowski(irisdf,C,1)), xlab = "index")
minkowski(irisdf,C,2) #r=2
plot(1:151, as.vector(minkowski(irisdf,C,2)), xlab = "index")
minkowski(irisdf,C,100) #r=100
plot(1:151, as.vector(minkowski(irisdf,C,100)), xlab = "index")
#Mahalanobis
N <- cov(irisdf)
mahalanobis(irisdf, C, N)
plot(1:150, mahalanobis(irisdf, C, N), xlab = "index")
#Time series generation
mu = c(0,0)
sigmaa = matrix(c(1,.3,.3,1), nrow = 2, byrow = T)
TS <- MASS::mvrnorm(n = 100, mu,sigmaa)
ts.plot(MASS::mvrnorm(n = 100, mu,sigmaa), col = c("red","blue")) #time series plots
legend("topright", c("series1", "series2"), fill = c("red", "blue"))
tsdf <- data.frame(TS)
tstat(tsdf[,1], tsdf[,2]) #tstat distance between time series
cor(tsdf[,1], tsdf[,2]) #correlation between 2 time series
scale(irisdf) #Normalize feature matrix to mean 0 and variance 1

