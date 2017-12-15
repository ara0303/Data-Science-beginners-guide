data("iris")
minkowski <- function(A,B,r,p){ # minkowski function
  {matres <- matrix(c(0), nrow = 0, ncol = 1)}
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
  data.matrix(matres)
}
trainIndex <- createDataPartition(iris$Species, p = 0.80, list = F)
traindata <- iris[trainIndex,] #training data set
testdata <- iris[-trainIndex,]#testing data set
a_train <- traindata[,1:4]
b_train <- traindata[,5]
a_test <- testdata[,1:4]
b_test <- testdata[,5]
B <- matrix(c(0,0,0,0), nrow = 1, ncol = 4)
minkowski(a_train,B,1,1)
knnPrediction <- function(test, train, k, distanceMethod,r){
  predictionResult <- c() 
  distances = matrix(c(0), nrow = 0, ncol = 1)          
  classes = matrix(c(0), nrow = 0, ncol = 1) 
  for(i in 1:nrow(train)){   
    {
      distanceb<-minkowski(train[i,-5],test,r)
    }
    distances <- rbind(distances, distanceb)
    classes <- rbind(classes, as.character(train[i,][[5]]))
    i = i+1
  }
  if(distanceMethod == "majority vote") {{ #Majority Vote method
  matrixCounters = matrix(unique(classes), nrow = 3, ncol = 1)
  matrixCounters <- cbind(matrixCounters,c(0,0,0))
  
  result <- data.frame(classes, distances) 
  result <- result[order(result$distances),]#sorting data
  result <- result[1:k,]#top K neighbors
  
  for(r in c(1:nrow(result))){
    class <- as.character(result[r,'classes'])
    for(ba in 1:nrow(matrixCounters)){
      if(class == matrixCounters[ba,1]){
        matrixCounters[ba,2] = as.numeric(as.character(matrixCounters[ba,2])) + 1
      } else 
        ba = ba + 1
    }
  }
  
  maxIndex = order(matrixCounters[,2])[nrow(matrixCounters)]
  predictionResult <- c(predictionResult,  matrixCounters[maxIndex,1])
  }
  }
  else if(distanceMethod == "avg"){             #Average distance method
    result <- data.frame(classes, distances) 
    result <- result[order(result$distances),]#sorting data
    result <- result[1:k,]#top K neighbors
    seto <- result[grep("setosa", result$classes),]
    versi <- result[grep("versicolor", result$classes),]
    virgi <- result[grep("virginica", result$classes),]
    x <- mean(rowSums(as.matrix(seto[,2]), na.rm = T))
    y <- mean(rowSums(as.matrix(versi[,2]), na.rm = T))
    z <- mean(rowSums(as.matrix(virgi[,2]), na.rm = T))
    matrixCounters = matrix(unique(classes), nrow = 3, ncol = 1)
    matrixCounters <- cbind(matrixCounters,rbind(x,y,z))
    MCdf <- as.data.frame(matrixCounters, row.names = FALSE)
    minIndex = order(MCdf[,2])[1]
    predictionResult <- c(predictionResult, as.character(matrixCounters[minIndex,1]))

  }}
#majority vote check
xaz <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(a_test)){
  xaz <-  rbind(xaz, knnPrediction(a_test[i,], traindata, 3, "majority vote", 1))
  i = i+1
}

#Majority vote
jebaited <- function(test,train,k,r){
xaz <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(test)){
      xaz <- rbind(xaz, knnPrediction(test[i,], train, k, "majority vote", r))
        i = i+1
}
data.matrix(xaz)
}
jebaited2 <- function(test,train,k,r){ #average distance
xyz <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(testdata)){
  xyz <- rbind(xyz, knnPrediction(test[i,], train, k, "avg", r))
  i = i+1
}
 data.matrix(xyz)
}

#1.2Cross Fold Validation#
cfv1 <- function(x,n,k,r, distanceMethod){   #crossfold validation for LDA
  sum1<-0
  folds <- createFolds(x$Species, n, list = T)
  str(folds)
  foldsdf<- data.frame(folds)
  irisdf <- data.frame(x)
  matrixx <- matrix(, nrow = 1, ncol = 0)
  if(distanceMethod == "vote"){
    for(i in 1:ncol(foldsdf)){
      testdata1 <- x[foldsdf[,i],]
      traindata1 <- x[-foldsdf[,i],]
    model123 <- jebaited(testdata1[,-5], traindata1, k , r) 
    cfm1 <- confusionMatrix(model123,testdata1[,5])
    print(cfm1$table)
    matrixx <- cbind(cfm1$overall[1],matrixx)
    sum1 <-sum1 + cfm1$overall[1]
    i = i+1
  } 
  print(matrixx)
  print(sum1/5)
  }
  if(distanceMethod == "avg") {{
    for(i in 1:ncol(foldsdf)){
      testdata1 <- x[foldsdf[,i],]
      traindata1 <- x[-foldsdf[,i],]
    model123 <- jebaited2(testdata1[,-5], traindata1, k , r) 
    cfm1 <- confusionMatrix(model123,testdata1[,5])
    print(cfm1$table)
    matrixx <- cbind(cfm1$overall[1],matrixx)
    sum1 <-sum1 + cfm1$overall[1]
    i = i+1
  }}} 
    print(matrixx)
    print(sum1/5)
  } 
  
#Solutions for Q1.2#
cfv1(iris,5,3,1,"avg")
cfv1(iris,5,3,3,"avg")
cfv1(iris,5,3,5,"avg")
cfv1(iris,5,5,1,"avg")
cfv1(iris,5,5,3,"avg")
cfv1(iris,5,5,5,"avg")
cfv1(iris,5,7,1,"avg")
cfv1(iris,5,7,3,"avg")
cfv1(iris,5,7,5,"avg")
cfv1(iris,5,3,1,"vote")
cfv1(iris,5,3,3,"vote")
cfv1(iris,5,3,5,"vote")
cfv1(iris,5,5,1,"vote")
cfv1(iris,5,5,3,"vote")
cfv1(iris,5,5,5,"vote")
cfv1(iris,5,7,1,"vote")
cfv1(iris,5,7,3,"vote")
cfv1(iris,5,7,5,"vote")

##Question2##
x <- read.csv("abalone.csv", header = T, stringsAsFactors = F)
head(x)
xdf <- as.data.frame(x)
y <- which(xdf[,9] < 9)  #replace all ages less than 9 by -1 #binary classification
ydf <- as.data.frame(y)
for(i in 1:nrow(ydf)){
  xdf[y[i], 9] <- -1
}
z <- which(xdf[,9] > 8)  # replace all ages greater than 8 by 1
zdf <- as.data.frame(z)
for(i in 1:nrow(zdf)){
  xdf[z[i], 9] <- 1
}
head(xdf$Rings)
xdf$Sex <- gsub("M", 1, xdf$Sex)
xdf$Sex <- gsub("I", -1, xdf$Sex)
xdf$Sex <- gsub("F", 0, xdf$Sex)
tindex <- createDataPartition(xdf$Rings, p = 0.80, list = F) #training test splitting
traindata1 <- xdf[tindex,] #training data set
testdata1 <- xdf[-tindex,]#testing data set
a_train1 <- traindata1[,1:8]
b_train1 <- traindata1[,9]
a_test1 <- testdata1[,1:8]
b_test1 <- testdata1[,9]
head(a_test1)
head(b_test1)
ab <- matrix(c(1), nrow = 3342, ncol = 1)
a_trains <- cbind(ab,a_train1)
bc <- matrix(c(1), nrow = nrow(testdata1), ncol = 1)
a_tests <- cbind(bc,a_test1)
mdf <- which(b_train1 == -1)
for(i in 1:length(mdf)){         #normalization
  for(j in 1:ncol(a_trains)){
    a_trains[mdf[i],j] <- as.numeric(a_trains[mdf[i],j]) - (2 * as.numeric(a_trains[mdf[i],j]))
     j = j +1
    }
    i = i+1
  }
a_trains <- data.matrix(data.frame(a_trains))
head(a_trains)
theta <-  .0001
eta <- 0.1
b <- 10 * matrix(c(1), nrow(a_trains),1)
a <- matrix(c(1), 1, ncol(a_trains))

norma <- function(a){
  sum<-0
  for(i in 1:nrow(a)){
    sq <- a[i,] * a[i,]
    sum <- sum + sq
    i <- i+1
  }
  sqr <- sqrt(sum)
}
norma(a)
widrowhuff <- function(a_trains,a,b,eta,theta){
  n <- nrow(a_trains)
  a0 <- 0
  k <- 0 
  stopp <- 0
  kmax <- 10000
  ja <- matrix(c(0), nrow = 0, ncol = 1)
    for(i in 1:n){
      a0 <- a
      a_delta <- (eta) * (as.numeric(a_trains[i,])) * ((b[i,] - (as.numeric(a_trains[i,])*t(a))))
      if(norma(a_delta) < eta)
        a <- t(a0) + t(a_delta)
      else
        a <- (a0) + t(a_delta)
        i <- i + 1
      }
      print(a)
      print(i)
  }
widrowhuff(a_trains,a,b,eta,theta)
Ja <- function(x,a,b){ #norm function in matlab
  sum <- 0 
  Ju <- x %*% t(a) - b
  for(i in 1:nrow(Ju)){
    sq <- Ju[i,] * Ju[i,]
    sum <- sum + sq
    i <- i+1
  }
  sqr <- sqrt(sum)
  print(sqr)
}

