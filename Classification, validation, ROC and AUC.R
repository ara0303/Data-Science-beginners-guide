setwd("E:/Fall 2017/data analytics/homework1")
data("iris")
#Question1#
trainIndex <- createDataPartition(iris$Species, p = 0.80, list = F)
traindata <- iris[trainIndex,] #training data set
testdata <- iris[-trainIndex,]#testing data set
model <- NaiveBayes(Species ~ ., data = traindata) #Naive Bayes model
model
a_test <- testdata[,1:4]
b_test <- testdata[,5]
prediction <- predict(model, a_test) #predicting test model using training model
cfm <- confusionMatrix(prediction$class, b_test) #Comparing test data obtained from training model and test data
cfm$overall[1] #prediction accuracy
cfm$table #confusion matrix
#cross validation function#
cfv <- function(x,n){
  sum1<-0
  folds <- createFolds(x$Species, n, list = T) # Creates separate folds of data based on the number n
  str(folds)
  foldsdf<- data.frame(folds)
  irisdf <- data.frame(x)
  matrixx <- matrix(, nrow = 1, ncol = 0)
  for(i in 1:ncol(foldsdf)){
    testdata1 <- x[foldsdf[,i],]
    traindata1 <- x[-foldsdf[,i],]
    model123 <- NaiveBayes(Species ~ ., data = traindata1) 
    prediction1 <- predict(model123,testdata1[,1:4])
    cfm1 <- confusionMatrix(prediction1$class,testdata1[,5])
    print(cfm1$table)
    matrixx <- cbind(cfm1$overall[1],matrixx)
    sum1 <-sum1 + cfm1$overall[1]
    i = i+1
  } 
  print(matrixx)
  print(sum1/5)
}
cfv(iris,5)
#question2#

k<- matrix(c(1,2,3,4), nrow = 1, ncol = 4)
gg <- lda(Species ~., data=iris)
g <- function(x,y){
  irissubset <- iris[grep( x, iris$Species),]
  sett <- irissubset[,1:4] 
  mean1 <- mean(sett[,1])
  mean2 <- mean(sett[,2])
  mean3 <- mean(sett[,3])
  mean4 <- mean(sett[,4])
  meanv <- c(mean1, mean2, mean3, mean4)
  covar <- cov(sett)
  invcovar <- solve(covar)
  a <- -0.5*invcovar
  b <- t(meanv) %*% invcovar
  c <- (-0.5 * t(meanv) %*% invcovar%*% meanv) - (0.5*log(det(covar))) + log(mean(iris$Species == x))
  m <- y %*% a %*% t(y)
  n <- y %*% t(b)
  gfun <- m + n + c

}
g1 <- g("setosa",k)
g2 <- g("versicolor",k)
g3 <- g("virginica",k)
maxclass <- function(x,y,z){
  max(x,y,z)
  print(max(x,y,z))
  if(max(x,y,z) == x)
  print("Setosa")
  else if(max(x,y,z) == y)
  print("Versicolor")
  else if(max(x,y,z) == z)
  print("versicolor")
}
maxclass(g1,g2,g3)
cfv1 <- function(x,n){   #crossfold validation for LDA
  sum1<-0
  folds <- createFolds(x$Species, n, list = T)
  str(folds)
  foldsdf<- data.frame(folds)
  irisdf <- data.frame(x)
  matrixx <- matrix(, nrow = 1, ncol = 0)
  for(i in 1:ncol(foldsdf)){
    testdata1 <- x[foldsdf[,i],]
    traindata1 <- x[-foldsdf[,i],]
    model123 <- lda(Species ~ ., data = traindata1) 
    prediction1 <- predict(model123,testdata1[,1:4])
    cfm1 <- confusionMatrix(prediction1$class,testdata1[,5])
    print(cfm1$table)
    matrixx <- cbind(cfm1$overall[1],matrixx)
    sum1 <-sum1 + cfm1$overall[1]
    i = i+1
  } 
  print(matrixx)
  print(sum1/5)
}
sett1 <- iris[grep("setosa",iris$Species),] 
sett2 <- iris[grep("versicolor",iris$Species),] 
sett3 <- iris[grep("virginica",iris$Species),] 
cfv1(iris,5)

#question3#
iris$Species <- gsub("versicolor", 1, iris$Species)#denote versicolor by 1
iris$Species <- gsub("virginica", -1, iris$Species)#denote virginica by -1
gnew <- function(x,y,n){
  t <- matrix(c(unlist(n)), nrow = 1, ncol = 4)
  gn <- g(x,t) - g(y,t)
  if(gn >= 0)
  opt <- x
  else if(gn < 0)
  opt <- y
}
abc <- gnew(1,-1,iris[84,-5])#checking gnew function for row 84 in data set to determine its class
xyz <- gnew(1, -1, iris[139, -5])
#Q3-2#
sett2$Species <- gsub("versicolor", 1, sett2$Species)#denote versicolor by 1
sett3$Species <- gsub("virginica", -1, sett3$Species)#denote virginica by -1
trainIndex1 <- createDataPartition(sett2$Species, p = 0.80, list = F) #data partition of versicolor
trainIndex2 <- createDataPartition(sett3$Species, p = 0.80, list = F)#data partition of virginica
traindatax <- sett2[trainIndex1,] #training data set from versicolor
traindatay <- sett3[trainIndex2,] #training data set from virginica
traindata1 <- rbind(traindatax,traindatay) #training data set
testdatax <- sett2[-trainIndex1,] #testing data set from versicolor
testdatay <- sett3[-trainIndex2,] #testing data set from virginica
testdata1 <- rbind(testdatax,testdatay) #testing data set for the problem
jebait <- function(x){ #function which automatically implements g1(X) - g2(X) to the given input sample
  matrixx <- matrix(, nrow = 0, ncol = 1)
  for(i in 1:nrow(x)){
    matrixx<- rbind(matrixx,as.matrix(gnew(1,-1,x[i,-5])))
    i=1+1
  }
  print(matrixx)
  }
spd <- jebait(testdata1) 
a_test1 <- testdata1[,1:4]
b_test1 <- testdata1[,5]
b_testdf <- data.matrix(b_test1)
cfm3 <- confusionMatrix(spd, b_testdf)#confusion matrix
cfm3$overall[1]#accuracy
cfm3$table#confusion matrix
cfm3$byClass[1]#sensitivity
cfm3$byClass[2]#specifity
##Question 3.3##
#with 2 features
testdata2 <- testdata1[1:20,c(1,2,5)]
g12 <- function(x,y){
       irissubset <- testdata2[grep( x, testdata2$Species),]
       sett <- irissubset[,1:2] 
       mean1 <- mean(sett[,1])
       mean2 <- mean(sett[,2])
       meanv <- c(mean1, mean2)
       covar <- cov(sett)
       invcovar <- solve(covar)
       a <- -0.5*invcovar
       b <- t(meanv) %*% invcovar
       c <- (-0.5 * t(meanv) %*% invcovar%*% meanv) - (0.5*log(det(covar))) + log(mean(iris$Species == x))
       m <- y %*% a %*% t(y)
       n <- y %*% t(b)
       gfun <- m + n + c
   }
gnew1 <- function(x,y,n,i){
  t <- matrix(c(unlist(n)), nrow = 1, ncol = 2)
  gn <- g12(x,t) - g12(y,t)
  if(gn >= i)
   opt <- x
      else if(gn < i)
    opt <-y
}
jebaited <- function(x,j){ #function which automatically implements g1(X) - g2(X) to the given input sample
  matrixx <- matrix(, nrow = 0, ncol = 1)
  for(i in 1:nrow(x)){
    matrixx<- rbind(matrixx,as.matrix(gnew1(1,-1,x[i,-3],j)))
    i=1+1
  }
  data.matrix(matrixx)
} 


doit <- function(x){
      matrixx <- matrix(, nrow = 0, ncol = 2)
      for(i in -75:75){
       spd <- jebaited(x,i)
       a_test1 <- x[,1:2]
       b_test1 <- x[,3]
       b_testdf <- data.matrix(b_test1)
       cfm3 <-confusionMatrix(spd,b_testdf)
       suar <- cbind(cfm3$byClass[1],cfm3$byClass[2])
       matrixx <- rbind(matrixx, suar)
       i + i+1
      }
      print(matrixx)
}
ggggggg <- doit(testdata2)
plot(1-ggggggg[,2],ggggggg[,1], xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC curve")
lines(1-ggggggg[,2],ggggggg[,1], xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC curve")
abline(a=0,b=1, col = "red")
#AUC calculation
spde <- ggggggg[,1]
height = (spde[-1]+spde[-length(spde)])/2
width = -diff(ggggggg[,2])
AUC <- sum(height*width)
AUC

#all 4 features
gnew2 <- function(x,y,n,i){
  t <- matrix(c(unlist(n)), nrow = 1, ncol = 4)
  gn <- g(x,t) - g(y,t)
  if(gn >= i)
    opt <- x
  else if(gn < i)
    opt <-y
}
jebaited1 <- function(x,j){ #function which automatically implements g1(X) - g2(X) to the given input sample
  matrixx <- matrix(, nrow = 0, ncol = 1)
  for(i in 1:nrow(x)){
    matrixx<- rbind(matrixx,as.matrix(gnew2(1,-1,x[i,-5],j)))
    i=1+1
  }
  data.matrix(matrixx)
} 
doit1 <- function(x){
  matrixx <- matrix(, nrow = 0, ncol = 2)
  for(i in -75:75){
    spd <- jebaited1(x,i)
    a_test1 <- x[,1:4]
    b_test1 <- x[,5]
    b_testdf <- data.matrix(b_test1)
    cfm3 <-confusionMatrix(spd,b_testdf)
    suar <- cbind(cfm3$byClass[1],cfm3$byClass[2])
    matrixx <- rbind(matrixx, suar)
    i + i+1
  }
  print(matrixx)
}
finally <- doit1(testdata1)
plot(1-finally[,2],finally[,1])
lines(1-finally[,2],finally[,1], xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC curve")
abline(a=0,b=1, col = "red")
#AUC calculation
spdf <- finally[,1]
height1 = (spdf[-1]+spdf[-length(spdf)])/2
width1 = -diff(finally[,2])
AUCfull <- sum(height1*width1)
AUCfull
