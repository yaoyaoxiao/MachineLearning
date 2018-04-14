library(caret)
library(corrplot)
# set working directory
setwd("/Users/xiaofangyu/ITUCourses/CSC633ML/finalProject")
rm(list=ls())  # clean up predefined objects
# load data
redwine <- read.csv("winequality-red.csv", header=TRUE, sep=";")
str(redwine)

#preprocessing the data set
preprocess_red <- preProcess(redwine[,1:11], c("BoxCox", "center", "scale"))
redwine_new <- data.frame(trans = predict(preprocess_red, redwine))

#checking for outliers using boxplots
boxplot(redwine_new$trans.fixed.acidity,xlab="fixed.acidity", show.names=TRUE)
boxplot(redwine_new$trans.volatile.acidity,xlab="volatile.acidity", show.names=TRUE)
boxplot(redwine_new$trans.citric.acid,xlab="citric.acid", show.names=TRUE)
boxplot(redwine_new$trans.residual.sugar,xlab="residual.sugar", show.names=TRUE)
boxplot(redwine_new$trans.chlorides,xlab="chlorides", show.names=TRUE)
boxplot(redwine_new$trans.free.sulfur.dioxide,xlab="free.sulfur.dioxide", show.names=TRUE)
boxplot(redwine_new$trans.total.sulfur.dioxide,xlab="total.sulfur.dioxide", show.names=TRUE) #no outliers
boxplot(redwine_new$trans.density,xlab="density", show.names=TRUE)
boxplot(redwine_new$trans.PH,xlab="PH", show.names=TRUE)
boxplot(redwine_new$trans.sulphates,xlab="sulphates", show.names=TRUE)
boxplot(redwine_new$trans.alcohol,xlab="alcohol", show.names=TRUE) #no outliers

#remove outliers after scaling, centering and transforming data
redwine_new <- redwine[!abs(redwine_new$trans.fixed.acidity) > 3,]

#display preprocessed data using str
str(redwine_new)


# Find correlations between predictors and quality
corrplot(cor(redwine_new), type = "lower", method = "number")

##(Top 5) Predictors with highest correlations with quality:
#volatile acidity
#citric acid
#residual.sugar
#sulphates
#alcohol

#redCollinear <- lm(quality ~ ., data = redwine)
#vif(redCollinear)
# No VIF > 10 for red wine, no variables highly correlated
corrplot(cor(redwine), method = "number")
# For variables of interest, alcohol and density have very high VIFs
# Create new data frame with variables of interest for red wine and white wine
#redwData <- data.frame(redwine_new$fixed.acidity, redwine_new$citric.acid, redwine_new$residual.sugar, redwine_new$sulphates, redwine_new$alcohol, redwine_new$quality)

# 75/25 train/test split
set.seed(1)
redSplit <- createDataPartition(redwine_new$quality,
                                p = 0.75,
                                list = FALSE)
redTrain <- redwine_new[redSplit,]
redTest  <- redwine_new[-redSplit,]

# check genarated training/testing data
str(redTrain)
str(resTest)


yTrain=redTrain[,12]
rmse=function(X,Y){return( sqrt(sum((X-Y)^2)/length(Y)) )}
lmodel=lm(quality~.,data=redTrain)
wineTrainErr=rmse(yTrain,predict(lmodel,newdata=redTrain[,-12]))
yTest=redTest[,12]
wineTestErr=rmse(yTest,predict(lmodel,newdata=redTest[,-12]))

#apply the trained/tested model to predict the results for the whole data set
whole=yTrain
whole=append(whole,yTest)
wholeErr = rmse(whole, predict(lmodel,newdata = rbind(redTrain[,-12],redTest[,-12])))
cat("Training Error: ", wineTrainErr, "\nTest Error: ", wineTestErr, "\nWhole Error: ", wholeErr, "\n")

# Ridge regression
library("ridge")
xlambda=rep(0, times = 30)
for (i in seq(from = 0, to = 29)){
	exp <- (+3 -4*(i/20))
	xlambda [i+1] <- 10^exp
}
	
wine_train_err_lr = rep(0, times = length(xlambda))
wine_test_err_lr = rep(0, times = length(xlambda))
min_lambda = 10000
for( i in 1:length(xlambda)){
	lrmodel = linearRidge(quality~., data = redTrain, lambda = xlambda [i])
	wine_train_err_lr[i] = rmse(yTrain, predict(lrmodel, newdata = redTrain[,-12]))
	wine_test_err_lr[i] = rmse(yTest, predict(lrmodel, newdata = redTest[,-12]))
	if( i > 1 && wine_test_err_lr[i] < (wine_test_err_lr[i-1]-0.005)){
		min_lambda = xlambda [i]
		index_lambda=i
	}
}

plot(1:length(xlambda),wine_train_err_lr,
  ylim=c(min(wine_train_err_lr, wine_test_err_lr),
  max(wine_train_err_lr, wine_test_err_lr)))
points(1:length(xlambda),wine_test_err_lr, col='red')

cat( index_lambda, "th ", "lambda is optimal: ", min_lambda, "\n")
cat("The Ridge Training Error: ", wine_train_err_lr[xlambda==min_lambda], ", Test Error: ", wine_test_err_lr[xlambda==min_lambda], "\n")

lrmodel2 = linearRidge(quality~., data = redTrain, lambda=min_lambda)
wine_train_err_lr2 = rmse(yTrain, predict(lrmodel2, newdata = redTrain[,-12]))
wine_test_err_lr2 = rmse(yTest, predict(lrmodel2, newdata = redTest[,-12]))
cat("Non-ridge Training Error: ", wineTrainErr, ", Test Error: ", wineTestErr, "\n")
## Non-ridge Training Error:  0.638642 , Test Error:  0.6977601 
cat("Ridge Training Error: ", wine_train_err_lr2, ", Test error: ", wine_test_err_lr2, "\n")
## Ridge Training Error:  0.6482447 , Test error:  0.7092735 	

lrmodel0 = linearRidge(quality~., data = redTrain, lambda=0)
lrmodel0$coef
lrmodel2$coef

#plot(abs(lrmodel0$coef), main="Absolute Coefficients", xlim=c(0,12),ylim=c(0,10))
#points(abs(lrmodel2$coef),col="red")

#use lines to make it clearer
plot(abs(lrmodel0$coef), main="Absolute Coefficients", xlim=c(0,12),ylim=c(0,10), type="b", lty=2, )
lines(abs(lrmodel2$coef),col="red", type = "b", lty=1)







