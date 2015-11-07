## Load libraries -----

library(nnet)
library(caret)

## maybe use caret as recommended here: http://stackoverflow.com/questions/7743768/using-nnet-for-prediction-am-i-doing-it-right
## however the SO post doesn't seem to have separate training and prediction sets, is this an error?

seed.val <- 1234

## Load and format data -----

MLdata <- read.csv(file = "MLdata_with_BS.csv", header = TRUE)
MLdata <- MLdata[-c(1:3),]

inputvars <- MLdata[-c(1:3),4:8]
resp <- MLdata[-c(1:3),3]
dat.in <- data.frame(resp,inputvars)

## separate into training and evaluation sets ----

numTrainObs <- 400
## training set 
datTrain <- dat.in[1:numTrainObs, ]
inputTrain <- datTrain[, 2:6]
respTrain <- datTrain[, 1]
## eval set
datEval <- dat.in[(numTrainObs + 1):dim(dat.in)[1], ]
inputEval <- datEval[, 2:6]

## Actual option, and Black-Scholes, values to compare to evaluation predictions ----
respEval <- datEval[, 1]
BS <- MLdata$Black.Scholes[(numTrainObs + 1):dim(dat.in)[1]]

## 
# nnet function from nnet package
set.seed(seed.val)
mod1 <- nnet(inputvars,resp,data=dat.in,size=10,linout=T)

## predict based on mod1

predictions <- predict(mod1, inputEval) 

predError <- abs(predictions - respEval)
BSError <- abs(BS - respEval)

## plot
plot(BSError, type = 'p', col = 'blue', main = "nnet Neural Net Model vs Black-Scholes: Out of Sample Test", ylab = "Valuation Error")
lines(predError, type = 'p', col = "green")
legend('topright', legend = c("Black-Scholes Error", "ANN Error"), col = c("blue", "green"), pch = c(1,1))


##Have not changed below this line

#neuralnet function from neuralnet package, notice use of only one response
library(neuralnet)
form.in<-as.formula('Y1~X1+X2+X3+X4+X5+X6+X7+X8')
set.seed(seed.val)
mod2<-neuralnet(form.in,data=dat.in,hidden=10)

#mlp function from RSNNS package
library(RSNNS)
set.seed(seed.val)
mod3<-mlp(rand.vars, resp, size=10,linOut=T)
