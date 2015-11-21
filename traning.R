## Load libraries -----

library(nnet)
library(RSNNS)
library(caret)
library(neuralnet)
library(FCNN4R)

## maybe use caret as recommended here: http://stackoverflow.com/questions/7743768/using-nnet-for-prediction-am-i-doing-it-right
## however the SO post doesn't seem to have separate training and prediction sets, is this an error?

## Load and format data -----

MLdata <- read.csv(file = "MLdata_with_BS.csv", header = TRUE)
MLdata <- MLdata[-c(1:3),]

inputFull <- MLdata[-c(1:3),4:8]
optionPrices <- MLdata[-c(1:3),3]
dataFull <- data.frame(optionPrices, inputFull)


## separate into training and evaluation sets ----
numTrainObs <- 400

## Create training set 
datTrain <- dataFull[1:numTrainObs, ]
inputTrain <- datTrain[, 2:6]
respTrain <- datTrain[, 1]
dataTrain <- data.frame(respTrain, inputTrain)


## Crete evaluation set
datEval <- dataFull[(numTrainObs + 1):dim(dataFull)[1], ]
inputEval <- datEval[, 2:6]

## Actual option, and Black-Scholes, values to compare to evaluation predictions ----
respEval <- datEval[, 1] # these are the actual option prices
BS <- MLdata$Black.Scholes[(numTrainObs + 1):dim(dataFull)[1]] #Black-Scholes prices

### Neural Net Models ---------

## nnet function from nnet package
mod1 <- nnet(inputTrain, respTrain,data=datTrain, size=10, linout=T)

## predict based on mod1

predictions <- predict(mod1, inputEval) 

predError <- abs(predictions - respEval)
BSError <- abs(BS - respEval)

## plot
plot(BSError, type = 'p', col = 'blue', main = "nnet Neural Net Model vs Black-Scholes: Out of Sample Test", ylab = "Valuation Error")
lines(predError, type = 'p', col = "green")
legend('topright', legend = c(paste("Black-Scholes Error (Avg. $", round(mean(BSError), 2), ")", sep = ""), paste("ANN Error (Avg. $", round(mean(predError), 2), ")", sep = "")), col = c("blue", "green"), pch = c(1,1))
## result looks good 


### mlp function from RSNNS package -----------


mod3 <- mlp(inputTrain, respTrain, size = 2, linOut=T)

predictionsRSNNS <- predict(mod3, inputEval) 

predErrorRSNNS <- abs(predictionsRSNNS - respEval)

## plot
plot(BSError, type = 'p', col = 'blue', main = "RSNNS Neural Net Model vs Black-Scholes: Out of Sample Test", ylab = "Valuation Error")
lines(predErrorRSNNS, type = 'p', col = "green")
legend('topright', legend = c(paste("Black-Scholes Error (Avg. $", round(mean(BSError), 2), ")", sep = ""), paste("RSNNS Error (Avg. $", round(mean(predErrorRSNNS), 2), ")", sep = "")), col = c("blue", "green"), pch = c(1,1))
## results doesn't look as good as nnet


### neuralnet function from neuralnet package, notice use of only one response ---------

form.in <- as.formula('optionPrices ~ TWTR + Strike + Time + IVlagged + rf')
mod2 <- neuralnet(form.in, data = datTrain, hidden = 7, algorithm = "rprop+")

predictionsNeuNet <- compute(mod2, covariate = inputEval)$net.result

predErrorNeuNet <- abs(predictionsNeuNet - respEval)

## plot
plot(BSError, type = 'p', col = 'blue', main = "neuralnet Neural Net Model vs Black-Scholes: Out of Sample Test", ylab = "Valuation Error")
lines(predErrorNeuNet, type = 'p', col = "green")
legend('topright', legend = c(paste("Black-Scholes Error (Avg. $", round(mean(BSError), 2), ")", sep = ""), paste("neuralnet Error (Avg. $", round(mean(predErrorNeuNet), 2), ")", sep = "")), col = c("blue", "green"), pch = c(1,1))

plot.nn(mod2)

### FCNN4R neural nets

net <- mlp_net(c(5, 10, 1))




### Other (non-neural-net) methods -----
