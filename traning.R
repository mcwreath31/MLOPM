## Pull and format data -----

inputvars <- MLdata[-c(1:3),4:8]
resp <- MLdata[-c(1:3),3]
dat.in <- data.frame(resp,inputvars)

## separate into training and evaluation sets ----

numTrainObs <- 400
datTrain <- dat.in[1:numTrainObs, ]
datEval <- dat.in[(numTrainObs + 1):dim(dat.in)[1], ]

## TODO: update model below to pull correct data ----
#nnet function from nnet package
library(nnet)
set.seed(seed.val)
mod1<-nnet(inputvars,resp,data=dat.in,size=10,linout=T)

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
