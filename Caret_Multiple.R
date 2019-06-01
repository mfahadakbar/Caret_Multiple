library(MASS)
library(caret)
library(caretEnsemble)

# this codes demonstrates how we can use caret package to apply multiple models , with cross validations
## we will use Boston data set from MASS library

##1 ) Create Partition : test & train

set.seed(1234)

intrain = createDataPartition(y=Boston$crim,times = 1,p = .70,list = F)
training = Boston[intrain,]
test= Boston[-intrain,]


##2) now we want to set our training parameters 
# we will select repeatedcv (cross validation ), number as 5 , cross validation will happen 5 times and  we will set returnResamp to true
# this will save the cross validation results for each cross validation
# now we can use cv or repeatedcv
# cv is simple, it will do cross validation, number arguments will let the algo know how many folds to preform
# so if you have set cv , and nuumber = 10, reg$resample will give you 10 RMSE results
# now some time, you need to 'repeat' the cv process multiple times , in that case you use 'repeatedcv'. you specify number and repeat
# say you have set repeatedcv, number= 10 and repeat = 3, then reg$resample will give you 30 results (10 folds , repeated 3 times)

### two new thing you need to do with ensambles 1) you need to mention savePrediction = 'final',
# second you need to pass Index argument. this argument makes sure that all the resamples in all the models 
# get trained on same samples..  (number argument and times argument should be equal)
trainctr = trainControl(method = 'cv', number = 5 , returnResamp  = 'final', savePredictions = 'final', index = 
                          createResample(training$medv, times = 5))


# now we need a model list
mlist = c('glmnet','xgbTree')

# now instead of train, you will use caretList, it works in the exact same way as training , but only this time method argument will be 
# methodlist
reg = caretList(x = training[,-14],y = training$medv  ,methodList = mlist, trControl = trainctr , metric = 'RMSE')


## this will result in reg , but there will be two models in them  , on lm, other onne glm

reg$lm$results
reg$glmnet$results
reg$xgbTree$results
reg$rf


##check prediction

pred = predict(object = reg,newdata = test)
# we can now check the individual RMSES
RMSE(pred = pred[,1] ,obs = test$medv)
RMSE(pred = pred[,2] ,obs = test$medv)
RMSE(pred = pred[,3] ,obs = test$medv)
RMSE(pred = pred[,4] ,obs = test$medv)




## to check if they will be good to combine , we need to see the correlation between them
modelCor(resamples(reg)) # but for this to work you need to have number of cross validations more than 1, the more you have the better corelation you get
xyplot(resamples(reg)) # this will plot samples RMSE for two models
# to check xgb abd lm 
reg1.2 = reg[c(1,3)]
xyplot(resamples(reg))

### lm and glm are very corelated, however we can see that xgbtree and glmnet are not corelated. we will definitely 
## benifit from the merger of these two models

### if we think that combining the results are better, then we can apply caretEnsamble
# caretEnsamble is just a liner combination of different models, there are more sophisticated models blending
## we will use that separately

regensamble = caretEnsemble(all.models = reg, metric = 'RMSE') # we can also use the trainControl here
summary(regensamble)

pred.ensamble =  predict(object = regensamble,newdata = test)
RMSE(pred = pred.ensamble,obs = test$medv) ### better than individual results




### lets do a more complex blending , that will be done through caretStack  
regsuperensamble =  caretStack(all.models = reg,method ='glmnet')
summary(regsuperensamble)

pred.super.ensamb = predict(object = regsuperensamble,newdata = test)
RMSE(pred = pred.super.ensamb,obs = test$medv)

