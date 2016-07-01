#Unit 4
14/20
stevens <- read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)
Train <- subset(stevens, cbind(stevens,spl)$spl==TRUE)
Test <- subset(stevens, cbind(stevens,spl)$spl==FALSE)
baseline_model <-table(Test$Reverse)[2]/(table(Test$Reverse)[1]+table(Test$Reverse)[2])
install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method = "class", minbucket=25)
prp(StevensTree)
PredictCART <- predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
accuracy <- (41+71)/(41+71+22+36)
table(Test$Reverse)[2]/(table(Test$Reverse)[1]+table(Test$Reverse)[2])
library(ROCR)
PredictROC <- predict(StevensTree, newdata = Test)
PredictROC
pred <- prediction(PredictROC[,2],Test$Reverse)
perf <- performance(pred,"tpr","fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)
StevensTree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method = "class", minbucket=5)
prp(StevensTree2)

StevensTree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method = "class", minbucket=100)
prp(StevensTree3)

install.packages("randomForest")
library(randomForest)
Train$Reverse <- as.factor(Train$Reverse)
Test$Reverse <- as.factor(Test$Reverse)
stevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize =25, ntree= 200 )
PredictForest <- predict(stevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
accuracy <- (43+77)/(43+77+16+34)

set.seed(100)
stevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize =25, ntree= 200 )
PredictForest <- predict(stevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
accuracy <- (43+74)/(43+74+19+34)

set.seed(200)
stevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize =25, ntree= 200 )
PredictForest <- predict(stevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
accuracy <- (44+76)/(44+76+17+33)

install.packages("caret")
library(caret)
install.packages("e1071")
library(caret)
numFolds <- trainControl(method = "cv",number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method = "rpart", trControl=numFolds, tuneGrid=cpGrid)
#cp is cross validation parameter
StevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method = "class", cp=0.18)
PredictCV <- predict(StevensTreeCV, newdata=Test, type="class")
table(Test$Reverse,PredictCV)
accuracy <- (59+64)/(59+64+29+18)
prp(StevensTreeCV)

#D2 Hawkeye Story
Claims <- read.csv("ClaimsData.csv")
str(Claims)
table(Claims$bucket2009)/nrow(Claims)
library(caTools)
set.seed(88)
spl <- sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain <- subset(Claims, cbind(Claims,spl)$spl == TRUE)
ClaimsTest <- subset(Claims, cbind(Claims,spl)$spl == FALSE)

mean(ClaimsTrain$age)
sum(ClaimsTrain$diabetes)/length(ClaimsTrain$diabetes)

table(ClaimsTest$bucket2009,ClaimsTest$bucket2008)
#accuracy should be higher than the following for our model
accuracy <- (110138+10721+2774+1539+104)/nrow(ClaimsTest)
PenaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)

#error should be lower than the following for our model
as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

poor_baseline_accuracy <- sum(ClaimsTest$bucket2009==1)/nrow(ClaimsTest)
sum(table(ClaimsTest$bucket2009)*PenaltyMatrix[,1])/nrow(ClaimsTest)

library(rpart)
library(rpart.plot)
ClaimsTree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure +ihd + kidney + osteoporosis + bucket2008 + reimbursement2008,data = ClaimsTrain, method = "class", cp=0.00005)
prp(ClaimsTree)
predictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")
as.matrix(table(ClaimsTest$bucket2009, predictTest))
(114076+16136+118+201+0)/nrow(ClaimsTest)
sum(as.matrix(table(ClaimsTest$bucket2009, predictTest))*PenaltyMatrix)/nrow(ClaimsTest)

ClaimsTree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure +ihd + kidney + osteoporosis + bucket2008 
                    + reimbursement2008,data = ClaimsTrain, method = "class", cp=0.00005, parms = list(loss=PenaltyMatrix))
PredictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, predictTest)
(94310+18942+4692+636+2)/nrow(ClaimsTest)
sum(as.matrix(table(ClaimsTest$bucket2009, predictTest))*PenaltyMatrix)/nrow(ClaimsTest)

#Recitation: Regression trees for housing data
boston <- read.csv("boston.csv")
str(boston)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue",pch=19)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red",pch=19)
summary(boston$NOX)

points(boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],col="green",pch=19)
plot(boston$LON,boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV) 

latlong <- lm(MEDV ~ LAT + LON, data = boston)
summary(latlong)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
latlong$fitted.values
points(boston$LON[latlong$fitted.values>=21.2],boston$LAT[latlong$fitted.values>=21.2],col="blue",pch="$")

library(rpart)
library(rpart.plot)
latlongtree <- rpart(MEDV ~ LAT + LON, data = boston)
prp(latlongtree)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
fittedvalues <- predict(latlongtree)
points(boston$LON[fittedvalues>=21.2],boston$LAT[fittedvalues>=21.2], col = "blue", pch="$")

#simpler model
latlongtree2 <- rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
plot(latlongtree2)
text(latlongtree2)
plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)

library(caTools)
set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio = 0.7)
train <- subset(boston,split == TRUE)
test <- subset(boston,split == FALSE)

#testing linear regression
linreg <- lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
summary(linreg)
linregpred <- predict(linreg, newdata = test)
linregsse <- sum((linregpred - test$MEDV)^2)
linregsse 

#testing regression tress
tree <- rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)
#since many things appear again means that that the relationship is non-linear
treepred <- predict(tree, newdata = test)
treessse <- sum((treepred - test$MEDV)^2)
treessse
#performs worse than linear regression
library(caret)
library(e1071)
install.packages("pROC")
library(pROC)
tr.control <- trainControl(method = "cv", number = 10)
cp.grid <- expand.grid(.cp=  seq(0,0.01,0.001))
train$RAD <- as.numeric(train$RAD)
train$TAX <- as.numeric(train$TAX)
train$TRACT <- as.numeric(train$TRACT)
tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train
            , method = "rpart", trcontrol = tr.control, tuneGrid = cp.grid)

#testing regression tree again with cp 
tree <- rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, cp = 0.001)
prp(tree)
#since many things appear again means that that the relationship is non-linear
treepred <- predict(tree, newdata = test)
treessse <- sum((treepred - test$MEDV)^2)
treessse
##OR
besttree <- tr$finalModel
prp(besttree)
besttreepred <- predict(besttree, newdata = test)
besttreessse <- sum((besttreepred - test$MEDV)^2)
besttreessse

#Assignment 1
#1
gerber <- read.csv("gerber.csv")
str(gerber)
table(gerber$voting)[2]/(table(gerber$voting)[1]+table(gerber$voting)[2])

summary(gerber[gerber$voting ==1,])#neighbours have the highest mean

gerberlm <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = "binomial")
summary(gerberlm)
gerberlmpred <- predict(gerberlm,type = "response")
table(gerber$voting,gerberlmpred > 0.3)
(134513+51966)/(134513+51966+56730+100875)

#the criteria here is quite strict so its unlikely that we will pick up anything
table(gerber$voting,gerberlmpred > 0.5)
(235388)/(235388+108696)

ROCRpredTest <- prediction(gerberlmpred, gerber$voting)

auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc <- performance(ROCRpredTest, "auc")
auc #is only 0.5308641 this is a weak predictive model

#2
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2) #this follows the proportion of voters across different conditions

CARTmodel3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)
plot(CARTmodel3)
text(CARTmodel3)
#install.packages("rattle")
#install.packages("RColorBrewer")
#library("rattle")
#library("RColorBrewer")
#fancyRpartPlot(CARTmodel3)

#3
CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(0.296638-0.34)

#just useless ask not required in the answer

CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

lmmodel1 <- glm(voting ~ control + sex, data=gerber, family = "binomial")
summary(lmmodel1)
#women are less likely to vote, since women have a larger value in the sex variable and 
#that is what is being measured

Possibilities <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(lmmodel1, newdata=Possibilities, type="response")
abs(0.290456 - 0.2908065)

lmmodel2 <- glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(lmmodel2)
#This coefficient is negative, so that means that a value of 1 in this variable decreases 
#the chance of voting. This variable will have variable 1 if the person is a woman and in 
#the control group.
predict(lmmodel2, newdata=Possibilities, type="response")
abs(0.290456 - 0.2904558)

#Assignment 2
#1
letters <- read.csv("letters_ABPR.csv")
letters$isB <- as.factor(letters$letter == "B")
head(letters)

library(caTools)
set.seed(1000)
split <- sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters,split == TRUE)
test <- subset(letters,split == FALSE)
table(train$isB)
accuracy <- 1175/(1175+383)

library(rpart)
library(rpart.plot)
CARTb <- rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)
CARTbpred <- predict(CARTb, newdata = test, type = "class")
table(test$isB,CARTbpred)
#CARTbsse <- sum((treepred - test$MEDV)^2)
accuracy <- (1118+340)/(1118+340+43+57)
accuracy

library(randomForest)
set.seed(1000)
RandomForestb <- randomForest(isB ~ . - letter, data=train, method="class")
RandomForestbpred <- predict(RandomForestb, newdata = test, type = "class")
table(test$isB,RandomForestbpred)
accuracy <- (1165+374)/(1165+374+9+10)
accuracy

#2
letters$letter <- as.factor(letters$letter)
library(caTools)
set.seed(2000)
split <- sample.split(letters$letter, SplitRatio = 0.5)
train <- subset(letters,split == TRUE)
test <- subset(letters,split == FALSE)
table(test$letter)
accuracy <- max(table(test$letter))/(nrow(test))

library(rpart)
library(rpart.plot)
CARTletter <- rpart(letter ~ . - isB, data=train, method="class")
prp(CARTletter)
CARTletterpred <- predict(CARTletter, newdata = test, type = "class")
table(test$letter,CARTletterpred)
#CARTbsse <- sum((treepred - test$MEDV)^2)
accuracy <- (348+318+363+340)/nrow(test)
accuracy

library(randomForest)
set.seed(1000)
RandomForestletter <- randomForest(letter ~ . - isB, data=train, method="class")
RandomForestletterbpred <- predict(RandomForestletter, newdata = test, type = "class")
table(test$letter, RandomForestletterbpred)
accuracy <- (390+380+393+364)/nrow(test)
accuracy

#Assignment3
#1
census <- read.csv("census.csv")
str(census)
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, split == TRUE)
test <- subset(census, split == FALSE)
lm1 <- glm(over50k ~., data = train, family = "binomial")
summary(lm1)

lm1pred <- predict(lm1, newdata = test, type = "response")
table(test$over50k,lm1pred > 0.5)
accuracy <- (9051+1888)/(9051+1888+1190+662)
accuracy

table(test$over50k)
accuracy <- max(table(test$over50k))/nrow(test)
accuracy

library(ROCR)
ROCRpredTest <- prediction(lm1pred, test$over50k)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc <- performance(ROCRpredTest, "auc")
auc

#2
library(rpart)
library(rpart.plot)
CARTsalary <- rpart(over50k ~ ., data=train, method="class")
prp(CARTsalary)
CARTsalarypred <- predict(CARTsalary, newdata = test, type = "class")
table(test$over50k,CARTsalarypred)
#CARTbsse <- sum((treepred - test$MEDV)^2)
accuracy <- (9243+1596)/nrow(test)
accuracy

library(ROCR)
PredictROC_CART <- predict(CARTsalary, newdata = test)
PredictROC_CART
pred_CART <- prediction(PredictROC_CART[,2],test$over50k)
perf_CART <- performance(pred_CART,"tpr","fpr")
plot(perf_CART)
as.numeric(performance(pred_CART, "auc")@y.values)

#3
set.seed(1)
trainSmall <- train[sample(nrow(train), 2000), ]
set.seed(1)
RandomForestsalary <- randomForest(over50k ~ ., data=trainSmall, method="class")
RandomForestsalarypred <- predict(RandomForestsalary, newdata = test, type = "class")
table(test$over50k, RandomForestsalarypred)
accuracy <- (9586+1093)/nrow(test)
accuracy

#assesing the most important for split
vu <- varUsed(RandomForestsalary, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(RandomForestsalary$forest$xlevels[vusorted$ix]))

#assessing the most important for information gain or reduction in impurity mean gini decrease
#importance as average reduction in impurity
varImpPlot(RandomForestsalary)


library(caret)
library("e1071")
KFolds <- trainControl(method = "cv", number = 10)
ComplxityParm <- expand.grid(.cp = seq(0.002,0.1,0.002))
CARTsalary <- train(over50k ~ ., data=train, method = "rpart", trControl = KFolds, tuneGrid = ComplxityParm)
CARTsalary$results
CARTsalary$bestTune

library(rpart)
library(rpart.plot)
CARTsalaryTuned <- rpart(over50k ~ ., data=train, method="class",cp = 0.002)
prp(CARTsalaryTuned)
CARTsalarypredTuned <- predict(CARTsalaryTuned, newdata = test, type = "class")
table(test$over50k,CARTsalarypredTuned)
#CARTbsse <- sum((treepred - test$MEDV)^2)
accuracy <- (9178+1838)/nrow(test)
accuracy
