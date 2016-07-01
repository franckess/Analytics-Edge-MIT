#Unit 3
logit <- -1.5 + (3*1) + (-0.5*5) #just the regression equation or log odd
odds <- exp(logit) #exponential of logit or odds
ProbEqual1 <- 1/(1 + exp(-logit)) #1 / (1 + exp(-logit)) or probability
ProbEqual0 <- 1 - ProbEqual1

quality <- read.csv("quality.csv") 
str(quality)
table(quality$PoorCare)
#mean equivalent to classification problem is 
98/(98+33) #class upon total which we need to beat
install.packages("caTools")
library("caTools")
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75) # we use 0.75 as this is baseline prediction which will be eqaull
#represented in test and training sets
split
qualityTrain <- subset(cbind(quality,split), cbind(quality,split)$split == TRUE)
qualityTest <- subset(cbind(quality,split), cbind(quality,split)$split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog) # we need to look for low AIC
predictTrain <- predict(QualityLog, type = "response")
summary(predictTrain)
tapply(cbind(predictTrain, qualityTrain["PoorCare"])$predictTrain, cbind(predictTrain, qualityTrain["PoorCare"])$PoorCare, function(x) mean(x))

QualityLog2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog2)

table(qualityTrain$PoorCare,ifelse(predictTrain > 0.5,"PoorCare-Pred","GoodCare-Pred")) #threshold 1
sensitivity <- 10/(15+10) #TRUE POSITIVE RATE
specificity <- 70/(70+4) #TRUE NEGATIVE RATE

#stringent criteria for response to become 1
table(qualityTrain$PoorCare,ifelse(predictTrain > 0.75,"PoorCare-Pred","GoodCare-Pred")) #threshold 2 using the actual prob
sensitivity <- 5/(5+20) #TRUE POSITIVE RATE
specificity <- 73/(73+1) #TRUE NEGATIVE RATE
#increasing the threshold reduces the correctly predicted cases or number of 1s resulting in less true positives and low sensitivity
#low sensitivity means no true positives will be captured
# we dont know what threshold to pick???
#matrix 1
20/(20+5)
15/(15+10)
#matrix 2

#ROC 
install.packages("ROCR")
library("ROCR")
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoff.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))
#we want more true positive and less false positives

predictTest <- predict(QualityLog, type = "response",newdata = qualityTest)

ROCRpredTest <- prediction(predictTest, qualityTest$PoorCare)

auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc <- performance(ROCRpredTest, "auc")
auc

#going beyond
ROCRpredTest <- prediction(predictTest, qualityTest$PoorCare)
ROCRperf <- performance(ROCRpredTest, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoff.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))


table(qualityTest$PoorCare,ifelse(predictTest > 0.3,"PoorCare-Pred","GoodCare-Pred")) #threshold 1
sensitivity <- 6/(6+2) #TRUE POSITIVE RATE
specificity <- 19/(19+5) #TRUE NEGATIVE RATE
#This is good model, has high senitivity and high specificity

#Farmingham Heart Study
fm <- read.csv("framingham.csv")
str(fm)
library(caTools)
set.seed(1000)
table(fm$TenYearCHD)
644/(644+3596)
split <- sample.split(Y = fm$TenYearCHD, SplitRatio = 0.65) #50-80% in training set
train <- subset(x = cbind(fm,split), cbind(fm,split)$split == TRUE)
test <- subset(x = cbind(fm,split), cbind(fm,split)$split == FALSE)

fmLog <- glm(TenYearCHD ~., data = train[-which(colnames(train) == "split")], family = binomial)
summary(fmLog)
predictTest <- predict(fmLog, type = "response",newdata = test[-which(colnames(train) == "split")])
table(test$TenYearCHD, predictTest>0.5) #confusion matrix
accuracy <- (1069+11)/(1069+6+187+11) #diaginal values over total
baseline_accuracy <- (1069+6)/(1069+6+187+11) #this is the more dominant class
library(ROCR)
ROCRpredTest <- prediction(predictTest,test$TenYearCHD)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values) #out of sample auc
auc #this shows that model can differentiate between low rish and high risk patients pretty well

sensitivity <- 11/(11+187)
specificity <- 1069/(1069+6)

#Recitation: Predicting election winners
polling <- read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

install.packages("mice")
library("mice")
simple <- polling[,c("Rasmussen","SurveyUSA","DiffCount","PropR")]
summary(simple)
set.seed(144)
imputed <- complete(mice(simple))
summary(imputed)
polling$Rasmussen <- imputed$Rasmussen 
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)

Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)

baseline_model <- table(train$Republican)[2]/(table(train$Republican)[1]+table(train$Republican)[2])
#weak model because it says republican
sign(20)
sign(-10)
sign(0)
?sign
Train$Rasmussen #republican minus democrat hence negative mean democrats
#smartbaseline
table(sign(Train$Rasmussen))[3]/(table(sign(Train$Rasmussen))[1]+table(sign(Train$Rasmussen))[2]+table(sign(Train$Rasmussen))[3]) 

#we can see this a much smarter baseline model
table(Train$Republican, sign(Train$Rasmussen))
cor(Train)
str(Train)
#finding most correlated to dependent variable
ifelse(cor(Train[,c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])>0.9
       ,cor(Train[,c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")]),0)
mod1 <- glm(Republican ~ PropR, data = Train, family = "binomial") 
summary(mod1)$aic
pred1 <- predict(mod1, type = "response")
pred1
table(Train$Republican, pred1>0.5)
#finding least correlated to each other
ifelse(cor(Train[,c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])<0.6
       ,cor(Train[,c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")]),0) 

mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = "binomial") 
summary(mod2)$aic
pred2 <- predict(mod2, type = "response")
table(Train$Republican, pred2>=0.5)

table(Test$Republican, sign(Test$Rasmussen))
TestPrediction <- predict(mod2, newdata = Test, type = "response")
table(Test$Republican, TestPrediction >= 0.5)

subset(Test, TestPrediction >= 0.5 & Republican == 0)

#Assignment 1
#1
songs <- read.csv("songs.csv")
str(songs)
nrow(subset(songs, songs$year>= 2010))
#or
table(songs$year)

nrow(subset(songs, songs$artistname == "Michael Jackson"))

subset(songs, songs$artistname == "Michael Jackson" & songs$Top10 == 1)["songtitle"]

table(songs$timesignature)

which.max(table(songs$timesignature))

subset(songs, songs$tempo == max(songs$tempo))["songtitle"]

#2
SongsTrain <- subset(songs, songs$year <= 2009)
SongsTest <- subset(songs, songs$year >= 2010)
nrow(SongsTrain)

novars <- c()

SongsTrain <- SongsTrain[,-which(colnames(SongsTrain) %in% c("year","songtitle","artistname","songID","artistID"))]  
SongsTest<- SongsTest[,-which(colnames(SongsTest) %in% c("year","songtitle","artistname","songID","artistID"))]  

SongsLog1 <- glm(Top10 ~., data = SongsTrain, family = "binomial")
summary(SongsLog1)

#3
cor(SongsTrain)
ifelse(cor(SongsTrain)>0.7,cor(SongsTrain),"xxxx") #we are always interested in finding those correlate to Y and other Xss

SongsLog2 <- glm(Top10 ~., data = SongsTrain[, -which(colnames(SongsTrain) == "loudness")], family = "binomial")
summary(SongsLog2)

SongsLog3 <- glm(Top10 ~., data = SongsTrain[, -which(colnames(SongsTrain) == "energy")], family = "binomial")
summary(SongsLog3)

#4
predSongsLog3 <- predict(SongsLog3, newdata = SongsTest, type = "response")
table(SongsTest$Top10, predSongsLog3>=0.45)
accuracy <- (309+19)/(309+19+40+5)

table(SongsTest$Top10) #summarizing the response we see that most frequent response is no hit, this mean that if we classify each song as no hit
#we will be right the follwing times
accuracy <- table(SongsTest$Top10)[1]/(table(SongsTest$Top10)[1]+table(SongsTest$Top10)[2])

table(SongsTest$Top10, predSongsLog3>=0.45)
#correctly predicted 19 as hits
#wrongly predicted 5 non hits as hits

sensitivity <- 19/(19+40) #low, doesnt pick up a lot of hits
specificity <- 309/(309+5) #high, picks up a lot of non-hits

#model picks more hits than non-hits to be a hit which means it is quite useful

#Assignment 2
#1
parole <- read.csv("parole.csv")
str(parole)
summary(parole)

table(parole$violator) 

#2
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole)

#3
set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio = 0.7)
train <- subset(cbind(parole,split),cbind(parole,split)$split == "TRUE")
test <- subset(cbind(parole,split),cbind(parole,split)$split == "FALSE")
nrow(parole)
nrow(train)
nrow(test)

#4
paroleLog1 <- glm(violator ~., data = train[,-which(colnames(train) == "split")], family = "binomial")
summary(paroleLog1)
logit <- 1.6119919 #or log odds
exp(logit) #odds 
#model predicts that a parolee who committed multiple offences has 5.01 times higher odds of being a violator than than once which hasnt and is identical

#This is tricky
male <- 1
race <- 1
age <- 50
state2 <- 0
state3 <- 0
state4 <- 0
timeserved <- 3
maxsentence <- 12
multipleoffences <- 0
crime2 <- 1
crime3 <- 0
crime4 <- 0

#log(odds) or log(chances)
logit <- -4.2411574 + 0.3869904*male + 0.8867192*race + -0.0001756*age + 0.4433007*state2 + 0.8349797*state3 + -3.3967878*state4 + 
    -0.1238867*timeserved + 0.0802954*maxsentence + 1.6119919*multipleoffences + 0.6837143*crime2 + -0.2781054*crime3 + -0.0117627*crime4

#just the regression equation
odds <- exp(logit) #exponential of logit or chances

#P(x) = (Chances for)/(Total chances)
ProbEqual1 <- 1/(1 + exp(-logit)) #1 / (1 + exp(-logit))
ProbEqual0 <- 1 - ProbEqual1

#5
logitpred <- predict(paroleLog1, newdata = test, type = "response" )

table(test$violator, logitpred > 0.5)
sensitivity <- 12/(12+11)
specificity <- 167/(167+12)
accuracy <- (167+12)/(167+12+11+12)
baseline_model_test <- table(test$violator)[1]/(table(test$violator)[1] + table(test$violator)[2])
#slighly better than baseline model

#using a lower cutoff increase the model's ability to pick violators
table(test$violator, logitpred > 0.1)
#which person is going to be a violator then??
subset(cbind(test,logitpred>0.1), cbind(test,logitpred>0.1)$logitpred == "TRUE") #these people will be violators
subset(cbind(test,logitpred>0.1), cbind(test,logitpred>0.1)$logitpred == "FALSE") #these should be released

library(ROCR)
ROCRpredTest <- prediction(logitpred,test$violator)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values) #out of sample auc
auc #this shows that model can differentiate between violators and non-violators

#6

#Assignment 1
#1
loans <- read.csv("loans.csv")
str(loans)
summary(loans)
table(loans$not.fully.paid)
1533/(8045+1533)

simple <- loans[,c("log.annual.inc","days.with.cr.line","revol.util","inq.last.6mths","delinq.2yrs","pub.rec")]
summary(simple)
set.seed(144)
imputed <- complete(mice(simple))
summary(imputed)
setdiff(names(loans), "not.fully.paid")
#handy loop for filling in all the column values
for(i in  1:length(colnames(loans)[colnames(loans) %in% colnames(imputed)])){
    loans[,colnames(loans)[colnames(loans) %in% colnames(imputed)]][i] <- imputed [,colnames(imputed )][i]
}
summary(loans)

#2
loans <- read.csv("loans_imputed.csv")
library(caTools)
set.seed(144)
split <- sample.split(Y = loans$not.fully.paid, SplitRatio = 0.70) #50-80% in training set
train <- subset(x = cbind(loans,split), cbind(loans,split)$split == TRUE)
test <- subset(x = cbind(loans,split), cbind(loans,split)$split == FALSE)
loanLog1 <- glm(not.fully.paid ~ ., train[,-which(colnames(train) == "split")], family = "binomial")
summary(loanLog1)

logitA <- -9.317e-03*700
logitB <- -9.317e-03*710
logitA - logitB
9.173e+00*-10
exp(logitA)/exp(logitB)

predloanLog1 <- predict(loanLog1, newdata = test[,-which(colnames(train) == "split")], type = "response")

test$predicted.risk <- predloanLog1
table(test$not.fully.paid, test$predicted.risk > 0.5)
accuracy <- (2400+3)/(2400+3+457+13)
baseline_model <- table(test$not.fully.paid)[1]/(table(test$not.fully.paid)[1]+table(test$not.fully.paid)[2])

library(ROCR)
ROCRpredTest <- prediction(predloanLog1,test$not.fully.paid)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values) #out of sample auc
auc #this shows that model can differentiate between violators and non-violators

#3
bivariate <- glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

predbivariate <- predict(bivariate, newdata = test[,-which(colnames(train) == "split")], type = "response")
max(predbivariate)
table(test$not.fully.paid, predbivariate >0.5) #bad call as this will make it look that all loans will be paid

ROCRpredTest <- prediction(predbivariate,test$not.fully.paid)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values) #out of sample auc
auc #this shows that model can differentiate between violators and non-violators

#4
c <- 10
r <- 0.06
t <- 3
c*exp(r*t) #compounding interest rate calculation

c*exp(r*t) -c

#5
#rt and assuming c as 1 or c*exp(r*t) -c
test$profit <- 1*exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid] <- -1
head(test)
max(10*exp(test$int.rate*3) - 10)

highInterest <- subset(test, int.rate >= 0.15)
summary(highInterest)
mean(highInterest$profit)
table(highInterest$not.fully.paid)[2]/(table(highInterest$not.fully.paid)[1]+table(highInterest$not.fully.paid)[2])

cutoff <- sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, highInterest$predicted.risk <=cutoff)
nrow(selectedLoans)
head(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)