#Unit 2
#1
#option c
#2
3*0 + 2
3*1 + 2

x <- c(0,1,1)
y <- c(2,2,8)

mean(y) # baseline model
TSS <-sum((mean(y) - y)^2) #total sum of squares
yhat <- as.vector(3*x + 2)
SSE <- sum((yhat - y)^2) #sum of square errors
RSquare <- 1 - (SSE/TSS) #should be closer to 1

#3
wine <- read.csv("wine.csv")
str(wine)
summary(wine)

#4
model1 <- lm(Price ~ AGST, data = wine)
summary(model1)
model1$residuals
SSE <- sum(model1$residuals^2)
SSE

model2 <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
model2$residuals
SSE <- sum(model2$residuals^2)
SSE

model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
model3$residuals
SSE <- sum(model3$residuals^2)
SSE

modelq <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(modelq)
modelq$residuals
SSE <- sum(modelq$residuals^2)
SSE

#5
model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine) #removing non -significant values starting from those with the lowest t values
summary(model4) #all of them have become significant, these is because two or more variables were collinear
model4$residuals
SSE <- sum(model4$residuals^2)
SSE

#6
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine) ##correlation matrix shows that Age and France Prop are highly correlated. The only good correlation is between independent variable and dependent variable\

model5 <- lm(Price ~ AGST + HarvestRain + WinterRain,  data = wine)
summary(model5) #keepin age as intuitively makes more sense as RSquared has dropped
#we stick to model four

cor(wine$HarvestRain, wine$WinterRain)

#7
wineTest <- read.csv("wine_test.csv")
str(wineTest)
predictTest4 <- predict(object = model4,newdata = wineTest) #good model
predictTest5 <- predict(object = model5,newdata = wineTest) #good model
    #we can compare these values with actual values in wineTest to see how close they are

#modelcomparision method 1
sqrt(mean((wineTest$Price-predictTest4)^2, na.rm=T)) #rmsea lower
sqrt(mean((wineTest$Price-predictTest5)^2, na.rm=T)) #rmsea higher

#modelcomparison method 2
SSE4 <- sum((wineTest$Price - predictTest4)^2) #predict test here is yhat
SSE5 <- sum((wineTest$Price - predictTest5)^2) #predict test here is yhat
SST <- sum((wineTest$Price - mean(wine$Price))^2) #base line model
1 - SSE4/SST #RSquare higher
1 - SSE5/SST #RSquare lower

#we want to find a model that does well on training as well as test data, test set can have a lower RSquare then training data but not the other war around

#CASE STUDY - MONEYBALL
baseball <- read.csv("baseball.csv")
str(baseball)

moneyball <- subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD <- moneyball$RS - moneyball$RA
str(moneyball)

plot(moneyball$RD, moneyball$W)

WinsReg <- lm(W ~ RD, data = moneyball)
summary(WinsReg)

#confirming depodesta's claim of winning 95 or more games
yhat <- 80.881375 + 0.105766*moneyball$RD ##gameswon model
summary(subset(data.frame(cbind(yhat,moneyball$RD)), yhat >95 | yhat == 95)$V2) #atleast 134 more runs than allowed are needed
##confirmed

round(subset(as.data.frame(cbind(yhat,moneyball$RD)), V2 == (713-614))) # with this rd a team is likely to win 99 games

str(moneyball)
RunsReg <- lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg) ##counterintuite as BA is correlated

RunsReg <- lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg) 
#batting average is overvalued, and on base percentage is the most important

RunsAll<- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsAll) 

OBP <- 0.311
SLG <- 0.405
yhat <- -804.63 + 2737.77*OBP + 1584.91*SLG ##runscored model

OOBP <- 0.297
OSLG <- 0.370
yhat <- -837.38 + 2913.60 *OOBP + 1514.29*OSLG ##runsallowed model

PlayerName <- c("Eric Chavez", "Jeremy Giambi", "Frank Menechino", "Greg Myers", "Carlos Pena")
OBP <- c(0.338,0.391,0.369,0.313,0.361)
SLG <- as.numeric(c(0.540,0.450,0.374,0.447,0.500))
Salary <- as.numeric(c(1400000,1065000,295000,800000,300000))
PickPlayer <- data.frame(PlayerName,OBP,SLG,Salary) 
str(PickPlayer)
yhatRS <- -804.63 + 2737.77*PickPlayer$OBP + 1584.91*PickPlayer$SLG ##runscored model
PickPlayer$RS <- yhatRS
PickPlayer[order(-PickPlayer$RS),]
# we pick player 1 and 3

teamRank <- c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)


#Recitation
NBA <- read.csv("NBA_train.csv")
str(NBA)
table(NBA$Playoffs, NBA$W)
NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
WinsReg <- lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)

yhat <- 4.100e+01 + 3.259e-02*NBA$PTSdiff
cbind(yhat, NBA$PTSdiff)
summary(subset(data.frame(cbind(yhat, NBA$PTSdiff)),yhat > 42 | yhat == 42)$V2) ##need to score 33 more points that we allow to win 42 or more games

PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)

PointsReg$residuals
SSE <- sum(PointsReg$residuals^2)
SSE

RMSEA <- sqrt(SSE/nrow(NBA))
RMSEA
mean(NBA$PTS)

PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2)

PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsReg3)

PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)

SSE4 <- sum(PointsReg4$residuals^2)
RMSEA4 <- sqrt(SSE4/nrow(NBA))
SSE4
RMSEA4

NBATest <- read.csv("NBA_test.csv")
PointsPrediction <- predict(PointsReg4, newdata = NBATest)
SSE <- sum((PointsPrediction - NBATest$PTS)^2)
SST <- sum((mean(NBA$PTS) - NBATest$PTS)^2)
RSquared <- 1 - SSE/SST
RMESEA <- sqrt(SSE/nrow(NBATest))
RSquared
RMESEA 

#Assignment 1
#1
Climate <- read.csv("climate_change.csv")
head(Climate)
str(Climate)
ClimateTrain <- Climate[Climate[["Year"]] < 2006 | Climate[["Year"]] == 2006,]
ClimateTest <- Climate[!Climate[["Year"]] %in% ClimateTrain[["Year"]],]
yhat <- lm(Temp~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, 
           data = ClimateTrain[colnames(Climate)[!colnames(Climate) %in% c("Year","Month")]])
summary(yhat)

#2
#c
CorrelationMatrix <- cor(ClimateTrain[colnames(ClimateTrain)[!colnames(ClimateTrain) %in% c("Year","Month")]])
ifelse(abs(CorrelationMatrix)>0.7,CorrelationMatrix,0) #Neat Trick

#3
yhat2 <- lm(Temp~ MEI + N2O + TSI + Aerosols, 
           data = ClimateTrain[colnames(ClimateTrain)[!colnames(ClimateTrain) %in% c("Year","Month")]])
summary(yhat2)

#4
yhatb <- step(yhat)

summary(yhatb)

#5
TempPrediction <- predict(yhatb, newdata = ClimateTest)
SSE <- sum((TempPrediction - ClimateTest$Temp)^2)
SST <- sum((mean(ClimateTrain$Temp) - ClimateTest$Temp)^2)
RSquared <- 1 - SSE/SST
RMSEA <- sqrt(mean((TempPrediction - ClimateTest$Temp)^2, na.rm=T))
RSquared
RMSEA 

#Assignment 2
#1
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")


nrow(pisaTrain)

tapply(pisaTrain$readingScore, pisaTrain$male, function(x) mean(x))

summary(pisaTrain)

pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)

#2
#all descriptive

#3
levels(pisaTrain$raceeth) #[1] "American Indian/Alaska Native"  
pisaTrain$raceeth <- relevel(pisaTrain$raceeth,ref = "White")
pisaTest$raceeth <- relevel(pisaTest$raceeth,ref = "White")
levels(pisaTrain$raceeth) #[1] "White"  
levels(pisaTest$raceeth)

yhat <- lm(readingScore ~ ., data = pisaTrain)
summary(yhat)
sum(yhat$residuals^2)
RMESEA <- sqrt(mean(yhat$residuals^2))
RMESEA

#4
predTest <- predict(yhat, newdata = pisaTest)
range(predTest)[1] - range(predTest)[2]

SSE <- sum((predTest - pisaTest$readingScore)^2)
SSE

RMESEA <- sqrt(mean((predTest - pisaTest$readingScore)^2, na.rm=T)) 
RMESEA 

SST <- sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST 

RSquare <- 1 - SSE/SST
RSquare

#Assignment 3
#1
FluTrain <- read.csv("FluTrain.csv")
head(FluTrain)
which.max(FluTrain$ILI)
FluTrain[303,]

plot(FluTrain$ILI)

plot(FluTrain$Queries,log(FluTrain$ILI))

#2
yhat <- lm(log(ILI)~ Queries, data = FluTrain)
summary(yhat)

cor(log(FluTrain$ILI),FluTrain$Queries)^2 
log(1/cor(FluTrain$ILI,FluTrain$Queries)) 
exp(-0.5*cor(FluTrain$ILI,FluTrain$Queries)) 

#3
FluTest <- read.csv("FluTest.csv")

PredTest1 <- exp(predict(yhat , newdata=FluTest)) #this is done to re transform log values to normal values
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

(FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),]$ILI 
- PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")])/FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),]$ILI 

RMSEA1 <- sqrt(mean((PredTest1 - FluTest$ILI)^2, na.rm=T)) 
RMSEA1

#4
install.packages("zoo")
library("zoo")
ILILag2 <- lag(zoo(FluTrain$ILI),-2,na.pad = TRUE)
FluTrain$ILILag2 <- coredata(ILILag2)
summary(FluTrain)
head(FluTrain)

plot(log(FluTrain$ILI),log(FluTrain$ILILag2))
yhat2 <- lm(log(ILI)~ Queries + log(ILILag2), data = FluTrain)
summary(yhat2)

#5
ILILag2 <- lag(zoo(FluTest$ILI),-2,na.pad = TRUE)
FluTest$ILILag2 <- coredata(ILILag2)
summary(FluTest)
head(FluTest)

FluTest$ILILag2[1] <- FluTrain$ILI[length(FluTrain$ILI)-1]
FluTest$ILILag2[2] <-  FluTrain$ILI[length(FluTrain$ILI)]

PredTest2 <- exp(predict(yhat2 , newdata=FluTest))
RMSEA2 <- sqrt(mean((PredTest2 - FluTest$ILI)^2, na.rm=T)) 
RMSEA2