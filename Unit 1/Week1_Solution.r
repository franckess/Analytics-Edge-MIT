#Unit 1
Sys.setlocale("LC_ALL", "C")
#1)
sd(c(5,8,12))
which.min(c(4,1,6))
#2)
8*6
2^16
sqrt(2)
abs(-65)
?sqrt
SquareRoot2 = sqrt(2)
SquareRoot2
HoursYear <- 365*24
ls()
#3)
c(2,3,5,8,13)
Country <- c("Brazil","China","India","Switzerland", "USA")
LifeExpectancy <- c(74,76,65,83,79)
Country[1]
LifeExpectancy[3]
seq(0,100,2)
CountryData <- data.frame(Country,LifeExpectancy)
CountryData$Population <- c(199000,139000,1240000,7977,318000)
Country <- c("Australia","Greece")
LifeExpectancy <- c(66,54)
Population <- c(23050,112344)
NewCountryData <- data.frame(Country,LifeExpectancy, Population)
AllCountryData <- rbind(CountryData,NewCountryData)
#4)
getwd()
setwd("H:/AnalyticsEdge")
list.files()
WHO <- read.csv("WHO.csv")
str(WHO)
summary(WHO)
WHO_Europe <- subset(WHO, Region = "Europe")
write.csv(WHO_Europe,"WHO_EUROPE.csv")
ls()
rm(WHO_Europe)
#5)
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
plot(WHO$GNI,WHO$FertilityRate)
Outliers <- subset(WHO,GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[,c("Country","GNI","FertilityRate")]

mean(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]
which.max(WHO$LiteracyRate)
WHO$Country[44]
#6)
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", main = "Life Expectancy of Countries by region")
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, function(x) mean(x))
tapply(WHO$LiteracyRate, WHO$Region, mean, na.rm = TRUE)

tapply(WHO$ChildMortality, WHO$Region, min, na.rm = TRUE)
which.min(WHO$ChildMortality)
WHO$Country[100]

#reverse string - function
paste(rev(strsplit("anomaly","")[[1]]), collapse = "")
a <- c("anomaly","quantium")
a

rev_string <- function(a){
    for(i in 1:length(a)){
        print(paste(rev(strsplit(a,"")[[i]]), collapse = ""))
    }
}
rev_string(a = c("obaid","umair","nayab"))

#base power - function
#@num is number you are trying to find the x for where x is some power to the base 10
#e.g. 100 == 10^x the function should return 2, here 100 is num and x is what we are trying to find

find.num <- function(num){
    x <- seq(1,100000,0.01)
    for(i in x){
        y <- as.integer(10^i)
        y <- y[!is.na(y)]
        if(y == num){
            print(i)
        } 
    }
}

#Recitation
USDA <- read.csv("USDA.csv")
head(USDA)
str(USDA)
summary(USDA)
which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]
HighSodium <- subset(USDA,Sodium>10000)
nrow(HighSodium)
HighSodium$Description
match("CAVIAR",USDA$Description)
USDA$Sodium[4154]
USDA$Sodium[match("CAVIAR",USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium)
sd(USDA$Sodium, na.rm = TRUE)
mean(USDA$Sodium, na.rm = TRUE) + sd(USDA$Sodium, na.rm = TRUE) #1 degree of standard deviation difference

plot(USDA$Protein, USDA$TotalFat)
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Fat vs Protein", col = "red")
hist(USDA$VitaminC, xlab = "Vitamin C", main = "Histogram of Vitamin C levels")
hist(USDA$VitaminC, xlab = "Vitamin C", main = "Histogram of Vitamin C levels", xlim = c(0,100), breaks = 2000)
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels")
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels", ylab = "Sugar(g)")
USDA$Sodium[1] > mean(USDA$Sodium, na.rm = TRUE)
USDA$Sodium[50] > mean(USDA$Sodium, na.rm = TRUE)
HighSodium <- USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE)
str(HighSodium)
table(HighSodium)

#EDx approach
USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))

#a better approach
names(USDA)
USDA[,c("Protein", "TotalFat","Carbohydrate","Sodium")]
sapply(USDA[,c("Protein", "TotalFat","Carbohydrate","Sodium")]
            ,function(x) as.numeric(x > mean(x, na.rm = TRUE)))
USDA <- cbind.data.frame(USDA,sapply(USDA[,c("Protein", "TotalFat","Carbohydrate","Sodium")]
                             ,function(x) as.numeric(x > mean(x, na.rm = TRUE))))

table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat)
table(USDA$HighSodium, USDA$HighFat, USDA$HighCarbs)

tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm =TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm =TRUE)       

#Assignment 1
#1
mvt <- read.csv("mvtWeek1.csv")

str(mvt)

max(mvt$ID)

min(mvt$Beat)

table(mvt$Arrest)

table(mvt[["LocationDescription"]] == "ALLEY")

#2
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
head(DateConvert)
summary(DateConvert)

mvt$Month <- months(DateConvert)
mvt$Weekday <- weekdays(DateConvert)
mvt$Date <- DateConvert
head(mvt)
which.min(table(mvt$Month))

which.max(table(mvt$Weekday))

which.max(table(subset(mvt, mvt$Arrest == "TRUE")$Month))

#3
hist(mvt$Date, breaks=100)

boxplot(mvt$Date~mvt$Arrest)

table(subset(mvt, mvt$Year == "2001")$Arrest)[2]/length(subset(mvt, mvt$Year == "2001")$Arrest)

table(subset(mvt, mvt$Year == "2007")$Arrest)[2]/length(subset(mvt, mvt$Year == "2007")$Arrest)

table(subset(mvt, mvt$Year == "2012")$Arrest)[2]/length(subset(mvt, mvt$Year == "2012")$Arrest)

#4
head(sort(table(mvt$LocationDescription),decreasing = TRUE),6)

nrow(mvt[mvt[,c("LocationDescription")] %in% as.vector(names(head(sort(table(mvt$LocationDescription),decreasing = TRUE),6)))[-3],])

Temp <- mvt[mvt$LocationDescription %in% as.vector(names(head(sort(table(mvt$LocationDescription),decreasing = TRUE),6)))[-3][1:5],]
sort(table(Temp$LocationDescription),decreasing = TRUE)[1:5]
sort(table(Temp[Temp$Arrest == "TRUE",]$LocationDescription),decreasing = TRUE)[1:5]
which.max(sort(table(Temp[Temp$Arrest == "TRUE",]$LocationDescription),decreasing = TRUE)[1:5]/sort(table(Temp$LocationDescription),decreasing = TRUE)[1:5])

which.max(sort(table(Temp[Temp$LocationDescription ==  "GAS STATION",]$Weekday),decreasing = TRUE)[1:5])

#Assignment 2
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#1
nrow(IBM)

summary(IBM)

mean(IBM$StockPrice)

min(GE$StockPrice)

max(CocaCola$StockPrice)

median(Boeing$StockPrice)

sd(ProcterGamble$StockPrice)

#2
plot(CocaCola$Date, CocaCola$StockPrice, col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue", lty = 2)
abline(v=as.Date(c("2000-03-01")), lwd=2)

abline(v=as.Date(c("1983-03-01")), lwd=2)

#3
plot(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210),lty = 2)
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="blue", ylim=c(0,210),lty = 3)
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="red", ylim=c(0,210),lty = 4)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="green", ylim=c(0,210),lty = 5)
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=3)
abline(v=as.Date(c("1997-11-01")), lwd=3)

#4
tapply(IBM$StockPrice, months(IBM$Date), mean, na.rm=TRUE) > mean(IBM$StockPrice)

which.max(tapply(GE$StockPrice, months(GE$Date), mean, na.rm=TRUE)) 
which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean, na.rm=TRUE)) 

#Assignment 3
#1
CPS <- read.csv("CPSData.csv")
head(CPS)
nrow(CPS)

sort(table(CPS$Industry),decreasing = TRUE)

sort(table(CPS$State))
sort(table(CPS$State),decreasing = TRUE)

(table(CPS$Citizenship)[1] + table(CPS$Citizenship)[2]) / length(CPS$Citizenship)

table(subset(CPS, CPS$Hispanic != 0)$Race)>250

#2
summary(CPS)

table(CPS$Region, is.na(CPS$Married))


table(CPS$State, is.na(CPS$MetroAreaCode))>0

table(CPS$Region, is.na(CPS$MetroAreaCode))    

tapply(is.na(CPS$MetroAreaCode), CPS$State, mean) 

#3
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")
nrow(MetroAreaMap)
nrow(CountryMap)

CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

summary(CPS)

head(sort(table(CPS$MetroArea),decreasing = TRUE),20)

names(head(sort(tapply(CPS$Hispanic, CPS$MetroArea, mean),decreasing = TRUE),5))

sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean),decreasing = TRUE)

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

#4
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)

table(is.na(CPS$Country))

sort(table(CPS$Country),decreasing = TRUE)

table(is.na(subset(CPS, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" & CPS$Country != "United States")$Country))[1]/
table(is.na(subset(CPS, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")$Country))[1]

sort(table(subset(CPS, CPS$Country == "India")$MetroArea),decreasing = TRUE)

sort(table(subset(CPS, CPS$Country == "Brazil")$MetroArea),decreasing = TRUE)

sort(table(subset(CPS, CPS$Country == "Somalia")$MetroArea),decreasing = TRUE)