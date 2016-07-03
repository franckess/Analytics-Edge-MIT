#Unit 6
a <- c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
b <- c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)


#function to transform names into a binary matrix based on letters
transNameDist <- function(x,y){
    alphabets <- data.frame(letters,number=seq_along(letters))
    name1 <- x
    name2 <- y
    a <- ifelse(
               is.na(
                    merge(x = alphabets,y = 
                                      subset(alphabets,alphabets$letters %in% strsplit(name1,"")[[1]]),by = c("letters"), all.x = TRUE)$number.y),0,1)
    b <- ifelse(
               is.na(
                    merge(x = alphabets,y = 
                                       subset(alphabets,alphabets$letters %in% strsplit(name2,"")[[1]]),by = c("letters"), all.x = TRUE)$number.y),0,1)
    print ("call x as a data.frame from global")
    return(data.frame(a = a, b = b))
    x <<- data.frame(a = a, b = b)
    
} 
transNameDist(x = "umair", y = "obaid")

#function to calculate euclidian distance
euclidDist <- function(x,y){
    x <- matrix(x)
    y <- matrix(y)
    z <- (x-y)^2
    d <- 0
    for (i in z[,1]){
        d <- d + (i)
    }
    return (sqrt(d))
}
euclidDist(x = x$a,y = x$b)

movies <- read.table("movieLens.txt", header = FALSE, sep = "|", quote = "\"")
str(movies)
colnames(movies) <- c("ID","Title","ReleaseDate", "VideoReleaseDate", "IMDB","Unknown", "Action", "Adventure","Animation", "Childrens", "Comedy", "Crime","Documentary", "Drama", "Fantasy", "FilmNoir","Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War","Western")
str(movies)
movies$ID <- NULL
movies$ReReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL
movies <- unique(movies)
str(movies)

table(movies$Comedy)
table(movies$Western)
nrow(subset(movies, movies$Romance==1 & movies$Drama ==1)) 

distances <- dist(movies[2:20], method = "euclidean")
clusterMovies <- hclust(distances, method = "ward") #ward uses centroids and variances
plot(clusterMovies)
clusterGroups <- cutree(clusterMovies, k = 10) #cutting movie cluster into 10 groups with assignment to each row
tapply(movies$Action,clusterGroups, mean)
tapply(movies$Romance,clusterGroups, mean)
#a better approach
cluster <- data.frame()
for (i in 4:length(colnames(movies))){
    print (colnames(movies)[i])
    cluster <- rbind.data.frame(cluster,tapply(movies[[colnames(movies)[i]]],clusterGroups, mean))  
} 
colnames(cluster) <- unique(clusterGroups) #naming the columns as the clusters
rownames(cluster) <- colnames(movies)[4:21] #naming rownames to those as the lecture

#Now lets look at the recommendation
clusterGroups[as.integer(row.names(subset(movies, Title == "Men in Black (1997)")))]
#Men in black belongs to cluster 2

##Iteration 2
head(subset(movies, clusterGroups == 2))
#these are the recommended movies!!

distances <- dist(movies[2:20], method = "euclidean")
clusterMovies <- hclust(distances, method = "ward") #ward uses centroids and variances
plot(clusterMovies)
clusterGroups <- cutree(clusterMovies, k = 2) #cutting movie cluster into 10 groups with assignment to each row
tapply(movies$Action,clusterGroups, mean)
tapply(movies$Romance,clusterGroups, mean)
#a better approach
cluster <- data.frame()
for (i in 4:length(colnames(movies))){
    print (colnames(movies)[i])
    cluster <- rbind.data.frame(cluster,tapply(movies[[colnames(movies)[i]]],clusterGroups, mean))  
} 
colnames(cluster) <- unique(clusterGroups) #naming the columns as the clusters
rownames(cluster) <- colnames(movies)[4:21] #naming rownames to those as the lecture

#Now lets look at the recommendation
clusterGroups[as.integer(row.names(subset(movies, Title == "Men in Black (1997)")))]
#Men in black belongs to cluster 2

head(subset(movies, clusterGroups == 2))
#these are the recommended movies!!

###Recitation - Seeing the big picture: Segmenting Images to Creat Data (Recitation)
flower <- read.csv("flower.csv", header = FALSE)
str(flower)
flowerMatrix <- as.matrix(flower)
str(flowerMatrix)

#using hierarchical clustering
flowerVector <- as.vector(flowerMatrix)
str(flowerVector)
distance <- dist(flowerVector, method = "euclidian")
clusterIntensity <- hclust(distance, method = "ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k = 3, border = "red")
flowerClusters = cutree(clusterIntensity, k= 3)
flowerClusters

tapply(flowerVector, flowerClusters, function(x) mean (x))
dim(flowerClusters) <- c(50,50)
image(flowerClusters, axes=FALSE)
image(flowerMatrix, axes = FALSE, col = grey(seq(0,1, length = 256)))


healthy <- read.csv("healthy.csv", header = FALSE)
healthyMatrix <- as.matrix(healthy) 
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1, length = 256)))

healthyVector <- as.vector(healthyMatrix)
str(healthyVector)
n = 365636
n*(n-1)/2 #number of permuations R will need to calculate

#using k-means clustering
k <- 5
set.seed <- 1

KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
healthyClusters <- KMC$cluster
KMC$centers[2]

dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col = rainbow(k))

tumor <- read.csv("tumor.csv", header = FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

install.packages("flexclust")
library("flexclust")
#converting our current objects to kcca class
KMC.kcca <- as.kcca(KMC,healthyVector)

tumorClusters <- predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col = rainbow(k))


#Assignment 1
#1.1
DailyKos <- read.csv("dailykos.csv") 
str(DailyKos)
distances <- dist(DailyKos, method = "euclidean")
DailyKosClust <- hclust(distances, method = "ward.D")

#1.2
plot(DailyKosClust) #2,3

#1.3
##7,8

#1.4
DailyKosclusterGroups <- cutree(DailyKosClust, k = 7)
cluster <- data.frame()
for (i in 1:length(colnames(DailyKos))){
    print (colnames(DailyKos)[i])
    cluster <- rbind.data.frame(cluster,tapply(DailyKos[[colnames(DailyKos)[i]]],DailyKosclusterGroups, mean))  
} 
colnames(cluster) <- unique(DailyKosclusterGroups) #naming the columns as the clusters
rownames(cluster) <- colnames(DailyKos) #naming rownames to those as the lecture
head(cluster)

#OR

cluster1 <- (subset(DailyKos, DailyKosclusterGroups ==1))
cluster2 <- (subset(DailyKos, DailyKosclusterGroups ==2))
cluster3 <- (subset(DailyKos, DailyKosclusterGroups ==3))
cluster4 <- (subset(DailyKos, DailyKosclusterGroups ==4))
cluster5 <- (subset(DailyKos, DailyKosclusterGroups ==5))
cluster6 <- (subset(DailyKos, DailyKosclusterGroups ==6))
cluster7 <- (subset(DailyKos, DailyKosclusterGroups ==7))
#e.g nrow(cluster1)
#OR

cluster1to7 <- split(DailyKos,DailyKosclusterGroups)
#e.g. nrow(cluster1to7$`1`)

#1.5
tail(sort(colMeans(cluster1)))

#1.6
tail(sort(colMeans(cluster2)))#ans

tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))#ans

tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))#ans

#2
#2.1
k <- 7
set.seed <- 1000

KMC <- kmeans(DailyKos, centers = k)
DailyKosClusters <- KMC$cluster

cluster1to7 <- split(DailyKos,DailyKosClusters)
nrow(cluster1to7$`3`)

nrow(cluster1to7$`4`)
nrow(cluster1to7$`5`)
nrow(cluster1to7$`6`)
nrow(cluster1to7$`7`)
nrow(cluster1to7$`1`)
nrow(cluster1to7$`2`)

#2.2
C <- c(cluster1,cluster2,cluster3,cluster4,cluster5,cluster6,cluster7)

for(i in 1:length(C)){
    print ("This is the iteration variable")
    print(i)
    print(tail(sort(colMeans(C[i]))))
}
tail(sort(colMeans(cluster1to7$`1`)))
tail(sort(colMeans(cluster1to7$`2`)))#ans
tail(sort(colMeans(cluster1to7$`3`)))#ans
tail(sort(colMeans(cluster1to7$`4`)))
tail(sort(colMeans(cluster1to7$`5`)))
tail(sort(colMeans(cluster1to7$`6`)))
tail(sort(colMeans(cluster1to7$`7`)))
#3,2

#2.3
table(DailyKosclusterGroups, KMC$cluster) #contains at least 50% of the points

#2.4
#5

#2.5
#none

#2.6
#2

#Assignment 2
#1.1
airlines <- read.csv("AirlinesCluster.csv")
summary(airlines)

#1.2
##larger domination

#1.3
library(caret)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc,airlines)
summary(airlinesNorm) #mean centered

#2.1
distances <- dist(airlinesNorm, method = "euclidean")
airlinesNormClust <- hclust(distances, method = "ward.D")
plot(airlinesNormClust)

#2.2
airlinesNormclusterGroups <- cutree(airlinesNormClust, k = 5)
cluster1to7 <- split(airlinesNorm,airlinesNormclusterGroups)
nrow(cluster1to7$`1`)

#2.3
airlinesCluster <- data.frame(cbind(airlines,airlinesNormclusterGroups))
head(airlinesCluster)
sapply(subset(airlinesCluster, airlinesCluster$airlinesNormclusterGroups  == 1), function(x) mean(x)) 
#OR
tail(sort(colMeans(subset(airlinesCluster, airlinesCluster$airlinesNormclusterGroups  == 1))))
#infrequent but loyal

#2.4
tail(sort(colMeans(subset(airlinesCluster, airlinesCluster$airlinesNormclusterGroups  == 2))))
#largest amount of miles

#2.5
tail(sort(colMeans(subset(airlinesCluster, airlinesCluster$airlinesNormclusterGroups  == 3))))
#large amount of miles with non-flight transactions

#2.6
tail(sort(colMeans(subset(airlinesCluster, airlinesCluster$airlinesNormclusterGroups  == 4))))
#Relatively new, non flight transactions

#2.7
tail(sort(colMeans(subset(airlinesCluster, airlinesCluster$airlinesNormclusterGroups  == 5))))
#Relatively new, dont use the flight very often

#3.1
k <- 5
set.seed <- 88

KMC <- kmeans(airlinesNorm, centers = k, iter.max = 1000)
airlinesNormClusters <- KMC$cluster

table(airlinesNormClusters)

#3.2
airlinesClusterKMeans <- data.frame(cbind(airlines,airlinesNormClusters))
tail(sort(colMeans(subset(airlinesClusterKMeans, airlinesClusterKMeans$airlinesNormClusters == 1))))
#no because order would be different

#Assignment 3
#1.1
stocks <- read.csv("StocksCluster.csv")
head(stocks)
nrow(stocks)

#1.2
table(stocks$PositiveDec)[2]/nrow(stocks)

#1.3
CorrelationMatrix <-cor(stocks)
ifelse(abs(CorrelationMatrix)>0.19,CorrelationMatrix,0) 

#1.4
sort(colMeans(stocks))

#2.1
set.seed(144)
library(caTools)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)

set.seed(144)
LogStocks <- glm(PositiveDec ~., data = stocksTrain, family = "binomial")

predictlm <- predict(LogStocks, type = "response")
table(stocksTrain$PositiveDec, predictlm>0.5)
accuracy <- (990+3640)/nrow(stocksTrain)
accuracy

#2.2
set.seed(144)
predictlm <- predict(LogStocks, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, predictlm>0.5)
accuracy <- (417+1553)/nrow(stocksTest)
accuracy

#2.3
max(table(stocks$PositiveDec))/nrow(stocks)

#3.1
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL

#3.2
library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)

mean(normTrain$ReturnJan) 
mean(normTest$ReturnJan) 

#3.3
k <- 3
set.seed <- 144

km <- kmeans(normTrain, centers = k)
normTrainClusters <- km$cluster
normTrainClustersData <- data.frame(cbind(normTrain,normTrainClusters))
table(normTrainClustersData$normTrainClusters)

#3.4
library(flexclust)
km.kcca <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca, newdata=normTest)
table(clusterTest)

#4.1
stocksTrain123 <- data.frame(cbind(stocksTrain,clusterTrain))
stocksTrain1 <- subset(stocksTrain123, stocksTrain123$clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain123, stocksTrain123$clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain123, stocksTrain123$clusterTrain == 3)
mean(stocksTrain1$PositiveDec) #ans
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

stocksTest123 <- data.frame(cbind(stocksTest,clusterTest))
stocksTest1 <- subset(stocksTest123, stocksTest123$clusterTest == 1)
stocksTest2 <- subset(stocksTest123, stocksTest123$clusterTest == 2)
stocksTest3 <- subset(stocksTest123, stocksTest123$clusterTest == 3)

#4.2
set.seed(144)
LogStocks1 <- glm(PositiveDec ~., data = stocksTrain1[,-13], family = "binomial")
summary(LogStocks1)
LogStocks2 <- glm(PositiveDec ~., data = stocksTrain2[,-13], family = "binomial")
summary(LogStocks2)
LogStocks3 <- glm(PositiveDec ~., data = stocksTrain3[,-13], family = "binomial")
summary(LogStocks3)

predictlm1 <- predict(LogStocks1, newdata = stocksTest1[,-13], type = "response")
table(stocksTest1$PositiveDec, predictlm1>0.5)
accuracy1 <- (30+774)/nrow(stocksTest1[,-13])
accuracy1

predictlm2 <- predict(LogStocks2, newdata = stocksTest2[,-13], type = "response")
table(stocksTest2$PositiveDec, predictlm2>0.5)
accuracy2 <- (388+757)/nrow(stocksTest2[,-13])
accuracy2

predictlm3 <- predict(LogStocks3, newdata = stocksTest3[,-13], type = "response")
table(stocksTest3$PositiveDec, predictlm3>0.5)
accuracy3 <- (49+13)/nrow(stocksTest3[,-13])
accuracy3

#4.4
AllPredictions <- c(predictlm1, predictlm2, predictlm3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions>0.5)
accuracyAll <- (467+1544)/(467+1544+353+1110)
accuracyAll
