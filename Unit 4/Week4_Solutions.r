#Unit 5
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)
##Checks for the package
#check for 174 stop words
length(stopwords("english")) 
#Check for US based preprocessing
Sys.setlocale("LC_ALL", "C")
#Check for stopwords removal
#tm_map(corpus, removeWords, sw) instead of tm_map(corpus, removeWords, stopwords("english"))

tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)

#preprocessing
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus[[1]]$content
corpus <- tm_map(corpus,tolower) #lower
corpus[[1]]$content
corpus <- tm_map(corpus, PlainTextDocument)
corpus[[1]]$content
corpus <- tm_map(corpus,removePunctuation) #punctuation 
corpus[[1]]$content
corpus <- tm_map(corpus,removeWords, c("apple", stopwords("english"))) #stopwords
corpus[[1]]$content
corpus <- tm_map(corpus,stemDocument) #stemming
corpus[[1]]$content

#converting all terms into a matrix
frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1179:1181,405:410])

#finding most frequent terms
findFreqTerms(frequencies, lowfreq = 20) #atleast 20 times
sparse <- removeSparseTerms(frequencies, 0.995) #terms that appear in 0.5% or more of the tweets
sparse

#converting sparse matrix in dataframe
tweetsSparse <- as.data.frame(as.matrix(sparse))
head(tweetsSparse)
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse)) #turning names into appropriate names
tweetsSparse$Negative <- tweets$Negative 

library(caTools)
set.seed(123)
split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse <- subset(tweetsSparse, split == TRUE)
testSparse <- subset(tweetsSparse, split == FALSE)

findFreqTerms(frequencies, lowfreq = 100)

library(rpart)
library(rpart.plot)

tweetCart <- rpart(Negative ~., data = trainSparse, method = "class")
prp(tweetCart)

predictCart <- predict(tweetCart, newdata = testSparse, type = "class")
table(testSparse$Negative, predictCart)
accuracy <- (294+18)/nrow(testSparse)
accuracy
baseline_accuracy <- max(table(testSparse$Negative))/nrow(testSparse)
baseline_accuracy

library(randomForest)
set.seed(123)
tweetRF <- randomForest(Negative ~., data = trainSparse)
predictRF <- predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predictRF)
accuracy <- (293+21)/nrow(testSparse)
accuracy

set.seed(123)
tweetlm <- glm(Negative ~., data = trainSparse, family = "binomial")
predictlm <- predict(tweetlm, newdata = testSparse)
table(testSparse$Negative, predictlm>0.5)
accuracy <- (253+33)/nrow(testSparse)
accuracy

#improving CART
library(caret)
library(e1071)
KFolds <- trainControl(method = "cv", number = 10)
ComplxityParm <- expand.grid(.cp = seq(0.002,0.1,0.002))
CARTtweet <- train(Negative ~., data = trainSparse, method = "rpart", trControl = KFolds, tuneGrid = ComplxityParm)
CARTtweet$results
CARTtweet$bestTune

tweetCarttuned <- rpart(Negative ~., data = trainSparse, method = "class", cp = 0.026) 
#cp stops overfitting and stops the nodes from further splitting after a certain threshold is achieved
#this results in a simpler model that performs much better on a test set
prp(tweetCarttuned)

predictCarttuned <- predict(tweetCarttuned, newdata = testSparse, type = "class")
table(testSparse$Negative, predictCarttuned)
accuracy <- (297+17)/nrow(testSparse)
accuracy

#Recitation - Bringing text analystics to courtroom
emails <- read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)
strwrap(emails$email[1])
strwrap(emails$email[2])
emails$responsive[2]

library(tm)
corpus <- Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])

#preprocessing
corpus <- tm_map(corpus,tolower) #lower
strwrap(corpus[[1]])
corpus <- tm_map(corpus, PlainTextDocument)
strwrap(corpus[[1]])
corpus <- tm_map(corpus,removePunctuation) #punctuation 
strwrap(corpus[[1]])
corpus <- tm_map(corpus,removeWords, c(stopwords("english"))) #stopwords
strwrap(corpus[[1]])
corpus <- tm_map(corpus,stemDocument) #stemming
strwrap(corpus[[1]])

#converting all terms into a matrix
dtm <- DocumentTermMatrix(corpus)
dtm

#finding most frequent terms
findFreqTerms(frequencies, lowfreq = 20) #atleast 20 times
dtm <- removeSparseTerms(dtm, 0.97) #terms that appear in 3% or more of the documents
dtm

#converting into a dataframe
labeledTerms <- as.data.frame(as.matrix(dtm)) #each observation is an email
labeledTerms$responsive <- emails$responsive
str(labeledTerms)

#converting data into training and test
library(caTools)
set.seed(144)
spl <- sample.split(labeledTerms$responsive, 0.7)
train <- subset(labeledTerms, spl == TRUE)
test <- subset(labeledTerms, spl == FALSE)

#modelling 
library(rpart)
library(rpart.plot)

emailCart <- rpart(responsive~., data = train, method = "class")
prp(emailCart)

pred <- predict(emailCart, newdata = test)
pred[1:10,]

pred.prob <- pred[,2]
table(test$responsive, pred.prob>=0.5)
accuracy <- (195+25)/(195+25+17+20)
accuracy
baseline_accuracy <- max(table(test$responsive))/sum(table(test$responsive))
baseline_accuracy

#identifying the cutoffs
library(ROCR)
predROCR <- prediction(pred.prob, test$responsive) #TPR AND FPR
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)

#we want to pick up as many 1s as possible
# we want to make the model more sensitive
table(test$responsive, pred.prob>=0.15)
#slight probability of an email being bad is being picked up
#we increase the number of 1s and reduce 0s but its okay
#we dont want to miss a dodgdy email
#we decrease specificity but there are no issues here
performance(predROCR, "auc")@y.values #our model can distinguish between good and bad 80% of the time

#Assignment 1
#1
wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
corpusAdded <- Corpus(VectorSource(wiki$Added))
strwrap(corpusAdded[[1]])

corpusAdded <- tm_map(corpusAdded,removeWords, c(stopwords("english"))) #stopwords
strwrap(corpusAdded[[1]])
corpusAdded <- tm_map(corpusAdded,stemDocument) #stemming
strwrap(corpusAdded[[1]])

dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded <- removeSparseTerms(dtmAdded, 0.997) #terms that appear in 3% or more of the documents
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
strwrap(corpusRemoved[[1]])
corpusRemoved <- tm_map(corpusRemoved,removeWords, c(stopwords("english"))) #stopwords
strwrap(corpusRemoved[[1]])
corpusRemoved <- tm_map(corpusRemoved,stemDocument) #stemming
strwrap(corpusRemoved[[1]])
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997) #terms that appear in 3% or more of the documents
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))
wordsRemoved

wikiWords <- cbind(wordsAdded,wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
set.seed(123)
library(caTools)
spl <- sample.split(wikiWords$Vandal, 0.7)
train <- subset(wikiWords, spl == TRUE)
test <- subset(wikiWords, spl == FALSE)
baseline_accuracy <- max(table(test$Vandal))/sum(table(test$Vandal))
baseline_accuracy

library(rpart)
library(rpart.plot)
wikiWordsCart <- rpart(Vandal ~., data = train, method = "class")
prp(wikiWordsCart)
pred <- predict(wikiWordsCart, newdata = test)
pred.prob <- pred[,2]
table(test$Vandal,pred.prob >= 0.5)
accuracy <- (618+12)/(618+12+533+0)
accuracy

#2
wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added, fixed = TRUE),1,0)
table(wikiWords2$HTTP)

wikiTrain2 <- subset(wikiWords2, spl == TRUE)
wikiTest2 <- subset(wikiWords2, spl == FALSE)
library(rpart)
library(rpart.plot)
wikiWordsCart2 <- rpart(Vandal ~., data = wikiTrain2, method = "class")
prp(wikiWordsCart2)
pred <- predict(wikiWordsCart2, newdata = wikiTest2)
pred.prob2 <- pred[,2]
table(wikiTest2$Vandal,pred.prob2 >= 0.5)
accuracy <- (609+57)/(609+57+488+9)
accuracy

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain3 <- subset(wikiWords2, spl == TRUE)
wikiTest3 <- subset(wikiWords2, spl == FALSE)
library(rpart)
library(rpart.plot)
wikiWordsCart3 <- rpart(Vandal ~., data = wikiTrain3, method = "class")
prp(wikiWordsCart3)
pred <- predict(wikiWordsCart3, newdata = wikiTest3)
pred.prob3 <- pred[,2]
table(wikiTest3$Vandal,pred.prob3 >= 0.5)
accuracy <- (514+248)/(514+248+297+104)
accuracy

#3
wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin
wikiTrain4 <- subset(wikiWords3, spl == TRUE)
wikiTest4 <- subset(wikiWords3, spl == FALSE)
library(rpart)
library(rpart.plot)
wikiWordsCart4 <- rpart(Vandal ~., data = wikiTrain4, method = "class")
prp(wikiWordsCart4)
pred <- predict(wikiWordsCart4, newdata = wikiTest4)
pred.prob4 <- pred[,2]
table(wikiTest4$Vandal,pred.prob4 >= 0.5)
accuracy <- (595+241)/(595+241+304+23)
accuracy

#Assignment 2
#1
trials <- read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
head(trials)
max(nchar(trials$abstract))

table(nchar(trials$abstract) == 0)

trials$title[which.min(nchar(trials$title))]

#2
corpusTitle <- Corpus(VectorSource(trials$title))
corpusTitle <- tm_map(corpusTitle,tolower) #lower
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusTitle <- tm_map(corpusTitle,removePunctuation)#punctuation
corpusTitle <- tm_map(corpusTitle,removeWords, c(stopwords("english"))) #stopwords
corpusTitle <- tm_map(corpusTitle,stemDocument) #stemming
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmTitle <- removeSparseTerms(dtmTitle, 0.95) #terms that appear in 3% or more of the documents
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
length(colnames(dtmTitle))

corpusAbstract<- Corpus(VectorSource(trials$abstract))
corpusAbstract <- tm_map(corpusAbstract,tolower) #lower
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract,removePunctuation) #punctuation
corpusAbstract <- tm_map(corpusAbstract,removeWords, c(stopwords("english"))) #stopwords
corpusAbstract <- tm_map(corpusAbstract,stemDocument) #stemming
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95) #terms that appear in 3% or more of the documents
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))
length(colnames(dtmAbstract))

which.max(colSums(dtmAbstract))

#3
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))

dtm <- cbind(dtmTitle,dtmAbstract)
dtm$trial <- trials$trial
length(colnames(dtm))

set.seed(144)
library(caTools)
spl <- sample.split(dtm$trial, 0.7)
train <- subset(dtm, spl == TRUE)
test <- subset(dtm, spl == FALSE)
baseline_accuracy <- max(table(test$trial))/sum(table(test$trial))
baseline_accuracy

library(rpart)
library(rpart.plot)
dtmCart <- rpart(trial ~., data = train, method = "class")
prp(dtmCart)
predTrain <- predict(dtmCart, newdata = train)
max(predTrain[,2]) 

pred.probTrain <- predTrain[,2]
table(train$trial, pred.prob >= 0.5)
accuracy <- (631+441)/(631+441+131+99)
sensitivity <- 441/(441+131)
specificity <- 631/(631+99)


predTest <- predict(dtmCart, newdata = test)
pred.probTest <- predTest[,2] 
table(test$trial,pred.probTest >= 0.5)
accuracy <- (261+162)/(261+162+83+52)
sensitivity <- 162/(162+83)
specificity <- 261/(261+52)

library(ROCR)
predicROCR <- prediction(test$trial,pred.probTest >= 0.5)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values 

#Assignment 3
#1
emails <- read.csv("emails.csv",stringsAsFactors = FALSE)
nrow(emails)

table(emails$spam)

emails$text[[1]]

max(nchar(emails$text))

which.min(nchar(emails$text))

#2
library(tm)
corpus<- Corpus(VectorSource(emails$text)) 
corpus <- tm_map(corpus,tolower) #lower
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus,removePunctuation) #punctuation
corpus <- tm_map(corpus,removeWords, c(stopwords("english"))) #stopwords
corpus <- tm_map(corpus,stemDocument) #stemming
dtm <- DocumentTermMatrix(corpus)

spdtm <- removeSparseTerms(dtm, 0.95) #terms that appear in 3% or more of the documents

emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

emailsSparse$spam <- emails$spam

length(colSums(subset(emailsSparse,spam ==0))[colSums(subset(emailsSparse,spam ==0)) >= 5000])

length(colSums(subset(emailsSparse,spam ==1))[colSums(subset(emailsSparse,spam ==1)) >= 1000])

#3
emailsSparse$spam <- as.factor(emails$spam)
library(caTools)
set.seed(123)
spl <- sample.split(emailsSparse$spam,0.7)
train <- subset(emailsSparse, spl == TRUE)
test <- subset(emailsSparse, spl == FALSE)

spamLog <- glm(spam ~., data = train, family = "binomial")
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam~., data=train, method="class")

set.seed(123)
library(randomForest)
spamRF <- randomForest(spam~., data=train)

predLog <- predict(spamLog, type = "response")
predCart <- predict(spamCART)[,2]
predRF <- predict(spamRF, type="prob")[,2]

table(predLog < 0.00001)
table(predLog > 0.99999)
table(predLog >= 0.00001 & predLog <= 0.99999)

summary(spamLog)

prp(spamCART)

table(train$spam, predLog > 0.5)
accuracyLog <- (3052+954)/nrow(train)
accuracyLog

library(ROCR)
predROCR <- prediction(predLog,train$spam)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values 


table(train$spam, predCart> 0.5)
accuracyCART <- (2885+894)/nrow(train)
accuracyCART

library(ROCR)
predROCR <- prediction(predCart,train$spam)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values 

#4
predLog <- predict(spamLog, newdata = test, type = "response")
predCart <- predict(spamCART, newdata = test)[,2]
predRF <- predict(spamRF, newdata = test, type="prob")[,2]
table(test$spam, predLog > 0.5)
accuracyLog <- (1257+376)/nrow(test)
accuracyLog

library(ROCR)
predROCR <- prediction(predLog,test$spam)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values 

table(test$spam, predCart > 0.5)
accuracyCART <- (1228+386)/nrow(test)
accuracyCART

library(ROCR)
predROCR <- prediction(predCart,test$spam)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values 

table(test$spam, predRF > 0.5)
accuracyRF <- (1290+385)/nrow(test)
accuracyRF

library(ROCR)
predROCR <- prediction(predRF,test$spam)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values