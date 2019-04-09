#Loading the packages needed
library(dplyr)
library(tm)
library(stringr)
library(tidyverse)
library(lubridate)
library(tidytext)
library(plyr)
library(stringr)
library(textstem)
library(pROC)
library(tinytex)
library(knitr)
library(e1071)
library(caret)

#rmarkdown::render("Markdwon.Rmd")
for (i in c('SnowballC','slam','tm','RWeka','Matrix', 'tidyverse', 'lubridate', 'plyr', 'dplyr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

if (!require("randomForest")) install.packages("randomForest", quiet=TRUE) ; require("randomForest")
library(ggplot2)
if (!require("ROCR")) install.packages("ROCR", quiet=TRUE) ; require("ROCR")

#Loading the database
load("C:/Users/rdoyen/Desktop/posts.Rdata")
postsBASE <- posts
str(posts)
max(posts$feed_created_time)
min(posts$feed_created_time)
posts$like_count <- as.numeric(posts$like_count)
posts$comments_count<- as.numeric(posts$comments_count)
mean(posts$like_count)
mean(posts$comments_count)
boxplot(posts$like_count)
#to avoid a dominant effect of the outlier, extreme outliers are removed. 
#We remove the numer of like above 10000 since we noticed that these posts are linked
#to a particular event (title won)
plot(posts$like_count)
posts <- posts[-which((posts$like_count>10000)),]
posts <- posts[-which((posts$comments_count>900) | (posts$like_count<30)),]
#Removing the post that have 0 comments 
posts <- posts[-which(posts$comments_count<1),]
par(mfrow = c(1,2))
hist(log(posts$like_count))
hist(log(posts$comments_count))
#For the regression, since the data is skewed, we apply a log transformation
par(mfrow = c(1,1))
posts$like_countLOG <- log(posts$like_count)
posts$comments_countLOG <- log(posts$comments_count)
#Changing the type of the variable like and comments count

hist(posts$like_count)
hist(posts$comments_count)

#boxplot
boxplot(posts$like_count)
boxplot(posts$comments_count)


##################################################################################
##################################################################################
###################           VARIABLE CREATION               ####################
##################################################################################
##################################################################################

#Removing 'useless' column
posts$id <- posts$feed_id <- posts$shares_count <- posts$page_name <- posts$extracted_on <- posts$ID <-NULL
#Remong rows with a blank
posts <- posts[-which(posts$feed_message==""),]

#Creation variable weekday and hour
posts$date <- as.Date(posts$feed_created_time)
posts$hour <- hour(posts$feed_created_time)
posts$week_day <- as.factor(weekdays(posts$date))

#Creation variable month
posts$month <- month(posts$feed_created_time)

#Creation of the variable length of the post
posts$length <- nchar(posts$feed_message)
posts <- posts[order(posts$feed_created_time),]


#Creation of the variable postperday to mesure the potential spamming effect
posts$lag <- lag(posts$date)
posts$lag[1] <- as.Date('2010-11-24')
posts$countperday = 0

for (i in 1:nrow(posts)){
  if (posts$date[i] != posts$lag[i]){
    posts$countperday[i] = 1
  } else {
    posts$countperday[i] = posts$countperday[i-1]+1
  }
}
posts$lag <- NULL

#Creation of the variable number of hastag
posts$numberhas <- str_count(posts$feed_message,"#")
#Creation of the variable number of ! 
posts$numberexcl <- str_count(posts$feed_message,"!")
#Creation of the variable number of ?
posts$numberinter <- str_count(posts$feed_message,"\\?")
#Creation of the variable number of .
posts$numberpoint <- str_count(posts$feed_message,"\\.")
#Creation of the variable number of capital letters
posts$numbercapitalletters <- data.matrix(ldply(str_match_all(posts$feed_message,"[A-Z]"),length))

#Creation of the dummy variable link.
posts$link <- ifelse(grepl("http[^[:space:]]*", posts$feed_message)|grepl("www[^[:space:]]*",posts$feed_message)|grepl("[^[:space:]]*com",posts$feed_message)
                     |grepl("[^[:space:]]*be",posts$feed_message),1,0)

#Creation of the variable opponent
#We noticed that the club used all the time the same form of hashtag during the match. #CluXXX or #XXXClu
posts$hashtags <- sapply(str_extract_all(posts$feed_message, "#[cC]lu[^[:space:]]{3}"), paste, collapse=", ")
posts$hashtags2 <- sapply(str_extract_all(posts$feed_message, "#[^[:space:]]{3}[cC]lu"), paste, collapse=", ")
#Removing the hashtag
posts$hashtags <- substr(posts$hashtags,2,7)
posts$hashtags2 <- substr(posts$hashtags2,2,7)
#Determining the opponent
posts$hashtags <- tolower(paste(posts$hashtags,posts$hashtags2))
posts$opponent <- ifelse(posts$hashtags2!="",posts$hashtags2,
                         ifelse(posts$hashtags=="","",posts$hashtags))
posts$hashtags2 <- posts$hashtags <- NULL
posts$opponent <- tolower(posts$opponent)
posts$opponent <- ifelse(startsWith(posts$opponent,'clu'),substr(posts$opponent,4,6),substr(posts$opponent,1,3))

count <- count(posts$opponent)
count <- count[order(count$freq,decreasing = TRUE),]

#51 teams faced during this period of time -> Created a dummy variable for any of them
#Assign dummy for the team faced for the one that have a least 20 posts
posts$d_Standard[posts$opponent=='sta']<-1
posts$d_Standard[posts$opponent!='sta']<-0

posts$d_Kortrijk[posts$opponent=='kor']<-1
posts$d_Kortrijk[posts$opponent!='kor']<-0

posts$d_Charleroi[posts$opponent=='cha']<-1
posts$d_Charleroi[posts$opponent!='cha']<-0

posts$d_Aalborg[posts$opponent=='aal']<-1
posts$d_Aalborg[posts$opponent!='aal']<-0

posts$d_Anderlecht[posts$opponent=='and']<-1
posts$d_Anderlecht[posts$opponent!='and']<-0

posts$d_CercleBrugge[posts$opponent=='cer']<-1
posts$d_CercleBrugge[posts$opponent!='cer']<-0

posts$d_Mechelen[posts$opponent=='kvm']<-1
posts$d_Mechelen[posts$opponent!='kvm']<-0

posts$d_Zulte[posts$opponent=='zwa']<-1
posts$d_Zulte[posts$opponent!='zwa']<-0

posts$d_Gantoise[posts$opponent=='gnt' | posts$opponent=='gen']<-1
posts$d_Gantoise[posts$opponent!='gnt' & posts$opponent !='gen']<-0

posts$d_Genk[posts$opponent=='gnk']<-1
posts$d_Genk[posts$opponent!='gnk']<-0

posts$d_Panathinaikos[posts$opponent=='pan']<-1
posts$d_Panathinaikos[posts$opponent!='pan']<-0

posts$d_Lokeren[posts$opponent=='lok']<-1
posts$d_Lokeren[posts$opponent!='lok']<-0

posts$d_Manchester[posts$opponent=='man']<-1
posts$d_Manchester[posts$opponent!='man']<-0

posts$d_Besiktas[posts$opponent=='bes']<-1
posts$d_Besiktas[posts$opponent!='bes']<-0

posts$d_Dnipro[posts$opponent=='dni']<-1
posts$d_Dnipro[posts$opponent!='dni']<-0

posts$d_Lierse[posts$opponent=='lie']<-1
posts$d_Lierse[posts$opponent!='lie']<-0

posts$d_Helsinki[posts$opponent=='hel']<-1
posts$d_Helsinki[posts$opponent!='hel']<-0

posts$d_Ostende[posts$opponent=='kvo']<-1
posts$d_Ostende[posts$opponent!='kvo']<-0

posts$d_Torino[posts$opponent=='tor']<-1
posts$d_Torino[posts$opponent!='tor']<-0

posts$d_Copenhague[posts$opponent=='kop']<-1
posts$d_Copenhague[posts$opponent!='kop']<-0

posts$d_Beveren[posts$opponent=='wbe']<-1
posts$d_Beveren[posts$opponent!='wbe']<-0

posts$d_Westerlo[posts$opponent=='wes']<-1
posts$d_Westerlo[posts$opponent!='wes']<-0

posts$d_Mouscron[posts$opponent=='moe']<-1
posts$d_Mouscron[posts$opponent!='moe']<-0

posts$d_Grasshopper[posts$opponent=='gra']<-1
posts$d_Grasshopper[posts$opponent!='gra']<-0

posts$d_SaintTrond[posts$opponent=='stv']<-1
posts$d_SaintTrond[posts$opponent!='stv']<-0

posts$d_Heist[posts$opponent=='hei']<-1
posts$d_Heist[posts$opponent!='hei']<-0

posts$d_Leuven[posts$opponent=='ohl']<-1
posts$d_Leuven[posts$opponent!='ohl']<-0

posts$opponent <- NULL

#creation of the dummy matchday. We noticed that the club most of the time used the term matchday in it's post when it's a day of match. 
posts$matchday <- sapply(str_extract_all(posts$feed_message, "[mM]atch[dD]ay"), paste, collapse=", ")
posts$matchday <- ifelse(posts$matchday != "",1,0)
#Creation of a list containing the date of the match. 
ListeMatchDay <- posts$date[posts$matchday==1]
#Creation of the Matchday variable, is date of the post is in the list then = 1
posts$d_Matchday <- ifelse(posts$date %in% ListeMatchDay,1,0)

#Creation of the variable goal
posts$d_Goal <- ifelse(grepl("goal[^[:space:]]",posts$feed_message, ignore.case = TRUE),1,0)

#Extract the hastag used in order to plot them, to discoer which are the most used ones. 
#Using the pattern  #[[:alpha:]]+ we extract the hashtag from the posts and we are creating a dataframe that we
#use to plot
extract.hashes <- function(vec){
  vec <- posts$feed_message
  hash.pattern <- "#[[:alpha:]]+"
  have.hash <- grepl(x = vec, pattern = "#[[:alpha:]]+")
  
  hash.matches <- gregexpr(pattern = hash.pattern,
                           text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  posts$feed_message[have.hash]
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}

hashtag <- extract.hashes(posts$feed_message)
#We only what to see the hashtag that are used more than 50 times.
hashtagused <- subset(hashtag, hashtag$freq>50)
library(ggplot2)
plot <- ggplot(data=data.frame(hashtagused), aes(x=reorder(tag,freq), y=freq))+geom_bar(stat='identity')
plot+coord_flip()

#From this graph, we can understand which are the most used hasthag and integrated them in our model
#Except the hastags from the match, the most frequent hashtags are COYB, wearebruges, uel, ucl, roadtothefinal
# thanksfans, cofidiscup (>50 utilization)

posts$d_COYB <- ifelse(grepl("#COYB",posts$feed_message, ignore.case = TRUE),1,0)

posts$d_wearebruges <- ifelse(grepl("#wearebruges",posts$feed_message, ignore.case = TRUE),1,0)

posts$d_uel <- ifelse(grepl("#uel",posts$feed_message, ignore.case = TRUE),1,0)

posts$d_roadtothefinal <- ifelse(grepl("#roadtothefinal",posts$feed_message, ignore.case = TRUE),1,0)

posts$d_ucl <- ifelse(grepl("#ucl",posts$feed_message, ignore.case = TRUE),1,0)

posts$d_thanksfans <- ifelse(grepl("#thanksfans",posts$feed_message, ignore.case = TRUE),1,0)

posts$d_cofidiscup <- ifelse(grepl("#cofidiscup",posts$feed_message, ignore.case = TRUE),1,0)


#What are the most used words?

#delete non-recognizable characters
PostsText <- sapply(posts[,1],function(x) iconv(x, 'utf8', 'ascii',""))

#removing the link from the posts, either start by http, www or finish by .be or .com
PostsText <-gsub("http[^[:space:]]*", "", PostsText)
PostsText <- gsub("www[^[:space:]]*","",PostsText)
PostsText <- gsub("[^[:space:]]*com","",PostsText)
PostsText <- gsub("[^[:space:]]*be","",PostsText)

#creating the corpus
postCorpus <- VCorpus(VectorSource(PostsText))

#Removing the # since we already created the appropriated variables
gsubtransfo <- content_transformer(function(x,from, to) gsub(from, to, x))
postCorpus <- tm_map(postCorpus, gsubtransfo, "#\\w+",  "")

as.character(postCorpus[[1996]])
#gsubtransfointernet <- content_transformer(function(x,from,to) gsub(from,to,x))
#post <- tm_map(post, gsubtransfointernet, c("www.\\+"),)
postCorpus <- tm_map(postCorpus, removePunctuation)
postCorpus <- tm_map(postCorpus, removeNumbers)
postCorpus <- tm_map(postCorpus, stripWhitespace)
postCorpus <- tm_map(postCorpus,content_transformer(tolower))

as.character(postCorpus[[1996]])


#Removing stopwords + some common words that according to us don't make much sense in the analysis
forremoval <- stopwords('dutch')
forremoval <- append(forremoval, c('we', 'HT', 'FT', 'KV', 'club', 'brugge', 'bruge', 'eerste', 'tweede', 'onze', 'jullie','jpg','via','morgen','vandaag','komt', 'helft'))
#Add the word geen, niet, nee
postCorpus <- tm_map(postCorpus,removeWords,c(forremoval[!forremoval %in% c("nee","niet","geen")]))

#Stemming
postCorpus <- tm_map(postCorpus, stemDocument)


#creating the dtm

dtm <- DocumentTermMatrix(postCorpus, control = list( wordlengths=c(2,Inf),
                                                      weighting =function(x) weightTf(x)))

#We put the sparse coefficient = to 99,5%. Then, we remove the terms that are more sparse than 0.995
dtmCommentsDense <- removeSparseTerms(dtm,0.995)

ncol(dtmCommentsDense)


install.packages('RWeka')
library(RWeka)
#We want to consider unigram, bigram and trigram
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
#We want to remove the words that are shorter than 2 letters
dtm <- DocumentTermMatrix(postCorpus, control = list(wordlengths=c(2,Inf), tokenizers = BigramTokenizer,
                                                     weighting =function(x) weightTf(x)))

dtmCommentsDense <- removeSparseTerms(dtm,0.995)
ncol(dtmCommentsDense)

dtmatrix <- as.matrix(dtmCommentsDense)
sumcol <- colSums(dtmatrix)
class(sumcol)
#With the following line, we will know the most used words. For those we can create a dummy variable
sort(sumcol)
library(dplyr)
#We want to plot the words, to have a graphical representation of the most used words
word <- data.frame(sumcol)
colnames(word)[1] <- "freq"
word$freq <- as.numeric(word$freq)
word$word <- rownames(word)
class(word)
str(word)
plot2 <- ggplot(data=data.frame(word[word$freq>100,]), aes(x=reorder(word,freq), y=freq))+geom_bar(stat='identity')
plot2+coord_flip()


#For the most commonly word used, creation of dummies variables
posts$d_wissel <- ifelse(grepl("wissel",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_vazquez <- ifelse(grepl("vazquez",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_refaelov <- ifelse(grepl("refaelov",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_sutter <- ifelse(grepl("sutter",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_goal <- ifelse(grepl("goal",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_wedstrijd <- ifelse(grepl("wedstrijd",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_ryan <- ifelse(grepl("ryan",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_Preudhomme <- ifelse(grepl("Preud'homme",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_seizoen <- ifelse(grepl("seizoen",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_nieuw <- ifelse(grepl("nieuw",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_meunier <- ifelse(grepl("meunier",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_speler <- ifelse(grepl("speler",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_simon <- ifelse(grepl("simon",posts$feed_message, ignore.case = TRUE),1,0)
posts$d_opstel <- ifelse(grepl("opstel",posts$feed_message, ignore.case = TRUE),1,0)

#Sentiment analasys, dictionnary-based lookup

dictionary <- read.csv("C:/Users/rdoyen/Desktop/SentimentLexicon.csv", header=T, sep=',')
head(dictionary)

#We we will use for, the analysis , the polarity of each words. We will also consider the negative words that can
#change the meaning of a sentence (nee, niet, geen)


posts$sentimentScore <- 0

for (i in 1:length(PostsText)){
  text <-tolower(PostsText)
  split <- strsplit(text[i],split=" ")[[1]]
  m <- match(split, dictionary$ï..form)
  present <- !is.na(m)
  presentMinusone <- lead(present,default = FALSE)
  wordpolarity <- dictionary$polarity[m[present]]
  negators <- c("nee","niet","geen")
  wordnegator <-  split[presentMinusone] %in% negators
  wordpolarity <- ifelse(wordnegator == TRUE, wordpolarity*(-1), wordpolarity)
  posts$sentimentScore[i] <- mean(wordpolarity, na.rm=TRUE)
}

posts$sentimentScore <- ifelse(is.na(posts$sentimentScore),0,posts$sentimentScore)

head(posts$sentimentScore)
mean(posts$sentimentScore)
sd(posts$sentimentScore)
length(posts$sentimentScore)


postsREG <- posts
postsREG$like_count <- postsREG$comments_count <- NULL
postsCLASS <- posts
postsCLASS$like_countLOG <- postsCLASS$comments_countLOG <- NULL


##################################################################################
##################################################################################
###################                 REGRESSION                ####################
##################################################################################
##################################################################################

#Variable selection
#To do the variable selection we will do the Best forward Selection based on the book "An Introduction to Statistical
#Learning"

########################################################
############# Variable selection for likes #############
########################################################

postsLREG <- postsREG
postsLREG$comments_count <- postsLREG$feed_message <-  postsLREG$feed_created_time <- postsLREG$date<- postsLREG$lag <- postsLREG$comments_countLOG <- NULL
dim(postsLREG)
sum(is.na(postsLREG))
install.packages('leaps')
library(leaps)

#We used the Forward stepwise Selection with the regsubset function
regfit.full=regsubsets(like_countLOG~.,postsLREG, really.big=TRUE,method='forward', nvmax=61)
reg.summary=summary(regfit.full)
options(max.print=999999)
par(mfrow=c(1,1))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS',type='l')
plot(reg.summary$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')

#From the graph we decided to keep 28 variables. indeed, the model that have the
#lowest BIC is the 28-variables model. 
#This methodlogy is used for the three other models
plot(regfit.full, scale='bic', height=150)
?plot
VariableLIKE1 <- names(coef(regfit.full, 28))
VariableLIKE1 <- VariableLIKE1[2:29]

postsLIKE1 <- posts[,c('like_countLOG',VariableLIKE1)]


###########################################################
############# Variable selection for comments #############
###########################################################

postsCREG <- postsREG
postsCREG$like_count <- postsCREG$feed_message <-  postsCREG$feed_created_time <- postsCREG$lag <- postsCREG$date <- postsCREG$like_countLOG <- NULL
dim(postsCREG)
sum(is.na(postsCREG))

#We used the Forward stepwise Selection with the regsubset function
regfit.full2=regsubsets(comments_countLOG~.,postsCREG, really.big=TRUE, method='forward', nvmax=61)
reg.summary2=summary(regfit.full2)
options(max.print=999999)
par(mfrow=c(1,1))
plot(reg.summary2$rss, xlab='Number of Variables', ylab='RSS',type='l')
plot(reg.summary2$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')

#From the graph we decided to keep 24 variables
plot(regfit.full2, scale='bic')
VariableCOMMENT1 <- names(coef(regfit.full, 24))
VariableCOMMENT1 <- VariableCOMMENT1[2:25]

postsCOMMENT1 <- posts[,c('comments_countLOG',VariableCOMMENT1)]


###################################################
############# Model creation for like #############
###################################################

## 70% of the sample size
smp_size <- floor(0.7 * nrow(postsLIKE1))

## set the seed to make your partition reproducible
set.seed(125)
train_ind <- sample(seq_len(nrow(postsLIKE1)), size = smp_size)

train <- postsLIKE1[train_ind, ]
test <- postsLIKE1[-train_ind, ]

str(train)

bestmtry <- tuneRF(train[,-1], train$like_countLOG, ntree=1000)
#Running the RandomForest
RF <- randomForest(like_countLOG ~ ., data=train, importance=TRUE,ntree = 1000, mtry=5)
summary(RF)
plot(RF)
varImpPlot(RF, type=1)
#Predict the values of the test set
predicted <- predict(RF, test)
#Evaluating the model
predictions <- as.numeric(predicted, type='response')
TEST <- exp(predictions) - exp(test$like_countLOG)
mean(abs(TEST))
MSE <- mean(abs(TEST))**2
MSE



#######################################################
############# Model creation for comments #############
#######################################################



## 70% of the sample size
smp_size <- floor(0.7 * nrow(postsCOMMENT1))

## set the seed to make your partition reproducible
set.seed(125)
train_ind2 <- sample(seq_len(nrow(postsCOMMENT1)), size = smp_size)

train2 <- postsCOMMENT1[train_ind2, ]
test2 <- postsCOMMENT1[-train_ind2, ]

bestmtry <- tuneRF(train[,-1], train2$comments_countLOG, ntree=1000)
#Running the RandomForest
RF2 <- randomForest(comments_countLOG ~ ., data=train2, importance=TRUE,ntree = 1000, mtry=5)
plot(RF2)
varImpPlot(RF2)
# Predict the value of the test set
predicted2 <- predict(RF2, test2)
predicted2

predictions2 <- as.numeric(predicted2, type='response')
#Evaluation
TEST2 <- exp(predictions2) - exp(test2$comments_countLOG)
mean(abs(TEST2))
MSE2 <- mean(abs(TEST2))**2
MSE2

##################################################################################
##################################################################################
###################             CLASSIFICATION                ####################
##################################################################################
##################################################################################

#If lower than 500 likes, it's considered as a 0
postsCLASS$CAT <- ifelse(posts$like_count<500,0,1)
#If lower than 40 comments, it's considered as a 0
postsCLASS$CATCOMMENT <- ifelse(posts$comments_count<40,0,1)

########################################################
############# Variable selection for likes #############
########################################################

#Variable selection for likes

postsL <- postsCLASS
hist(posts$like_count)

postsL$comments_count <- postsL$feed_message <-  postsL$feed_created_time <- postsL$date<- postsL$lag <- postsL$like_count <- postsL$CATCOMMENT<- NULL
dim(postsL)
sum(is.na(postsL))
library(leaps)


regfit.full=regsubsets(CAT~.,postsL, really.big=TRUE, method='forward', nvmax=61)
reg.summary=summary(regfit.full)
options(max.print=999999)
par(mfrow=c(1,1))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS',type='l')
plot(reg.summary$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')

#From the graph we decided to keep 14 variables
plot3 <- plot(regfit.full, scale='bic')
VariableLIKE2 <- names(coef(regfit.full, 14))
VariableLIKE2 <- VariableLIKE2[2:15]

postsLIKE2 <- postsCLASS[,c('CAT',VariableLIKE2)]

###########################################################
############# Variable selection for comments #############
###########################################################

postsC <- postsCLASS
postsC$like_count <- postsC$feed_message <-  postsC$feed_created_time <- postsC$lag <- postsC$date <- postsC$comments_count  <- postsC$CAT<- NULL
dim(postsC)
sum(is.na(postsC))


regfit.full2=regsubsets(CATCOMMENT~.,postsC, really.big=TRUE, method='forward', nvmax=61)
reg.summary2=summary(regfit.full2)
options(max.print=999999)
par(mfrow=c(1,1))
plot(reg.summary2$rss, xlab='Number of Variables', ylab='RSS',type='l')
plot(reg.summary2$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')

#From the graph we decided to keep 16 variables
plot(regfit.full2, scale='bic')
VariableCOMMENT2 <- names(coef(regfit.full, 16))
VariableCOMMENT2 <- VariableCOMMENT2[2:17]

postsCOMMENT2 <- postsCLASS[,c('CATCOMMENT',VariableCOMMENT2)]

###################################################
############# Model creation for like #############
###################################################

postsLIKE2$CAT<-as.factor(postsLIKE2$CAT)


## 70% of the sample size
smp_size <- floor(0.7 * nrow(postsLIKE2))

## set the seed to make your partition reproducible
set.seed(125)
train_ind <- sample(seq_len(nrow(postsLIKE2)), size = smp_size)

train3 <- postsLIKE2[train_ind, ]
test3 <- postsLIKE2[-train_ind, ]


bestmtry <- tuneRF(train3[,-1], train3$CAT, ntree=1000)
#Random Forest
RF3 <- randomForest(CAT ~ ., data=train3,ntree = 1000, mtry=3)
RF3
summary(RF3)
plot(RF3)
varImpPlot(RF3)
#Predict the value in the test set
predicted3 <- predict(RF3, test3)
predicted32 <- predict(RF3, test3,type="prob")[,2]

predML1 <- prediction(as.numeric(predicted32),test3$CAT)

# Plotting the ROC curve
perfML1 <- performance(predML1 ,"tpr","fpr")
plot(perfML1)
abline(0,1)

## Computing the AUC
auc.perfML1 = performance(predML1, measure = "auc")
auc.perfML1@y.values

#Creation of the ConfusionMatrix
predicted3 <- as.factor(predicted3)
test3$CAT <- as.factor(test3$CAT)
confusionMatrix(predicted3,test3$CAT)

#######################################################
############# Model creation for comments #############
#######################################################

postsCOMMENT2$CATCOMMENT<-as.factor(postsCOMMENT2$CATCOMMENT)


## 70% of the sample size
smp_size <- floor(0.7 * nrow(postsCOMMENT2))

## set the seed to make your partition reproducible
set.seed(125)
train_ind2 <- sample(seq_len(nrow(postsCOMMENT2)), size = smp_size)

train4 <- postsCOMMENT2[train_ind2, ]
test4 <- postsCOMMENT2[-train_ind2, ]

bestmtry <- tuneRF(train4[,-1], train4$CATCOMMENT, ntree=1000)
#Random Foresy
RF4 <- randomForest(CATCOMMENT ~ ., data=train4,ntree = 1000, mtry=4)

summary(RF4)
plot(RF4)
varImpPlot(RF4)

predicted42 <- predict(RF4, test4,type="prob")[,2]
predicted4 <- predict(RF4, test4)


predML2 <- prediction(as.numeric(predicted42),test4$CATCOMMENT)

# Plotting the ROC curve
perfML2 <- performance(predML1 ,"tpr","fpr")
plot(perfML1)
abline(0,1)

## Computing the AUC
auc.perfML2 = performance(predML2, measure = "auc")
auc.perfML2@y.values

#Creation of the ConfusionMatrix
predicted4 <- as.factor(predicted4)
test4$CATCOMMENT <- as.factor(test4$CATCOMMENT)
confusionMatrix(predicted4,test4$CATCOMMENT)


plot(posts$numberhas, posts$comments_countLOG)
