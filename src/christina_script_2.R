##### STAT6750 FINAL #####
#Purpose: genre classification for subset of the Million Song Dataset
#Author: Christina Sousa
#Date: 2019-11-02
#Credits: Train/test split and data cleaning by Fernando
################################################################################

#set seed
set.seed(6750)

#libaries
library(tidyverse)
library(randomForest)
library(corrplot)
library(RColorBrewer)
library(xgboost)
library(Matrix)
library(caret)
library(ggplot2)
#library(MASS)

#set working directory
setwd("~/OhioState/STAT6750/StatConsultingFinal")

#load
#dat <- read.csv("data/music.csv")
#save(dat1, file = "musicdata.RData")
load("dat.RData")

#data cleaning
summary(dat$track.genre_top)

dat <- dat[which(dat$track.genre_top != ""),]
dat <- dat[which(dat$track.genre_top != "Experimental"),]
dat <- dat[which(dat$track.genre_top != "Instrumental"),]
dat <- dat[which(dat$track.genre_top != "Blues"),]

dat$track.genre_top = as.factor(as.character(dat$track.genre_top))
summary(dat$track.genre_top)

#train/test split
n = length(dat[,1])

rnd_samp = sample (c(1:n), size = floor(n*0.3), replace = F)
train.set = dat[-rnd_samp,]
val.set = dat[rnd_samp,]


val_samp = sample (c(1:length(rnd_samp)), size = floor(length(rnd_samp)*0.5), replace = F)
test.set = val.set[-val_samp,]
val.set = val.set[val_samp,]

gc(remove(n, rnd_samp, val_samp))

train <- train.set
#View(colnames(train))
#View(head(train))


##### EDA #####
#potential predictors: album.tracks, album.listens/time, album.type,
#latitude, longitude, track.interest, track duration, social_features,
#audio_features
train2<-select(train, c(audio_features.acousticness
                         , audio_features.danceability
                         , audio_features.energy
                         , audio_features.instrumentalness
                         , audio_features.liveness
                         , audio_features.speechiness
                         , audio_features.tempo
                         , audio_features.valence
                         , social_features.artist_discovery
                         , social_features.artist_familiarity
                         , social_features.artist_hotttnesss
                         , social_features.song_currency
                         , social_features.song_hotttnesss
                         , album.tracks
                         , album.type
                         , artist.latitude
                         , artist.longitude
                         , track.duration
                         , track.genre_top))

#Correlation plot
train3 <- select(train2, -c(album.type
                            , track.genre_top
                            , artist.latitude
                            , artist.longitude))

c <- strsplit(colnames(train3),"[.]")
colnames(train3) <- unlist(c)[c(FALSE,TRUE)]

Corrlxn <-cor(train3)
corrplot(Corrlxn, type="upper", order="hclust"
         , col=brewer.pal(n=8, name="RdYlBu")
         , mar = c(1,1,1,1))

#examine collinear features;
#drop acousticness, discovery, hotttnesss due to unfavorable distributions
par(mfrow=c(3,3))
hist(train3$acousticness, col = 'skyblue')
hist(train3$energy, col = 'skyblue')
hist(train3$artist_familiarity, col = 'skyblue')
hist(train3$artist_discovery, col = 'skyblue')
hist(train3$artist_hotttnesss, col = 'skyblue')
hist(train3$song_currency, col = 'skyblue')
hist(train3$song_hotttnesss, col = 'skyblue')


#remove colinear features
train4 <- select(train2, -c(audio_features.acousticness
                            , social_features.artist_discovery
                            , social_features.artist_hotttnesss
                            , social_features.song_currency))

#plot social and audio features to check for skewnewss/log transformation
par(mfrow = c(4,3))
for(col in c(1:10,14)){
  label <- unlist(strsplit(colnames(train4)[col], "[.]"))[2]
  hist(train4[,col], col = 'skyblue', main = label, xlab = label)
}

#log transform liveness, speechiness, tracks, duration
train4$a.lliveness <- log(train4$audio_features.liveness+.Machine$double.eps)
train4$a.lspeechiness <- log(train4$audio_features.speechiness
                           +.Machine$double.eps)
train4$t.lduration <- log(train4$track.duration+.Machine$double.eps)

#plot transformed social and audio features
par(mfrow = c(4,3))
for(col in c(1:3,6:9,16:18)){
  label <- unlist(strsplit(colnames(train4)[col], "[.]"))[2]
  hist(train4[,col], col = 'skyblue', main = label, xlab = label)
}

train4 <- train4[,c(1:3,6:9,16:17,10:14,15)]

#do chi-squared test for all features and 
#adjust for multiple comparisons
#also get cramer's V statistics on each of the features

#create matrix to store results
csq.results <- matrix(0, nrow = 8, ncol = 4)
k <- 1
for (col in c(1:6,8:9)){
  #if (col <=32){csq.results[k,5] <- "Echonest"}
  #else{csq.results[k,5] <- "LibROSA"}
  label <- unlist(strsplit(colnames(train4)[col], "[.]"))[2]
  csq.results[k,1] <- label #colnames(train4)[col]
  temp <- table(train4$track.genre_top, cut(train4[,col],3)) #bin cts variables
  test <- chisq.test(temp)
  csq.results[k,2] <- test$p.value
  csq.results[k,4] <- sqrt(test$statistic/(6432 * (min(9,3)-1)))
  k <- (k+1)
}

csq.results[,3] <- p.adjust(csq.results[,2], method = "bonferroni")
colnames(csq.results) <- c("variable", "X2 p-val", "p.adj", "Cramers V")

write.csv(csq.results, "output/csq.results2.csv")

#plot social features class conditional distributions on train set
par(mfrow=c(4,2))
for (col in c(2,1,9,3,6,5,8,4)){
    label <- colnames(train4)[col]
    plot <- ggplot(train4
                   , aes_string(x = "track.genre_top"
                                , y = label
                                , fill = "track.genre_top")) +
      geom_violin(trim = FALSE) + 
      geom_boxplot(width=0.1) +
      #ggtitle(unlist(strsplit(label, "[.]"))[2]) +
      labs(title = unlist(strsplit(label, "[.]"))[2]
           , x = ""
           , y = "") + 
      theme(plot.title = element_text(lineheight = 1.5)) + 
      theme(legend.position="none")
    print(plot)
}


#HISTOGRAMS (not as good as violin plots)
#par(mfrow = c(3,3))
#for (col in 20:32){
#  for (genre in levels(train$track.genre_top)){
#    label <- unlist(strsplit(colnames(train)[col], "[.]"))[2]
#    hist(train[which(train$track.genre_top==genre),col]
#         , main = genre
#         , col = 'skyblue'
#         , xlab = label)
#  }
#}
#par(mfrow = c(1,1))

#####Random Forest#####

#MODEL RF1

#balance data by down- and up-sampling each of the 9 genres to have
#182 members
summary(train$track.genre_top)
train2<-train[which(train$track.genre_top == "Classical"),]
genres<- unique(train$track.genre_top)[-8]

for (genre in genres){
  ind<-which(train$track.genre_top == genre)
  if (length(ind) >= 192){
    newind <- sample(ind,192,replace = FALSE)
    train2 <- rbind(train2, train[newind,])
  }
  else{
    newind<-sample(ind,192,replace = TRUE)
    train2 <- rbind(train2, train[newind,])
  }
}

summary(train2$track.genre_top)


#fit random forest model with default parameters
rf1 <- randomForest(track.genre_top ~ .
                    , data = train3
                    , ntree = 1000
                    , importance = TRUE
                    , na.action=na.roughfix)

write.csv(rf1$confusion, "output/rf1trainconf.csv")
write.csv(rf1$importance, "output/rf1trainimportance.csv")


varImpPlot(rf1
           ,type=2
           , main = "Feature Importance for RF1 model")

#Check performance on validation set
valid <- val.set

valid2<-select(valid, c(audio_features.acousticness
                         , audio_features.danceability
                         , audio_features.energy
                         , audio_features.instrumentalness
                         , audio_features.liveness
                         , audio_features.speechiness
                         , audio_features.tempo
                         , audio_features.valence
                         , social_features.artist_discovery
                         , social_features.artist_familiarity
                         , social_features.artist_hotttnesss
                         , social_features.song_currency
                         , social_features.song_hotttnesss
                         , album.tracks
                         , album.type
                         , artist.latitude
                         , artist.longitude
                         , track.duration
                         , track.genre_top))

rf1.pred <- predict(rf1, newdata = valid2[,-19])

rf1.conf <- confusionMatrix(as.factor(rf1.pred),as.factor(valid2[,19]))$table
class.accuracy <- diag(rf1.conf)/rowSums(rf1.conf)
rf1.conf <- cbind(rf1.conf, class.accuracy)

rownames(rf1.conf) <- levels(train$track.genre_top)
colnames(rf1.conf) <- c(rownames(rf1.conf), "class accuracy")
write.csv(rf1.conf, "output/rf1validconf.csv")

overall.accuracy <- sum(diag(rf1.conf))/sum(rf1.conf)
#0.6948

#MODEL RF2

train4 <- train2[,33:550]
train4 <- cbind(train4,train2[,14])
colnames(train4)[519] <- "track.genre_top"

#fit random forest model with default parameters
rf2 <- randomForest(track.genre_top ~ .
                    , data = train4
                    , ntree = 1000
                    , importance = TRUE
                    , na.action=na.roughfix)

write.csv(rf2$confusion, "output/rf2trainconf.csv")
write.csv(rf2$importance, "output/rf2trainimportance.csv")


varImpPlot(rf2
           ,type=2
           , main = "Feature Importance for RF2 model")

#Check performance on validation set
valid3<-valid[,33:550]

rf2.pred <- predict(rf2, newdata = valid3)

rf2.conf <- confusionMatrix(as.factor(rf2.pred),as.factor(valid[,14]))$table
class.accuracy <- diag(rf2.conf)/rowSums(rf2.conf)
rf2.conf <- cbind(rf2.conf, class.accuracy)

rownames(rf2.conf) <- levels(train$track.genre_top)
colnames(rf2.conf) <- c(rownames(rf2.conf), "class accuracy")
write.csv(rf2.conf, "output/rf2validconf.csv")

overall.accuracy <- sum(diag(rf2.conf))/sum(rf2.conf)
#0.6471


#####XG Boost#####
train5<-select(train, c(audio_features.acousticness
                         , audio_features.danceability
                         , audio_features.energy
                         , audio_features.instrumentalness
                         , audio_features.liveness
                         , audio_features.speechiness
                         , audio_features.tempo
                         , audio_features.valence
                         , social_features.artist_discovery
                         , social_features.artist_familiarity
                         , social_features.artist_hotttnesss
                         , social_features.song_currency
                         , social_features.song_hotttnesss
                         , album.tracks
                         , album.type
                         , artist.latitude
                         , artist.longitude
                         , track.duration
                         , track.genre_top))

#MODEL XGB1
y <- as.numeric(train5$track.genre_top)-1
X <- train5[,1:19]
X[is.na(X)] <- 0
X <- sparse.model.matrix(track.genre_top ~ .-1, data = X)

Xtrain <- xgb.DMatrix(data = X,label = y)
xgb1 <- xgboost(data = X
                , label = y
                , nrounds = 100
                , subsample = 0.5
                , colsample_bytree = 0.3
                , seed = 6750
                , objective = "multi:softprob"
                , num_class = 9
                , max_depth = 3
                , nthread = 2)

#get predictions on training set
xgb1trainprobs <- predict(xgb1,Xtrain)

#set up function to generate confusion matrix
getconf <- function(probs, y, name){
  xgb1preds <- matrix(nrow = length(y), ncol = 10)
  j <- 1
  for (i in 1:length(y)){
    xgb1preds[i,1:9] <- probs[j:(j+8)]
    xgb1preds[i,10] <- which(xgb1preds[i,1:9] == max(xgb1preds[i,1:9]))
    j <- (j+9)
  }
  
  conf <- confusionMatrix(as.factor(xgb1preds[,10])
                          , as.factor(y))$table
  
  class.error <- diag(conf)/rowSums(conf)
  conf <- cbind(conf, 1-class.error)
  
  rownames(conf) <- levels(train$track.genre_top)
  colnames(conf) <- c(rownames(conf), "class.error")
  
  write.csv(conf, paste("output/", name))
}

#get confusion matrix for training set predictions
getconf(xgb1trainprobs, y=(y+1), "xgb1trainconf.csv")

#get confusion matrix for validation set predictions
y.valid <- as.numeric(valid2$track.genre_top)
X.valid <- valid2[,1:19]
X.valid[is.na(X.valid)] <- 0
Xvalid <- sparse.model.matrix(track.genre_top ~ .-1, data = X.valid)

#get predictions
xgb1validprobs <- predict(xgb1,Xvalid)

#convert probabilities to numeric predictions:
getconf(xgb1validprobs,y.valid,"xgb1validconf.csv")

#overall accuracy
#0.8287

#prep test data for predictions
test3<-select(test, c(audio_features.acousticness
                         , audio_features.danceability
                         , audio_features.energy
                         , audio_features.instrumentalness
                         , audio_features.liveness
                         , audio_features.speechiness
                         , audio_features.tempo
                         , audio_features.valence
                         , social_features.artist_discovery
                         , social_features.artist_familiarity
                         , social_features.artist_hotttnesss
                         , social_features.song_currency
                         , social_features.song_hotttnesss
                         , album.tracks
                         , album.type
                         , artist.latitude
                         , artist.longitude
                         , track.duration
                         , track.genre_top))

y.test <- as.numeric(test3$track.genre_top)
X.test <- test3[,1:19]
X.test[is.na(X.test)] <- 0
Xtest <- sparse.model.matrix(track.genre_top ~ .-1, data = X.test)

#get predictions
xgb1testprobs <- predict(xgb1,Xtest)

#convert probabilities to numeric predictions:
getconf(xgb1testprobs,y.test,"xgb1testconf.csv")

#get feature importance
xgb.plot.importance(xgb.importance(model = xgb1)
                    , main = "XGB1 Feature Importance")


#MODEL XGB2

train6<-select(train, c(audio_features.acousticness
                        , audio_features.danceability
                        , audio_features.energy
                        , audio_features.instrumentalness
                        , audio_features.liveness
                        , audio_features.speechiness
                        , audio_features.tempo
                        , audio_features.valence
                        , social_features.artist_discovery
                        , social_features.artist_familiarity
                        , social_features.artist_hotttnesss
                        , social_features.song_currency
                        , social_features.song_hotttnesss
                        , album.tracks
                        , album.type
                        , artist.latitude
                        , artist.longitude
                        , track.duration
                        , track.genre_top))
train6 <- cbind(train6,train[,33:550])

y2 <- as.numeric(train6$track.genre_top)-1
X2 <- train6
X2[is.na(X2)] <- 0
X2 <- sparse.model.matrix(track.genre_top ~ .-1, data = X2)

X2train <- xgb.DMatrix(data = X2,label = y2)
xgb2 <- xgboost(data = X2
                , label = y2
                , nrounds = 25
                , subsample = 0.5
                , colsample_bytree = 0.5
                , seed = 6750
                , objective = "multi:softprob"
                , num_class = 9
                , max_depth = 3
                , nthread = 2)

#get predictions on training set
xgb2trainprobs <- predict(xgb2,X2train)

#get confusion matrix for training set predictions
getconf(xgb2trainprobs, y=(y+1), "xgb2trainconf.csv")

#get test predictions
test4<-select(test, c(audio_features.acousticness
                      , audio_features.danceability
                      , audio_features.energy
                      , audio_features.instrumentalness
                      , audio_features.liveness
                      , audio_features.speechiness
                      , audio_features.tempo
                      , audio_features.valence
                      , social_features.artist_discovery
                      , social_features.artist_familiarity
                      , social_features.artist_hotttnesss
                      , social_features.song_currency
                      , social_features.song_hotttnesss
                      , album.tracks
                      , album.type
                      , artist.latitude
                      , artist.longitude
                      , track.duration
                      , track.genre_top))
test4 <- cbind(test4, test[,33:550])

y2.test <- as.numeric(test4$track.genre_top)
X2.test <- test4
X2.test[is.na(X2.test)] <- 0
X2test <- sparse.model.matrix(track.genre_top ~ .-1, data = X2.test)

#get predictions
xgb2testprobs <- predict(xgb2,X2test)

#convert probabilities to numeric predictions:
getconf(xgb2testprobs,y2.test,"xgb2testconf.csv")

#MODEL XGB3

train7<-select(train, c(audio_features.acousticness
                        , audio_features.danceability
                        , audio_features.energy
                        , audio_features.instrumentalness
                        , audio_features.liveness
                        , audio_features.speechiness
                        , audio_features.tempo
                        , audio_features.valence
                        , social_features.artist_discovery
                        , social_features.artist_familiarity
                        , social_features.artist_hotttnesss
                        , social_features.song_currency
                        , social_features.song_hotttnesss
                        , album.tracks
                        , album.type
                        , artist.latitude
                        , artist.longitude
                        , track.duration
                        , track.genre_top))
train7 <- cbind(train7,train[,305:364]) #add only mfcc variables

y3 <- as.numeric(train$track.genre_top)-1
X3 <- train7
X3[is.na(X3)] <- 0
X3 <- sparse.model.matrix(track.genre_top ~ .-1, data = X3)

X3train <- xgb.DMatrix(data = X3,label = y3)
xgb3 <- xgboost(data = X3
                , label = y3
                , nrounds = 25
                , subsample = 0.5
                , colsample_bytree = 0.5
                , seed = 6750
                , objective = "multi:softprob"
                , num_class = 9
                , max_depth = 3
                , nthread = 2)

#get predictions on training set
xgb3trainprobs <- predict(xgb3,X3train)

#get confusion matrix for training set predictions
getconf(xgb3trainprobs, y=(y3+1), "xgb3trainconf.csv")

#get confusion matrix for validation set
valid4 <- select(valid, c(audio_features.acousticness
                          , audio_features.danceability
                          , audio_features.energy
                          , audio_features.instrumentalness
                          , audio_features.liveness
                          , audio_features.speechiness
                          , audio_features.tempo
                          , audio_features.valence
                          , social_features.artist_discovery
                          , social_features.artist_familiarity
                          , social_features.artist_hotttnesss
                          , social_features.song_currency
                          , social_features.song_hotttnesss
                          , album.tracks
                          , album.type
                          , artist.latitude
                          , artist.longitude
                          , track.duration
                          , track.genre_top))
valid4 <- cbind(valid4,valid[,305:364])
y.valid <- as.numeric(valid4$track.genre_top)
X.valid <- valid4
X.valid[is.na(X.valid)] <- 0
Xvalid <- sparse.model.matrix(track.genre_top ~ .-1, data = X.valid)

#get predictions
xgb3validprobs <- predict(xgb3,Xvalid)

#convert probabilities to numeric predictions:
getconf(xgb3validprobs,y.valid,"xgb3validconf.csv")

#get test predictions
test5<-select(test, c(audio_features.acousticness
                      , audio_features.danceability
                      , audio_features.energy
                      , audio_features.instrumentalness
                      , audio_features.liveness
                      , audio_features.speechiness
                      , audio_features.tempo
                      , audio_features.valence
                      , social_features.artist_discovery
                      , social_features.artist_familiarity
                      , social_features.artist_hotttnesss
                      , social_features.song_currency
                      , social_features.song_hotttnesss
                      , album.tracks
                      , album.type
                      , artist.latitude
                      , artist.longitude
                      , track.duration
                      , track.genre_top))
test5 <- cbind(test5, test[,305:364])

y3.test <- as.numeric(test5$track.genre_top)
X3.test <- test5
X3.test[is.na(X3.test)] <- 0
X3test <- sparse.model.matrix(track.genre_top ~ .-1, data = X3.test)

#get predictions
xgb3testprobs <- predict(xgb3,X3test)

#convert probabilities to numeric predictions:
getconf(xgb3testprobs,y3.test,"xgb3testconf.csv")


#####Discriminant Analysis#####

train6 <- select(train3, -c(social_features.artist_hotttnesss))

lda1 <- lda(track.genre_top~., data = train6)

