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

#train/test split
n = length(dat[,1])

rnd_samp = sample (c(1:n), size = floor(n*0.3), replace = F)
train.set = dat[-rnd_samp,]
val.set = dat[rnd_samp,]


val_samp = sample (c(1:length(rnd_samp)), size = floor(length(rnd_samp)*0.5), replace = F)
test.set = val.set[-val_samp,]
val.set = val.set[val_samp,]

gc(remove(n, rnd_samp, val_samp))

#data cleaning
summary(train.set$track.genre_top)

train <- train.set[which(train.set$track.genre_top != ""),]
train <- train[which(train$track.genre_top != "Experimental"),]
train <- train[which(train$track.genre_top != "Instrumental"),]
train <- train[which(train$track.genre_top != "Blues"),]

train$track.genre_top = as.factor(as.character(train$track.genre_top))
summary(train$track.genre_top)

#View(colnames(train))
#View(head(train))

#potential predictors: album.tracks, album.listens/time, album.type,
#latitude, longitude, track.interest, track duration, social_features,
#audio_features

#do chi-squared test for all social features & audio features and 
#adjust for multiple comparisons
#create matrix to store results
csq.results <- matrix(0, nrow = 531, ncol = 4)
k <- 1
for (col in 20:550){
  if (col <=32){csq.results[k,4] <- "Echonest"}
  else{csq.results[k,4] <- "LibROSA"}
  label <- unlist(strsplit(colnames(train)[col], "[.]"))[2]
  csq.results[k,1] <- colnames(train)[col]
  temp <- table(train$track.genre_top, cut(train[,col],3)) #bin cts variables
  csq.results[k,2] <- chisq.test(temp)$p.value
  k <- (k+1)
}

csq.results[,3] <- p.adjust(csq.results[,2], method = "bonferroni")

write.csv(csq.results, "output/csq.results.csv")

#plot social features class conditional distributions on train set

for (col in c(25,26,27,28,30)){
    label <- colnames(train)[col]
    plot <- ggplot(train
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

train2<-train[which(train$track.genre_top == "Classical"),]
genres<- unique(train$track.genre_top)[-8]

for (genre in genres){
  ind<-which(train$track.genre_top == genre)
  if (length(ind) >= 182){
    newind <- sample(ind,182,replace = FALSE)
    train2 <- rbind(train2, train[newind,])
  }
  else{
    newind<-sample(ind,182,replace = TRUE)
    train2 <- rbind(train2, train[newind,])
  }
}

summary(train2$track.genre_top)

train3<-select(train2, c(audio_features.acousticness
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

#fit random forest model with default parameters
rf1 <- randomForest(track.genre_top ~ .
                       , data = train3
                       , importance = TRUE
                       , na.action=na.roughfix)

write.csv(rf1$confusion, "output/rf1_conf.csv")
write.csv(rf1$importance, "output/rf1_importance.csv")


varImpPlot(rf1
           ,type=2
           , main = "Feature Importance for RF1 model")

#MODEL RF2

train4 <- train2[,33:550]
train4 <- cbind(train4,train2[,14])
colnames(train4)[519] <- "track.genre_top"

#fit random forest model with default parameters
rf2 <- randomForest(track.genre_top ~ .
                       , data = train4
                       , importance = TRUE
                       , na.action=na.roughfix)

write.csv(rf2$confusion, "output/rf2_conf.csv")
write.csv(rf2$importance, "output/rf2_importance.csv")


varImpPlot(rf2
           ,type=2
           , main = "Feature Importance for RF2 model")


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

#prep test data for predictions

test <- test.set[which(test.set$track.genre_top != ""),]
test <- test[which(test$track.genre_top != "Experimental"),]
test <- test[which(test$track.genre_top != "Instrumental"),]
test <- test[which(test$track.genre_top != "Blues"),]

test$track.genre_top = as.factor(as.character(test$track.genre_top))

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

y3 <- as.numeric(train6$track.genre_top)-1
X3 <- train6
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

#Correlation plot
train5 <- select(train3, -c(album.type
                            , track.genre_top
                            , artist.latitude
                            , artist.longitude))

c <- strsplit(colnames(train5),"[.]")
colnames(train5) <- unlist(c)[c(FALSE,TRUE)]

Corrlxn <-cor(train5)
corrplot(Corrlxn, type="upper", order="hclust"
         , col=brewer.pal(n=8, name="RdYlBu")
         , mar = c(1,1,1,1))

train6 <- select(train3, -c(social_features.artist_hotttnesss))

lda1 <- lda(track.genre_top~., data = train6)

