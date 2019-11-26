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

train1 <- train.set[which(train.set$track.genre_top != ""),]
train1 <- train1[which(train1$track.genre_top != "Experimental"),]
train1 <- train1[which(train1$track.genre_top != "Instrumental"),]
train1 <- train1[which(train1$track.genre_top != "Blues"),]

train1$track.genre_top = as.factor(as.character(train1$track.genre_top))
summary(train1$track.genre_top)

View(colnames(train1))
View(head(train1))

#potential predictors: album.tracks, album.listens/time, album.type,
#latitude, longitude, track.interest, track duration, social_features,
#audio_features

train2<-select(train1, c(audio_features.acousticness
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

#balance data by down- and up-sampling each of the 9 genres to have
#182 members

train3<-train2[which(train2$track.genre_top == "Classical"),]
genres<- unique(train2$track.genre_top)[-8]

for (genre in genres){
  ind<-which(train2$track.genre_top == genre)
  if (length(ind) >= 182){
    newind <- sample(ind,182,replace = FALSE)
    train3 <- rbind(train3, train2[newind,])
  }
  else{
    newind<-sample(ind,182,replace = TRUE)
    train3 <- rbind(train3, train2[newind,])
  }
}

summary(train3$track.genre_top)

#fit random forest model with default parameters
model1 <- randomForest(track.genre_top ~ .
                       , data = train3
                       , importance = TRUE
                       , na.action=na.roughfix)

write.csv(model1$confusion, "output/rf1_conf.csv")
write.csv(model1$importance, "output/rf1_importance.csv")


varImpPlot(model1
           ,type=2
           , main = "Feature Importance for RF1 model")
