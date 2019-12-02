
set.seed(6750)

setwd("C:/Users/ferjo/Documents/MS Statistics/3rd semester/STAT 6750/Project")

music.data <- read.csv("audio_features.csv")

#pairs(music.data[,6:13],col = music.data$track.genre_top)

summary(music.data$track.genre1)
summary(music.data$track.genre_top)

plot(c(1:16),col=c(1:16))

music.data1 = music.data[which(music.data$track.genre_top != ""),]
music.data1 = music.data1[which(music.data1$track.genre_top != "Experimental"),]
music.data1 = music.data1[which(music.data1$track.genre_top != "Instrumental"),]
music.data1 = music.data1[which(music.data1$track.genre_top != "Blues"),]
summary(music.data1$track.genre_top)
#pairs(music.data[,6:13],col = music.data$track.genre_top)

music.data1$track.genre_top = as.factor(as.character(music.data1$track.genre_top))

music.data1$audio_features.tempo = music.data1$audio_features.tempo/max(music.data1$audio_features.tempo)



n = length(music.data1[,1])

rnd_samp = sample (c(1:n), size = floor(n*0.3), replace = F)
train.set = music.data1[-rnd_samp,]
val.set = music.data1[rnd_samp,]


val_samp = sample (c(1:length(rnd_samp)), size = floor(length(rnd_samp)*0.5), replace = F)
test.set = val.set[-val_samp,]
val.set = val.set[val_samp,]


features.train <- train.set[,6:13]
genre.train <- train.set$track.genre_top

features.val <- val.set[,6:13]
genre.val <- val.set$track.genre_top

features.test <- test.set[,6:13]
genre.test <- test.set$track.genre_top


summary(genre.train)
summary(genre.val)
summary(genre.test)
