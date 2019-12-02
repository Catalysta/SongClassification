
n = length(genre.train)

data.distance <- function(x,y) {
  distnc = 0
  for (i in 1:8) {
    distnc = distnc + (x[i]-y[i])^2
  }
  return(distnc[1,1])
}

k = 15

m = unname(length(genre.val))
val.pred = rep(0,m)


for (j in 1:150) {
  distances = cbind(1:n,rep(0,n))
  for (i in 1:n) {
    distances[i,2] = (features.train[i,1]-features.val[j,1])^2
    distances[i,2] = distances[i,2] + (features.train[i,2]-features.val[j,2])^2
    distances[i,2] = distances[i,2] + (features.train[i,3]-features.val[j,3])^2
    distances[i,2] = distances[i,2] + (features.train[i,4]-features.val[j,4])^2
    distances[i,2] = distances[i,2] + (features.train[i,5]-features.val[j,5])^2
    distances[i,2] = distances[i,2] + (features.train[i,6]-features.val[j,6])^2
    distances[i,2] = distances[i,2] + (features.train[i,7]-features.val[j,7])^2
    distances[i,2] = distances[i,2] + (features.train[i,8]-features.val[j,8])^2
  }
  distances = distances[order(distances[,2]),]
  
  k1 = k
  
  repeat{
    nearest = genre.train[distances[1:k1,1]]
    votes = max(summary(nearest))
    majority = which(summary(nearest) == votes)
    majority
    if (length(majority) == 1 || k1 <= 1) {
      break
    }
    k1 = k1 - 1
  }
  val.pred[j] = unname(majority)
}


results=cbind(as.numeric(genre.val),val.pred)
results = results[1:150,]

sum(results[,1] == results[,2])/150
