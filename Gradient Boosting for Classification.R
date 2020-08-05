pkgs <- c("rpart", "caret", "dplyr")
sapply(pkgs, require, character.only = TRUE)

data <- read.csv("C:/Users/lvaid/skku/Technometrics/Data/Dataset/letter-recognition.csv", header = F)
attach(data)

y = data[, 1]
Q.prob = array(10^-5,c(nrow(y), nrow(Q.list))) #해당 알파벳에 1 들어간.

for(i in 1:length(y)){
  Q.prob[i, y[i]] <- 1
}


indexes = createDataPartition(data$V1, p = .70, list = F)
train = data.frame(data[indexes, ])
test = data.frame(data[-indexes, ])
train_x = train[,-1]
test_x = test[,-1]
train_y = Q.prob[indexes,]
test_y = Q.prob[-indexes,]

# function set up
iter <- 10
eta <- 0.001

# implement the gradient boosting method
gradient.boost <- function(x_matrix, y_matrix, eta) {
  gb.list <- rep(list(list()), ncol(y_matrix))   
  P.prob <- array(1/ncol(y_matrix),c(nrow(y_matrix),ncol(y_matrix)))
  
  for (i in 1:iter){
    rsd <- -(log(P.prob/y_matrix)+1)
    for (j in 1:ncol(y_matrix)){
      gb.list[[j]][[i]] <- rpart(rsd[,j] ~ ., data = x_matrix)
      gb.pred <- predict(gb.list[[j]][[i]], x_matrix)
      P.prob[,j] <- P.prob[,j] * exp(eta * gb.pred)
    }
    P.prob <- P.prob / rowSums(P.prob)
  }
  return (gb.list)
}

model <- gradient.boost(train_x, train_y, eta)
yhat <- array(1/ncol(test_y),c(nrow(test_y),ncol(test_y)))
result.vec <- c() 

loss <- function(q,p){
  err <- sum(p * log(p/q))
  return (err)
}

for (i in (1:iter)){
  for (j in (1:length(model))){
    model.pred <- predict(model[[j]][[i]], test_x)
    yhat[,j] <- yhat[,j] * exp(eta * model.pred)
  }
  yhat <- yhat / rowSums(yhat)
  result.vec <- append(result.vec, loss(test_y, yhat))
}

plot(result.vec)
