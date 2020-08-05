pkgs <- c("rpart")
sapply(pkgs, require, character.only = TRUE)


# functions
yf <- function(x){
  y <-  3*(1-x[,1])^2*exp(-x[,1]^2-(x[,2]+1)^2) - 10*(x[,1]/5-x[,1]^3-x[,2]^5)*exp(-x[,1]^2-x[,2]^2)- exp(-(x[,1]+1)^2-x[,2]^2)/3  + rnorm(1,0,0.2)
  return(y)
}

loss <- function(y, yhat){
  return(sum(1/2*(y-yhat)^2))
}


# initial
x1 <- runif(1000, -4, 4)
x2 <- runif(1000, -4, 4)
x <- data.frame(cbind(x1, x2))
y <- yf(x)
iter <- 100
eta <- 1


# implement the gradient boosting method
gradient.boost <- function(x,y,eta) {
  gb.list <- list()
  rsd <- y - mean(y)
  
  for (i in 1:iter){
    gb.model <- rpart(rsd ~ ., data = x, control = rpart.control(cp = -1, maxdepth = 2))
    gb.list[[i]] <- gb.model
    gb.pred <- predict(gb.model, x)
    rsd <- rsd - eta * gb.pred
  }
  return (gb.list)
}

model <- gradient.boost(x,y,eta)
yhat <- rep(mean(y), 1000)
result.vec <- c() 

for (i in (1:iter)){
  model.pred <- predict(model[[i]], x)
  yhat <- yhat + eta * model.pred
  result.vec <- append(result.vec, loss(y, yhat))
}

plot(result.vec)

