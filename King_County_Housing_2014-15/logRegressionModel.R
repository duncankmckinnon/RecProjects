#Logistic Regression Model
#Duncan McKinnon


logisticRegModel <- function(XTrain, YTrain, XTest, YTest, alpha = 0.01, num_iters = 10, raw = F)
{
  XTrain <- as.matrix(t(XTrain))
  YTrain <- as.matrix(t(YTrain))
  XTest <- as.matrix(t(XTest))
  YTest <- as.matrix(t(YTest))
  w = matrix(0, nrow = dim(XTrain)[1])
  b = 0
  
  vals <- optimize(w, b, XTrain, YTrain, alpha, num_iters)
  
  pred_Train <- as.matrix(predict(vals$w, vals$b, XTrain, raw), nrow = 1)
  pred_Test <- as.matrix(predict(vals$w, vals$b, XTest, raw), nrow = 1)
  
  accuracy_Train <- 1 - (sum(abs(pred_Train - t(YTrain))) / length(YTrain))
  accuracy_Test <- 1 - (sum(abs(pred_Test - t(YTest))) / length(YTest))
  
  logMod <- list("w" = vals$w, "b" = vals$b, "costs" = vals$costs, 
              "Train_Per" = accuracy_Train, "Test_Per" = accuracy_Test,
              "Train_Vals" = pred_Train, "Test_Vals" = pred_Test)
  return(logMod)
}


predict <- function(w, b, XTest, raw = F)
{
  if(!raw)
  {
    return(ifelse(sigma(t(XTest) %*% w + b) > 0.5, 1, 0))
  }
  return(sigma(t(XTest) %*% w + b))
}

optimize <- function(w, b, XTrain, YTrain, alpha, num_iters)
{
  costs <- c()
  for(i in 1:num_iters)
  {
    vals <- propogate(w, b, XTrain, YTrain)
    
    w = w - (alpha * vals$dw)
    b = b - (alpha * vals$db)
    costs <- c(costs, vals$cost)
  }
  return(list("w" = w, "b" = b, "dw" = vals$dw, "db" = vals$db,  "costs" = costs))
}

propogate <- function(w, b, XTrain, YTrain)
{
  m <- dim(XTrain)[2]
  
  guess <- sigma(t(XTrain) %*% w + b)
  
  cost <- -(1/m) * sum((YTrain %*% log(guess)) + (1 - YTrain) %*% log(1 - guess))
  
  
  
  dw <- (1/m) * XTrain %*% (guess - t(YTrain))
  
  db <- (1/m) * sum(guess - t(YTrain))
  
  return(list("dw" = dw, "db" = db, "cost" = cost))
}

sigma <- function(z, type = "Sigmoid")
{
  return(1 / (1 + exp(-z)))
}