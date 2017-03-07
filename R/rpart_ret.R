
rpart_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response){

ret <- list()

if(class.response == "numeric" | class.response == "integer"){
  return.matrix <- matrix(NA,1,7)
  colnames(return.matrix) <- c("nodes","nvar","nsplits","rmse.samp",
                              "rsq.samp","rmse.test","rsq.test")
}else{
  return.matrix <- matrix(NA,1,5)
  colnames(return.matrix) <- c("nodes","nvar","nsplits","accuracy.samp",
                               "accuracy.test")
}

#rpart.out <- rpart(formula,data.train)

ctrl <- trainControl(method=samp.method)
train.out <- train(formula,data.train,method="rpart",tuneLength=tuneLength,
                   trControl=ctrl)
rpart.out <- train.out$finalModel

cp <- rpart.out$cptable
min.error <- which(min(cp[,"rel error"]) == cp[,"rel error"])[1]
return.matrix[1,"nsplits"] <- cp[min.error,"nsplit"]
#train.out$varImp



  rpart.ret <- rpart.out



vars <- rpart.ret$frame[,"var"]
vars2 <- vars[vars != "<leaf>"]
return.matrix[1,"nvar"] <- length(unique(vars2))

return.matrix[1,"nodes"] <- length(vars[vars == "<leaf>"])
ind <- as.numeric(row.names(train.out$bestTune))
if(class.response == "numeric" | class.response == "integer"){
   #which(train.out$results[,"cp"] == train.out$bestTune)

  return.matrix[1,"rmse.samp"] <- train.out$results[ind,"RMSE"]
  #return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(rpart.out))^2)/nrow(data.train)
  return.matrix[1,"rsq.samp"] <- train.out$results[ind,"Rsquared"]

      if(subset==FALSE){
        return.matrix[1,"rmse.test"] <- NA
        return.matrix[1,"rsq.test"] <- NA
      }else{
        return.matrix[1,"rmse.test"] <- mean((data.test[,response] -
                                                  predict(rpart.out,data.test))^2)/nrow(data.test)
        return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(rpart.out,data.test)))**2
      }
}else{
  return.matrix[1,"accuracy.samp"] <- train.out$results[ind,"Accuracy"]
  #return.matrix[1,"accuracy.train"] <- mean(round(predict(rpart.out)[,2])+1 == as.numeric(data.train[,response]))
  return.matrix[1,"accuracy.test"] <- mean(round(predict(rpart.out,data.test)[,2])+1 == as.numeric(data.test[,response]))
}


ret$vec <- return.matrix
ret$rpart.ret <- rpart.ret
ret$rpart.train <- train.out
return(ret)

}
