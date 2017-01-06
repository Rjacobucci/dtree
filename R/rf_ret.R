
rf_ret <- function(formula, data.train, data.test,class.response, response){
  ret <- list()

  if(class.response == "numeric" | class.response == "integer"){
    return.matrix <- matrix(NA,1,8)
    colnames(return.matrix) <- c("nodes","nvar","nsplits","misfit.cv",
                                 "misfit.train","rsq.train","misfit.test","rsq.test")
  }else{
    return.matrix <- matrix(NA,1,6)
    colnames(return.matrix) <- c("nodes","nvar","nsplits","accuracy.cv",
                                 "accuracy.train","accuracy.test")
  }


if(class.response == "numeric" | class.response == "integer"){
  rf.out <- train(formula,data.train,method="rf",
                  trControl=trainControl(method="cv"),importance=TRUE)
}else{
  rf.out <- train(formula,data.train,method="rf",metric="Accuracy",
                  trControl=trainControl(method="cv"),importance=TRUE)
}






if(class.response == "numeric" | class.response == "integer"){

  return.matrix[1,"misfit.train"] <- suppressWarnings(mean((data.train[,response] - predict(rf.out))^2)/nrow(data.train))
  return.matrix[1,"misfit.test"] <- suppressWarnings(mean((data.test[,response] -
                                            predict(rf.out,data.test))^2)/nrow(data.test))
  return.matrix[1,"misfit.cv"] <- min(rf.out$results$RMSE)/(.75*nrow(data.train))
  return.matrix[1,"rsq.train"] <- suppressWarnings((cor(data.train[,response],predict(rf.out)))**2)
  return.matrix[1,"rsq.test"] <- suppressWarnings((cor(data.test[,response],predict(rf.out,data.test)))**2)
}else{
  #return.matrix[1,"accuracy.cv"] <- cp[min.error,"xerror"]
  return.matrix[1,"accuracy.train"] <- mean(round(predict(rf.out)[,2])+1 == as.numeric(data.train[,response]))
  return.matrix[1,"accuracy.test"] <- mean(round(predict(rf.out,data.test)[,2])+1 == as.numeric(data.test[,response]))
}


ret$vec <- return.matrix
ret$rf.ret <- rf.out
return(ret)

}
