
rpart_ret <- function(formula, data.train, data.test, prune, class.response, response){

ret <- list()

return.matrix <- matrix(NA,1,8)
colnames(return.matrix) <- c("nodes","nvar","nsplits","misfit.cv","misfit.train","rsq.train","misfit.test","rsq.test")

rpart.out <- rpart(formula,data.train)


cp <- rpart.out$cptable
min.error <- which(min(cp[,"xerror"]) == cp[,"xerror"])[1]
return.matrix[1,"nsplits"] <- cp[min.error,"nsplit"]
return.matrix[1,"misfit.cv"] <- cp[min.error,"xerror"]


if(prune == TRUE){
  rpart.ret <- prune.rpart(rpart.out,cp[min.error,"CP"])
}else{
  rpart.ret <- rpart.out
}


vars <- rpart.ret$frame[,"var"]
vars2 <- vars[vars != "<leaf>"]
return.matrix[1,"nvar"] <- length(unique(vars2))

return.matrix[1,"nodes"] <- length(vars[vars == "<leaf>"])

if(class.response == "numeric" | class.response == "integer"){

  return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(rpart.out))^2)/nrow(data.train)
  return.matrix[1,"misfit.test"] <- mean((data.test[,response] -
                                            predict(rpart.out,data.test))^2)/nrow(data.test)
  return.matrix[1,"rsq.train"] <- (cor(data.train[,response],predict(rpart.out)))**2
  return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(rpart.out,data.test)))**2
}else{
  return.matrix[1,"misfit.train"] <- NA
  return.matrix[1,"misfit.test"] <- NA
}


ret$vec <- return.matrix
ret$rpart.ret <- rpart.ret
return(ret)

}
