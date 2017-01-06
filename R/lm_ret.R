
lm_ret <- function(formula, data.train, data.test,class.response, response){

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

  lm.out <- lm(formula,data.train)
  p_s <- summary(lm.out)$coefficients[,"Pr(>|t|)"] < 0.05
  return.matrix[1,"nvar"] <- sum(p_s[-1])
}else{
  lm.out <- glm(formula,family=binomial,data.train)
  return.matrix[1,"nvar"] <- sum(summary(lm.out)$coefficients[,"Pr(>|z|)"][-1] < .05)
}



#preds <- predict(lm.out)


if(class.response == "numeric" | class.response == "integer"){

  return.matrix[1,"misfit.train"] <- suppressWarnings(mean((data.train[,response] - predict(lm.out))^2)/nrow(data.train))
  return.matrix[1,"misfit.test"] <- suppressWarnings(mean((data.test[,response] -
                                            predict(lm.out,data.test))^2)/nrow(data.test))
  return.matrix[1,"rsq.train"] <- suppressWarnings((cor(data.train[,response],predict(lm.out)))**2)
  return.matrix[1,"rsq.test"] <- suppressWarnings((cor(data.test[,response],predict(lm.out,data.test)))**2)
}else{
  return.matrix[1,"accuracy.train"] <- mean(round(predict(lm.out,type="response"))+1 == as.numeric(data.train[,response]))
  return.matrix[1,"accuracy.test"] <- mean(round(predict(lm.out,data.test,type="response"))+1 == as.numeric(data.test[,response]))
}


ret$vec <- return.matrix
ret$lm.ret <- lm.out
return(ret)

}
