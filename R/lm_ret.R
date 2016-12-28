
lm_ret <- function(formula, data.train, data.test,class.response, response){

ret <- list()

return.matrix <- matrix(NA,1,8)
colnames(return.matrix) <- c("nodes","nvar","nsplits","misfit.cv","misfit.train","rsq.train","misfit.test","rsq.test")


if(class.response == "numeric" | class.response == "integer"){

  lm.out <- lm(formula,data.train)
  p_s <- summary(lm.out)$coefficients[,"Pr(>|t|)"] < 0.05
  return.matrix[1,"nvar"] <- sum(p_s[-1])
}else{
  #lm.out <- glm(formula,family=binomial,data.train)
}



preds <- predict(lm.out)


if(class.response == "numeric" | class.response == "integer"){

  return.matrix[1,"misfit.train"] <- suppressWarnings(mean((data.train[,response] - predict(lm.out))^2)/nrow(data.train))
  return.matrix[1,"misfit.test"] <- suppressWarnings(mean((data.test[,response] -
                                            predict(lm.out,data.test))^2)/nrow(data.test))
  return.matrix[1,"rsq.train"] <- suppressWarnings((cor(data.train[,response],predict(lm.out)))**2)
  return.matrix[1,"rsq.test"] <- suppressWarnings((cor(data.test[,response],predict(lm.out,data.test)))**2)
}else{
  return.matrix[1,"misfit.train"] <- NA
  return.matrix[1,"misfit.test"] <- NA
}


ret$vec <- return.matrix
ret$lm.ret <- lm.out
return(ret)

}
