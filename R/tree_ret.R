
tree_ret <- function(formula, data.train, data.test, prune, class.response, response){

ret <- list()

return.matrix <- matrix(NA,1,6)
colnames(return.matrix) <- c("nodes","nvar","nsplits","fit.cv","fit.train","fit.test")

tree1 <- tree::tree(formula,data.train)
cv1 <- tree::cv.tree(tree1)

min.loc <- which(min(cv1$dev) == cv1$dev)
return.matrix[1,"nodes"] <- cv1$size[min.loc]

pruned.tree <- tree::prune.tree(tree1,cv1$k[min.loc])

#return.matrix[1,"nsplits"] <- cp[min.error,"nsplit"]
return.matrix[1,"fit.cv"] <- cv1$dev[min.loc]/nrow(data.train)
#return.matrix[1,"fit.train"] <- cp[min.error,"rel error"]

#if(prune == TRUE){
#  rpart.ret <- prune.rpart(rpart.out,cp[min.error,"CP"])
#}else{
#  rpart.ret <- rpart.out
#}


#vars <- rpart.ret$frame[,"var"]
#vars2 <- vars[vars != "<leaf>"]
#return.matrix[1,"nvar"] <- length(unique(vars2))



if(class.response == "numeric" | class.response == "integer"){

  return.matrix[1,"fit.test"] <- cor(predict(tree1,data.test),data.test[,response])**2
}else{
  return.matrix[1,"fit.test"] <- NA
}


ret$vec <- return.matrix
ret$rpart.ret <- tree1
return(ret)

}
