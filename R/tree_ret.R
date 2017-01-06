
tree_ret <- function(formula, data.train, data.test, prune, class.response, response){

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

tree1 <- tree::tree(formula,data.train)
cv1 <- tree::cv.tree(tree1)

min.loc <- which(min(cv1$dev) == cv1$dev)
return.matrix[1,"nodes"] <- cv1$size[min.loc]

pruned.tree <- tree::prune.tree(tree1,best=cv1$size[min.loc])

return.matrix[1,"nodes"] <- cv1$size[min.loc]

#return.matrix[1,"nsplits"] <- cp[min.error,"nsplit"]
#return.matrix[1,"fit.cv"] <- cv1$dev[min.loc]/nrow(data.train)
#return.matrix[1,"fit.train"] <- cp[min.error,"rel error"]

#if(prune == TRUE){
#  rpart.ret <- prune.rpart(rpart.out,cp[min.error,"CP"])
#}else{
#  rpart.ret <- rpart.out
#}

uniq <- unique(pruned.tree$frame[,"var"])
return.matrix[1,"nvar"] <- length(uniq[uniq != "<leaf>"])

return.matrix[1,"nsplits"] <- length(pruned.tree$frame[,"var"]) - cv1$size[min.loc]
#vars <- rpart.ret$frame[,"var"]
#vars2 <- vars[vars != "<leaf>"]
#return.matrix[1,"nvar"] <- length(unique(vars2))



if(class.response == "numeric" | class.response == "integer"){

  #return.matrix[1,"misfit.cv"] <- cp[min.error,"xerror"]
  return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(pruned.tree))^2)/nrow(data.train)
  return.matrix[1,"misfit.test"] <- mean((data.test[,response] -
                                            predict(pruned.tree,data.test))^2)/nrow(data.test)
  return.matrix[1,"rsq.train"] <- (cor(data.train[,response],predict(pruned.tree)))**2
  return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(pruned.tree,data.test)))**2
}else{
  #return.matrix[1,"accuracy.cv"] <- cp[min.error,"xerror"]
  return.matrix[1,"accuracy.train"] <- mean(round(predict(pruned.tree)[,2])+1 == as.numeric(data.train[,response]))
  return.matrix[1,"accuracy.test"] <- mean(round(predict(pruned.tree ,data.test)[,2])+1 == as.numeric(data.test[,response]))
}


ret$vec <- return.matrix
ret$tree.ret <- pruned.tree
return(ret)

}
