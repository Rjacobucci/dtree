
ctree_ret <- function(formula, data.train, data.test,class.response, response){

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
ctree.out <- party::ctree(formula,data.train)


#min.error <- which(min(cp[,"xerror"]) == cp[,"xerror"])[1]

nodes <- ctree.out@get_where()
return.matrix[1,"nsplits"] <- max(nodes) - length(unique(nodes))
#return.matrix[1,"fit.cv"] <- cp[min.error,"xerror"]

preds <- predict(ctree.out)



#depth(ctree.out$node)


node.try <- subset(1:max(nodes), !(1:max(nodes) %in% unique(nodes)))

var.name <- rep(NA,length(node.try))

if(length(var.name) > 0){
  for(i in 1:length(var.name)){
    var.name[i] <- party::nodes(ctree.out,node.try[i])[[1]]$psplit$variableName
  }
}





return.matrix[1,"nvar"] <- length(unique(var.name))

return.matrix[1,"nodes"] <- length(unique(nodes))



if(class.response == "numeric" | class.response == "integer"){

  return.matrix[1,"misfit.train"] <- suppressWarnings(mean((data.train[,response] - predict(ctree.out))^2)/nrow(data.train))
  return.matrix[1,"misfit.test"] <- suppressWarnings(mean((data.test[,response] -
                                            predict(ctree.out,data.test))^2)/nrow(data.test))

  if(sd(predict(ctree.out)) == 0){
    return.matrix[1,"rsq.train"] <- 0
  }else{
    return.matrix[1,"rsq.train"] <- suppressWarnings((cor(data.train[,response],predict(ctree.out)))**2)
  }

  if(sd(predict(ctree.out,data.test))==0){
    return.matrix[1,"rsq.test"] <- 0
  }else{
    return.matrix[1,"rsq.test"] <- suppressWarnings((cor(data.test[,response],predict(ctree.out,data.test)))**2)
  }

}else{
  return.matrix[1,"accuracy.train"] <- mean(as.numeric(predict(ctree.out)) == as.numeric(data.train[,response]))
  return.matrix[1,"accuracy.test"] <- mean(as.numeric(predict(ctree.out,data.test)) == as.numeric(data.test[,response]))
}


ret$vec <- return.matrix
ret$ctree.ret <- ctree.out
return(ret)

}
