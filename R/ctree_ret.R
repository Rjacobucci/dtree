
ctree_ret <- function(formula, data.train, data.test,class.response, response){

ret <- list()

return.matrix <- matrix(NA,1,8)
colnames(return.matrix) <- c("nodes","nvar","nsplits","misfit.cv","misfit.train","rsq.train","misfit.test","rsq.test")

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
  return.matrix[1,"rsq.train"] <- suppressWarnings((cor(data.train[,response],predict(ctree.out)))**2)
  return.matrix[1,"rsq.test"] <- suppressWarnings((cor(data.test[,response],predict(ctree.out,data.test)))**2)
}else{
  return.matrix[1,"misfit.train"] <- NA
  return.matrix[1,"misfit.test"] <- NA
}


ret$vec <- return.matrix
ret$ctree.ret <- ctree.out
return(ret)

}
