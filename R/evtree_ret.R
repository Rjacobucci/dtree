
evtree_ret <- function(formula, data.train, data.test,class.response, response){

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
evtree.out <- try(evtree::evtree(formula,data.train),silent=TRUE)

if(inherits(evtree.out, "try-error")){
  return.matrix <- NA
}else{






#min.error <- which(min(cp[,"xerror"]) == cp[,"xerror"])[1]
return.matrix[1,"nsplits"] <- max(fitted(evtree.out)[,1]) - length(unique(fitted(evtree.out)[,1]))
#return.matrix[1,"fit.cv"] <- cp[min.error,"xerror"]



#depth(evtree.out$node)

ret.obj <- as.list(evtree.out$node)
len <- length(ret.obj)

vars <- rep(NA,len)
for(i in 1:len){

  if(is.null(ret.obj[[i]]$split$varid)==FALSE){
    vars[i] <- ret.obj[[i]]$split$varid
  }else{
    vars[i] <- NA
  }
}

vars2 <- vars[is.na(vars)==FALSE]
vars3 <- length(unique(vars2))


#attributes(evtree.out)


return.matrix[1,"nvar"] <- vars3

return.matrix[1,"nodes"] <- length(unique(fitted(evtree.out)[,1]))



if(class.response == "numeric" | class.response == "integer"){

  return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(evtree.out))^2)/nrow(data.train)
  return.matrix[1,"misfit.test"] <- mean((data.test[,response] -
                                            predict(evtree.out,data.test))^2)/nrow(data.test)
  return.matrix[1,"rsq.train"] <- (cor(data.train[,response],predict(evtree.out)))**2
  return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(evtree.out,data.test)))**2
}else{
  return.matrix[1,"accuracy.train"] <- mean(as.numeric(predict(evtree.out)) == as.numeric(data.train[,response]))
  return.matrix[1,"accuracy.test"] <- mean(as.numeric(predict(evtree.out,data.test)) == as.numeric(data.test[,response]))
}

}

ret$vec <- return.matrix
ret$evtree.ret <- evtree.out
return(ret)

}
