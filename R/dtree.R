#' Main function for creating different types of decision trees
#'
#' @param formula a formula, weight a response to left of ~.
#' @param data Data frame to run models on
#' @param weights Optional weights for each case.
#' @param frac.sub What fraction of data to put into train dataset. 1-frac.sub
#'        is allocated to test dataset.
#' @param prune Whether to prune rpart tree
#'
#' @export
#' @import rpart
#' @import party
#' @import evtree
#' @import caret
#'


dtree = function(formula,
                 data,
                 weights,
                 perc.sub=.5,
                 prune=TRUE){

  ret <- list()

  complexity <- matrix(NA,3,6)
  rownames(complexity) <- c("rpart","evtree","ctree")
  colnames(complexity) <- c("nodes","nvar","nsplits","fit.cv","fit.train","fit.test")


  ids <- sample(nrow(data),nrow(data)*perc.sub)
  data.train <- data[ids,]
  data.test <- data[-ids,]



  # parse out the response variable name
  getResponseFormula <- function (object)
  {
    form <- formula(object)
    if (!(inherits(form, "formula") && (length(form) == 3))) {
      stop("'form' must be a two-sided formula")
    }
    eval(parse(text = paste("~", deparse(form[[2]]))))
  }

  response <- attr(terms(getResponseFormula(formula)),"term.labels")
  # ------------------------------------------------------------------


  class.response <- class(data.train[,response])



  # ------------------------------------------------------------------

  # Rpart

  # ----------------------------------------------------------------


  rpart.out <- rpart(formula,data=data.train)

  if(class.response == "numeric" | class.response == "integer"){
    pred1 <- predict(rpart.out)
    #complexity["rpart","fit.train"] <- cor(pred1,data.train[,response])**2
  }else{
    #complexity["rpart","fit.train"] <- NA
  }




  cp <- rpart.out$cptable
  min.error <- which(min(cp[,"xerror"]) == cp[,"xerror"])[1]
  complexity["rpart","nsplits"] <- cp[min.error,"nsplit"]
  complexity["rpart","fit.cv"] <- cp[min.error,"xerror"]
  complexity["rpart","fit.train"] <- cp[min.error,"rel error"]

  if(prune == TRUE){
    rpart.ret <- prune.rpart(rpart.out,cp[min.error,"CP"])
  }else{
    rpart.ret <- rpart.out
  }


  vars <- rpart.ret$frame[,"var"]
  vars2 <- vars[vars != "<leaf>"]
  complexity["rpart","nvar"] <- length(unique(vars2))

  complexity["rpart","nodes"] <- length(vars[vars == "<leaf>"])

  if(class.response == "numeric" | class.response == "integer"){

    complexity["rpart","fit.test"] <- cor(predict(rpart.ret,data.test),data.test[,response])**2
  }else{
    complexity["rpart","fit.test"] <- NA
  }

  ret$rpart.out <- rpart.ret

  #----------------------------------------------------

  # Evtree

  # ---------------------------------------------------

  evtree.out <- evtree(formula,data=data.train)
  ret$evtree.out <- evtree.out


  # ----------------------------------------------------

  # Ctree

  # ----------------------------------------------------

  ctree.out <- ctree(formula,data=data.train)

  ret$ctree.out <- ctree.out

  ret$response.type <- class.response
  ret$complexity <- complexity
  ret$call <- match.call()
  class(ret) <- "dtree"
  return(ret)

}
