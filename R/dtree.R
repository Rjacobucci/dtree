#' Main function for creating different types of decision trees
#'
#' @param formula a formula, with a response to left of ~.
#' @param data Data frame to run models on
#' @param methods Which tree methods to use. Defaults:
#'        lm, rpart, ctree, evtree. Also can use "rf" for random forests
#' @param samp.method Sampling method. Refer to caret package trainControl()
#'        documentation. Default is repeated cross-validation. Other options
#'        include "cv" and "boot".
#' @param tuneLength Number of tuning parameters to try. Applies to train().
#'        Can also be specified as a vector, with order corresponding to the
#'        order specified in the methods argument.
#' @param subset Whether to split dataset into training and test sets
#' @param perc.sub What fraction of data to put into train dataset. 1-frac.sub
#'        is allocated to test dataset. Defaults to 0.75
#' @param prune Whether to prune rpart tree
#' @param weights Optional weights for each case.
#' @param verbose
#'
#'
#' @importFrom stats cor fitted lm predict terms glm binomial sd
#' @import party rpart evtree caret
#' @export
#'
#' @examples
#' # continuous outcome
#' #library(MASS) # for boston data
#' #data(Boston)
#' #out <- dtree(medv ~., data=Boston,methods=c("lm","rpart","ctree"))
#' #summary(out)
#'# plot(out$rpart.out)
#'
#' # categorical outcome
#' #library(ISLR)
#' #data(Default)
#'
#' #out <- dtree(default ~ ., data=Default,methods=c("lm","rpart"))
#' #summary(out)


dtree = function(formula,
                 data,
                 methods=c("lm","rpart","tree","ctree","evtree"),
                 samp.method="repeatedcv",
                 tuneLength=3,
                 subset=FALSE,
                 perc.sub=.75,
                 prune=TRUE,
                 weights,
                 verbose=TRUE){

  ret <- list()
  if(length(tuneLength)==1){
    tune.rpart <- tune.ctree <- tune.evtree <- tune.rf <- tuneLength
  }else{
    if(any(methods=="rpart")){
      tune.rpart <- tuneLength[methods=="rpart"]
    }
    if(any(methods=="ctree")){
      tune.ctree <- tuneLength[methods=="ctree"]
    }
    if(any(methods=="evtree")){
      tune.evtree <- tuneLength[methods=="evtree"]
    }
    if(any(methods=="rf")){
      tune.rf <- tuneLength[methods=="rf"]
    }
  }

  if(subset==TRUE){
    ids <- sample(nrow(data),nrow(data)*perc.sub)
    data.train <- data[ids,]
    data.test <- data[-ids,]
  }else{
    data.train <- data
    data.test <- data
  }


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


  if(class.response == "numeric" | class.response == "integer"){
    return.matrix <- matrix(NA,length(methods),7)
    rownames(return.matrix) <- methods
    colnames(return.matrix) <- c("nodes","nvar","nsplits","rmse.samp",
                                 "rsq.samp","rmse.test","rsq.test")
    Metric="RMSE"
  }else{
    return.matrix <- matrix(NA,length(methods),7)
    rownames(return.matrix) <- methods
    colnames(return.matrix) <- c("nodes","nvar","nsplits","auc.samp",
                                 "accuracy.samp","auc.test","accuracy.test")

    Metric="ROC"

  }


  # -----------------------------------------------------------------

  # Linear (Logistic) Regression

  # -----------------------------------------------------------------

  if(any(methods=="lm")){
    if(verbose==TRUE) cat("Currently running linear regression\n")
    ret0 <- lm_ret(formula, data.train,data.test,samp.method,tuneLength=1,subset, class.response,response,Metric)
    return.matrix["lm",] <- ret0$vec
    ret$lm.out <- ret0$rpart.ret
    ret$lm.train <- ret0$lm.train



  }



  # ------------------------------------------------------------------

  # Rpart

  # ----------------------------------------------------------------


  if(any(methods=="rpart")){
    if(verbose==TRUE) cat("Currently running",tune.rpart, "rpart models\n")
  ret1 <- rpart_ret(formula, data.train,data.test,samp.method,tuneLength=tune.rpart,subset, class.response,response,Metric)
  return.matrix["rpart",] <- ret1$vec
  ret$rpart.out <- ret1$rpart.ret
  ret$rpart.train <- ret1$rpart.train
  ret$rpart.splits <- ret1$return.splits


  }

  # ----------------------------------------------------

  # Ctree

  # ----------------------------------------------------
  if(any(methods == "ctree")){
    if(verbose==TRUE) cat("Currently running",tune.ctree, "ctree models\n")
    ret3 <- ctree_ret(formula, data.train,data.test,samp.method,tuneLength=tune.ctree,subset, class.response,response,Metric)
    return.matrix["ctree",] <- ret3$vec
    ret$ctree.out <- ret3$ctree.ret
    ret$ctree.train <- ret3$ctree.train
    ret$ctree.splits <- ret3$return.splits
  }


  #----------------------------------------------------

  # Evtree

  # ---------------------------------------------------

  if(any(methods == "evtree")){
    if(verbose==TRUE) cat("Currently running",tune.evtree, "evtree models\n")
    ret4 <- evtree_ret(formula, data.train,data.test,samp.method,tuneLength=tune.evtree,subset, class.response,response,Metric)
    return.matrix["evtree",] <- ret4$vec
    ret$evtree.out <- ret4$evtree.ret
    ret$evtree.train <- ret4$evtree.train
    ret$evtree.splits <- ret4$return.splits
  }

  #----------------------------------------------------

  # Random Forests

  # ---------------------------------------------------

  if(any(methods == "rf")){
    if(verbose==TRUE) cat("Currently running",tune.rf, "random forest models\n")
    ret5 <- rf_ret(formula, data.train,data.test,samp.method,tuneLength=tune.rf,subset, class.response,response,Metric)
    return.matrix["rf",] <- ret5$vec
    ret$rf.out <- ret5$rf.ret
    ret$rf.train <- ret5$rf.train
  }



  ret$response.type <- class.response
  ret$return.matrix <- return.matrix
  ret$call <- match.call()
  ret$subset <- subset
  class(ret) <- "dtree"
  return(ret)

}
