#' Main function for creating different types of decision trees
#'
#' @param formula a formula, weight a response to left of ~.
#' @param data Data frame to run models on
#' @param methods Which tree methods to use. Defaults:
#'        lm, rpart, tree, ctree, evtree. Also can use "rf" for random forests
#' @param weights Optional weights for each case.
#' @param perc.sub What fraction of data to put into train dataset. 1-frac.sub
#'        is allocated to test dataset. Defaults to 0.75
#' @param prune Whether to prune rpart tree
#'
#'
#' @importFrom stats cor fitted lm predict terms glm binomial sd
#' @import party rpart evtree caret
#' @export
#'
#' @examples
#' # continuous outcome
#' library(MASS) # for boston data
#' data(Boston)
#' out <- dtree(medv ~., data=Boston,methods=c("lm","rpart","ctree"))
#' summary(out)
#' plot(out$rpart.out)
#'
#' # categorical outcome
#' library(ISLR)
#' data(Default)
#'
#' out <- dtree(default ~ ., data=Default,methods=c("lm","rpart"))
#' summary(out)


dtree = function(formula,
                 data,
                 methods=c("lm","rpart","tree","ctree","evtree"),
                 weights,
                 perc.sub=.75,
                 prune=TRUE){

  ret <- list()

  ids <- sample(nrow(data),nrow(data)*perc.sub)
  data.train <- data[ids,]
  data.test <- data[-ids,]



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
    return.matrix <- matrix(NA,length(methods),8)
    rownames(return.matrix) <- methods
    colnames(return.matrix) <- c("nodes","nvar","nsplits","misfit.cv",
                                 "misfit.train","rsq.train","misfit.test","rsq.test")
  }else{
    return.matrix <- matrix(NA,length(methods),6)
    rownames(return.matrix) <- methods
    colnames(return.matrix) <- c("nodes","nvar","nsplits","accuracy.cv",
                                 "accuracy.train","accuracy.test")
  }


  # -----------------------------------------------------------------

  # Linear (Logistic) Regression

  # -----------------------------------------------------------------

  if(any(methods=="lm")){

    ret0 <- lm_ret(formula, data.train,data.test,class.response,response)
    return.matrix["lm",] <- ret0$vec
    ret$lm.out <- ret0$lm.ret



  }



  # ------------------------------------------------------------------

  # Rpart

  # ----------------------------------------------------------------


  if(any(methods=="rpart")){

  ret1 <- rpart_ret(formula, data.train,data.test, prune, class.response,response)
  return.matrix["rpart",] <- ret1$vec
  ret$rpart.out <- ret1$rpart.ret



  }

  # --------------------------------------------------

  # Tree

  # --------------------------------------------------

  if(any(methods == "tree")){

    ret1 <- tree_ret(formula, data.train,data.test, prune, class.response,response)
    return.matrix["tree",] <- ret1$vec
    ret$tree.out <- ret1$tree.ret



  }

  # ----------------------------------------------------

  # Ctree

  # ----------------------------------------------------

  if(any(methods == "ctree")){
    ret3 <- ctree_ret(formula, data.train,data.test, class.response,response)
    return.matrix["ctree",] <- ret3$vec
    ret$ctree.out <- ret3$ctree.ret
  }


  #----------------------------------------------------

  # Evtree

  # ---------------------------------------------------

  if(any(methods == "evtree")){
    ret4 <- evtree_ret(formula, data.train,data.test, class.response,response)
    return.matrix["evtree",] <- ret4$vec
    ret$evtree.out <- ret4$evtree.ret
  }

  #----------------------------------------------------

  # Random Forests

  # ---------------------------------------------------

  if(any(methods == "rf")){
    ret5 <- rf_ret(formula, data.train,data.test, class.response,response)
    return.matrix["rf",] <- ret5$vec
    ret$rf.out <- ret5$rf.ret
  }



  ret$response.type <- class.response
  ret$return.matrix <- return.matrix
  ret$call <- match.call()
  class(ret) <- "dtree"
  return(ret)

}
