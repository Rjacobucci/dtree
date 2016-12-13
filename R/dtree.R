#' Main function for creating different types of decision trees
#'
#' @param formula a formula, weight a response to left of ~.
#' @param data Data frame to run models on
#' @param weights Optional weights for each case.
#' @param frac.sub What fraction of data to put into train dataset. 1-frac.sub
#'        is allocated to test dataset.
#'
#' @export
#' @import rpart
#' @import party
#' @import evtree
#' @import rattle
#'


dtree = function(formula,
                 data,
                 weights,
                 perc.sub=.5){

}
