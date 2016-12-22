#' Summary results from dtree.
#'
#' @param object An object from dtree.
#' @param ... Other arguments.
#' @export


summary.dtree <- function(object,...){

  ret <- list(complexity = object$complexity,
              response.type = object$response.type,
              call = object$call)



  class(ret) <- "summary.dtree"
  print(ret)
}
