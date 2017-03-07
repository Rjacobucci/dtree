#' Summary results from dtree.
#'
#' @param object An object from dtree.
#' @param ... Other arguments.
#' @method summary dtree
#' @export


summary.dtree <- function(object,...){

  ret <- list(results = object$return.matrix,
              response.type = object$response.type,
              call = object$call)


  class(ret) <- "summary.dtree"
  print(ret)
}
