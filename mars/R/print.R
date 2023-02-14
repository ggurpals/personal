#print,mars method
#' Print method for Multivariate Adaptive Regression Splines (MARS) fits
#' @description print method for class "mars"
#'
#'
#' @param x an x of class "mars".
#' @param ... further arguments passed to or from other methods.
#'
#'
#' @return There is no return value.
#' @export
#' @details print.mars method prints two important pieces of information about the x of class "mars".
#' First is the call made by user to mars function, second is the coefficients of the fitted model in the x argument.
#'
#'
#' @examples print(testmars)
#' @rdname print.mars
#' @author Samir Arora and Gurpal Singh Tulli
#' @references Friedman, J.H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#' @seealso "Multivariate Adaptive Regression Splines (MARS)" to create an x of class "mars", plot.mars method to plot mars xs, predict.mars method to prediction,
#' and summary.mars method to summarize mars xs.
print.mars <- function(x, ...){

  #print call
  cat("Call:\n")
  print(x$call)

  #print coefficients
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

