#summary.mars method
#' Summarizing Multivariate Adaptive Regression Splines (MARS) fits
#' @description summary method for class "mars".
#'
#'
#'
#' @param object an obejct of class "mars".
#' @param ... further arguments passed to or from other methods.
#'
#' @return There is no return value for this method.
#' @export
#' @details summary.mars method presents important parts of an object of class "mars" in a very
#' structured way. It presents the call made by user to mars function, coefficients of the fitted model,
#'  basis functions and component of each and every basis function presented in the final model.
#'
#' @examples summary(testmars)
#' @rdname summary.mars
#' @author Samir Arora and Gurpal Singh Tulli
#' @references Friedman, J.H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#' @seealso "Multivariate Adaptive Regression Splines (MARS)" to create an object of class "mars", plot.mars method to plot mars objects, predict.mars method for
#' prediction, and print.mars method to print mars objects.
summary.mars <- function(object, ...){

  #printing out call
  cat("Call:\n")
  print(object$call)

  #printing out coefficients
  cat("\nCoefficients:\n")
  print(object$coefficients)

  #printing basis functions
  cat("\nBasis Functions and their component hinge functions:\n\n")
  cat("\nB0:\n Intercept\n")

  for (i in 2:length(object$Bfuncs)){
    cat(paste0("\n", names(object$coefficient)[i], ":\n"))
    for (j in 1:NROW(object$Bfuncs[[i]])){
      cat(paste0("\nComponent ", j, ":\n"))
      cat(paste0("Sign: ", round(object$Bfuncs[[i]][j,"s"], 4), "\n"))
      cat(paste0("Split variable: ", round(object$Bfuncs[[i]][j,"v"], 4), "\n"))
      cat(paste0("Split point: ", round(object$Bfuncs[[i]][j, "t"], 4), "\n"))
    }
  }

}
