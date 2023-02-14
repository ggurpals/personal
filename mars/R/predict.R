#predict,mars method

#' Predict method for  Multivariate Adaptive Regression Splines (MARS) fits
#' @description predicted values based on Multivariate Adaptive Regression Splines (MARS) objects.
#'
#'
#' @param object an object of class "mars".
#' @param newdata an optional data frame in which to look for variables with which to predict.
#' If omitted, the fitted values are used.
#' @param ... further arguments passed to or from other methods.
#'
#' @return produces a vector of predictions.
#' @export
#' @details predict.mars produces predicted values. It takes in as argument an object of class "mars", and use the
#' fit from this object to produce predictions on the test data provided in the newdata argument. If newdata is NULL,
#' prediction will be done on the training data (the one used to create object argument). There will be no need to create
#' a new B matrix as B matrix from object argument will be used. If the newdata argument is not NULL, then prediction will be
#' done on the new dataset, and new B matrix will be constructed for this particular dataset using Bfuncs list from fitted model.
#'
#'
#' @examples
#' #if newdata is NULL
#' predict(object = testmars)
#'
#' #if newdata is not NULL
#' predict(object = testmars, newdata = marstestdata)
#' @rdname predict.mars
#'
#' @author Samir Arora and Gurpal Singh Tulli
#' @references Friedman, J.H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#' @seealso "Multivariate Adaptive Regression Splines (MARS)" to create an object of class "mars", plot.mars method to plot mars objects, print.mars method to print mars objects and summary.mars method to summarize mars objects.
predict.mars <- function(object,newdata, ...) {
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}


make_B <- function(X, Bfuncs){

  #making an empty B matrix
  B <- init_B(N = nrow(X), Mmax = length(Bfuncs) - 1)

  for (i in 1:nrow(X)){
    for (j in 2:length(Bfuncs)){
      output = 1
      for (k in 1:nrow(Bfuncs[[j]])){
        s <- Bfuncs[[j]][k,1]
        v <- Bfuncs[[j]][k,2]
        t <- Bfuncs[[j]][k,3]
        output <- output*pmax(0,s*(X[i,v]-t))
      }
      B[i,j] <- output
      output <- 1
    }
  }
  return(as.matrix(B))
}

