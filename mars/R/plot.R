#plot.mars method

#' Plotting Multivariate Adaptive Regression Splines (MARS) fit
#' @description plot method for class "mars".
#'
#'
#' @param x an object of class "mars".
#' @param ... further arguments passed to or from other methods.
#'
#' @return There is no return value for this function. This function produces a plot for mars x.
#' @export
#' @details plot.mars method produces plots for the input mars object. It is limited to three dimensional plots, therefore
#' it only produces plots for basis functions with one or two variables in them. NO PLOT IS PRODUCED FOR BASIS FUNCTIONS
#' WITH MORE THAN TWO VARIABLES. This method identifies the basis functions with 1 or 2 components from Bfuncs list of mars object.
#' Index for one and two components basis function is stored separately in two different vectors. Then it splits the plot into a matrix of
#' plots based on total number of one and two components basis functions. "plot" function is then used to plot all the basis functions with
#' one variable in it, and "persp" function is used to plot basis functions with two variable components in it. Graphical parameters are reset
#' at the end using an exit handler.
#'
#' @examples plot(testmars)
#' @rdname plot.mars
#' @import graphics
#' @author Samir Arora and Gurpal Singh Tulli.
#' @references Friedman, J.H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#' @seealso "Multivariate Adaptive Regression Splines (MARS)" to create an x of class "mars", summary.mars method to summarise mars xs, predict.mars method for prediction, and print.mars to print mars xs.
plot.mars <- function(x, ...){

  #determine appropriate range of values
  data <- eval(x$call$data)
  bfs <- x$Bfuncs
  tt <- terms(x$formula,data=data)
  tt <- delete.response(tt)
  mf <- model.frame(tt,data)
  mt <- attr(mf, "terms")
  X <- model.matrix(mt, mf)[,-1] # remove intercept

  #identifying basis function that depends on one variable
  one.basis <- which(sapply(bfs, function(x) NROW(x) == 1))

  #identifying basis function that depends on two variable
  two.basis <- which(sapply(bfs, function(x) NROW(x) == 2))


  #determining and setting size for the plot matrix
  total <- length(one.basis) + length(two.basis)
  n <- as.integer(sqrt(total))
  if (n*n < total){
    n <- n+1
  }
  par(mfrow = c(n,n))


  #plotting single variable basis function
  count = 1
  for(i in one.basis){
    vv <- bfs[[i]][1,2]
    xx <- seq(from = min(X[,vv]), to = max(X[,vv]), length = 100)
    bb <- h(xx, bfs[[i]][1, "s"], bfs[[i]][1,"t"])
    plot(x = xx, y = bb, xlab = x$x_names[vv], ylab = "", main = paste0(count, " ", names(x$coefficients[i])))
    count = count + 1
  }

  #plotting two variable basis function
  count = 1
  for(i in two.basis){
    vv1 <- bfs[[i]][1,"v"]
    varname1 <- x$x_names[[vv1]]
    vv2 <- bfs[[i]][2,"v"]
    varname2 <- x$x_names[[vv2]]
    xx <- seq(from=min(X[,vv1]),to=max(X[,vv1]),length=100)
    yy <- seq(from=min(X[,vv2]),to=max(X[,vv2]),length=100)
    ff <- function(x,y) {
      h(x,bfs[[i]][1,"s"],bfs[[i]][1,"t"])*
        h(y,bfs[[i]][2,"s"],bfs[[i]][2,"t"])}
    zz <- outer(xx,yy,FUN=ff)
    persp(xx,yy,zz,xlab=varname1,ylab=varname2,zlab="",
          main=paste0(count, " ", varname1,":",varname2, " (", names(x$coefficients[i]), ")"),theta=-30,phi=30,
          col="lightblue",lwd=.1)
    count = count + 1
  }

  #putting function call on top
  mtext(x$call, side = 3, line = -2, outer = TRUE)

  #resetting the old graphical parameters
  opar <- par(mfrow=c(n,n),mar=c(2,2,2,2)); on.exit(par(opar))
}

