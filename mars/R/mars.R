#MARS implementation

#' Multivariate Adaptive Regression Splines (MARS)
#' @description This package is used for flexible regression modelling of high dimensional data.
#' The resulting model is a product of spline basis functions, where the number of basis functions
#' as well as the parameters associated with each one (product degree and knot locations) are automatically
#' determined by the data. These basis functions and their components will be presented in the Bfuncs list, which is
#' a part of the output from "mars" function.
#' @usage mars(formula, data, control)
#'
#' @param formula an object of class "formula":
#' a symbolic description of the model to be fitted.
#' @param data a data frame containing the variables in the model.
#' @param control an object of class "mars.control".
#'
#' @return an object of class "mars". It is a list containing call made to the mars function, formula argument, a vector of response variable, a matrix B, a list
#' Bfuncs (containing final basis functions), and x_names (containing name of explanatory variables), along with other outputs you get from an object of class "lm".
#' @export
#' @details The formula argument takes in a formula same as an "lm" function. A typical formula has the form response ~ terms,
#' where response is the numeric response vector and terms is a series of terms which specifies a linear predictor for response.
#' The data argument is a data frame that consists of one column for response variable and one or more columns for explanatory variables.
#' The control argument takes in an object of class "mars.control". This can be created using function "mars.control". Mars.control function takes
#' in three arguments: Mmax (maximum number of basis functions you want to allow; default value is 2), d (used in Generalized Cross Validation criterion;
#' default value is 3), trace (if user wants to see what is happening as the mars function is fitting the data; default value is FALSE).
#' In the mars function, the whole algorithm can be divided into two sub-algorithms, forward step-wise and backward step-wise algorithm.
#' Forward step-wise algorithm is based on recursive partitioning, with the aim of optimally splitting the covariate space into the maximum number of sub-regions
#' specified by user in control argument. This algorithm is implemented using four loops. The outermost loop iterates over 1 to half of the Mmax.
#' Here we are looping up to half of Mmax because each iteration of this outer loop will add pairs of basis functions, where each basis function represents a
#' sub-region. Within this first loop, based on what iteration number of first loop is going on, second for loop tries to find the optimum basis function to split on.
#' The second for loop iterates from 1 to twice the iteration number of first loop minus 1, since that is the maximum number of basis function in the current model based on iteration
#' number of first loop. The third loop selects the variable to split on within this basis function, and it has to be a variable which has not already been used in this basis function.
#' Lastly, the fourth loop selects the split point (knot) on the variable. The variable and the split point is selected by fitting a model at every iteration and computing a measure of
#' lack of fit using GCV criterion. An optimum variable and split point is the one with the smallest value for GCV criterion. To record all these basis functions, there is a list called
#' Bfuncs and matrix called B. These are updated as the model selects the optimum splits. At each iteration of the first loop, the algorithm adds two basis function in pairs that are identical
#' except that a different side of a mirrored hinge is used for each basis function. A hinge function is defined by a sign (+/-), variable, and a split-point. In this way, MARS is searching
#' over all combinations of existing basis functions, variables in them, and values of each variable. Due to this, forward step-wise algorithm is "greedy" and therefore must be followed by
#' backward step-wise algorithm. Backward step-wise algorithm removes terms one by one, selecting the least effective term at each step until it finds the best submodel. Model subsets are compared
#' using the Generalized Cross Validation (GCV) criterion. This is implemented using two for loops. First loop iterates over all the possible model sizes starting from Mmax + 1. At each iteration of
#' this outer loop we set variable keeping track of lack of fit to infinity. The inner loop iterates over all the model terms in the current model under outer loop, and try removing one basis function
#' at a time, fitting the model and calculating lack of fit value using GCV criterion. If lack of fit value observed is lowest in this iteration of the inner loop, it updates the index set of the current model, and if
#' this LOF value is also the lowest in all iterations so far of the outer loop, it updates the index set of the final model that will be returned as an output.
#'
#' @examples mars(formula = y ~ ., data = marstestdata, control = testmc)
#' @import stats
#' @author Samir Arora and Gurpal Singh Tulli
#' @references Friedman, J.H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#' @seealso summary.mars for summary of mars object, print.mars for printing mars object, plot.mars for plotting mars object, and predict.mars for prediction.
mars <- function(formula, data, control){
  cc <- match.call() #save the call
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1, drop = FALSE]
  fwd <- fwd_stepwise(y, x, control)
  bwd <- bwd_stepwise(fwd, control)

  #structuring the output: just to compare the output for now, need to be updated
  fit_final <- lm(y~.-1, data = data.frame(y = y, bwd$B))
  out <- list(call = cc, formula = formula, y = bwd$y, B = bwd$B, Bfuncs = bwd$Bfuncs,
              x_names = colnames(x))
  output <- c(out, fit_final)
  class(output) <- c("mars", class(fit_final))
  return(output)
}



#fwd_stepwise() function

fwd_stepwise <- function(y, x, mc){

  #Initialize:
  N <- length(y) #sample size
  n <- ncol(x) # number of predictors
  B <- init_B(N, mc$Mmax)
  splits <- data.frame(m=rep(NA,mc$Mmax),v=rep(NA,mc$Mmax),t=rep(NA,mc$Mmax))
  Bfuncs <- vector(mode = "list", length = mc$Mmax + 1)
  Bfuncs[[1]] <- NULL
  #----------------------------------------------------
  #Looping for forward selection
  for(i in 1:(mc$Mmax/2)){ #loop over pairs i
    M <- (2*i)-1 #setting value of M from the value of i
    lof_best <- Inf

    for (m in 1:M){ #choosing a basis function to split on
      var_set <- setdiff(1:n, Bfuncs[[m]][,"v"])

      for(v in var_set){ #select a variable to split on
        tt <- split_points(x[,v], B[,m])

        for (t in tt){
          Bnew <- data.frame(B[,1:M],
                             Btem1 = B[,m]*h(x[,v], +1, t),
                             Btem2 = B[,m]*h(x[,v], -1, t))
          gdat <- data.frame(y=y, Bnew)
          lof <- LOF(y~., gdat, mc)
          if(lof < lof_best){
            lof_best <- lof
            splits[M,] <- c(m,v,t)
          }#end if
        }#end loop over splits
      }#end loop over variables
    }#end loop over basis functions to split

    #update B and Bfuncs
    m <- splits[M,1]; v <- splits[M,2]; t <- splits[M,3]
    Bfuncs[[M+1]] <- rbind(Bfuncs[[m]], c("s" = -1, "v" = v, "t" = t))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[m]], c("s" = 1, "v" = v, "t" = t))
    B[,M+1] <- B[,m]*h(x[,v], -1, t)
    B[,M+2] <- B[,m]*h(x[,v], +1, t)

  }#end for loop over i

  #formatting the output
  attr(y, "names") <- as.character(1:length(y))


  return(list(y=y, B=B, Bfuncs=Bfuncs))
}

#bwd_stepwise function

bwd_stepwise <- function(fwd, mc){

  #initializing J_star and K_star
  J_star <- 2:(mc$Mmax+1)
  K_star <- J_star

  #fitting model from fwd_stepwise to initialize lof_star
  data <- data.frame(y = fwd$y, fwd$B)
  lof_star <- LOF(formula = y~.-1, data = data, mc = mc)

  #looping over all the possible sizes
  for (M in ((mc$Mmax+1):3)){
    L <- K_star
    b <- Inf

    #looping over all the basis function upto current model size
    for (m in L){
      K <- setdiff(L, m)
      dat <- data.frame(y = fwd$y, fwd$B[,K])
      lof <- LOF(formula = y~., data = dat, mc = mc)
      if (lof < b){
        b <- lof
        K_star <- K
      }#end if

      if (lof < lof_star){
        lof_star <- lof
        J_star <- K
      }#end if
    }#end for loop over basis function
  }#end for loop over possible sizes

  #returning output
  output <- list(y = fwd$y, B = fwd$B[,c(1,J_star)], Bfuncs = fwd$Bfuncs[c(1,J_star)])
  return(output)

}#end algorithm


#init_B() function
init_B <- function(N, Mmax){
  output <- matrix(data = NA, nrow = N, ncol = Mmax+1)

  #filling 1's in first column
  for (i in 1:N){
    output[i,1] <- 1
  }

  #assigning names to columns
  names <- vector("character")
  for (j in 0:Mmax){
    names <- c(names, paste0("B",j))
  }

  colnames(output) <- names

  #converting it into dataframe
  output <- as.data.frame(output)
  return(output)
}

#split_points() function
split_points <- function(x_v, B_m){
  out <- x_v[B_m > 0]
  out <- unique(sort(out))
  return(out[-length(out)])
}

##h() - hinge function
h <- function(x, s, t){
  return(pmax(0, s*(x-t)))
}


#GCV criterion function
LOF <- function(formula, data, mc){

  #fitting the model
  fit <- lm(formula = formula, data = data)

  #extracting components of the GCV criterion
  N <- nrow(data)
  RSS <- sum(residuals(fit)^2)
  M <- length(coefficients(fit)) - 1
  d <- mc$d
  CM <- sum(hatvalues(fit))

  #GCV calculation and return output
  output <- RSS*(N/((N - (CM + d*M))^2))
  return(output)
}


###Mars.control object


#constructor
new_mars.control <- function(mc = list()) {
  structure(mc, class = "mars.control")
}

#Validator
validate_mars.control <- function(mc) {

  #checking if Mmax is an integer
  if (all.equal(mc$Mmax, as.integer(mc$Mmax)) != TRUE) {
    warning("Mmax entered is not an integer, trying it to coerce to an integer.")
    mc$Mmax <- as.integer(mc$Mmax)
  }

  #checking if Mmax is even number
  if (mc$Mmax %% 2 != 0){
    warning("Mmax entered is an odd number. Increasing it by 1 to make it even number.")
    mc$Mmax = mc$Mmax + 1
  }

  #checking if d is numeric
  if (!is.numeric(mc$d)){
    stop("Error: d entered is not a numeric value.")
  }

  #checking if trace is logical
  if (!is.logical(mc$trace)){
    stop("Error: trace value entered is not logical.")
  }

  return(mc)

}

#helper
#' Creating objects of class "mars.control"
#' @description This function is used to create an object of class "mars.control".
#'
#' @usage mars.control(Mmax, d, trace)
#'
#' @param Mmax an integer representing maximum number of basis functions allowed by the user, default value is 2
#' @param d smoothing parameter
#' @param trace logical value. if user wants to see what is happening in the mars function, then set it to TRUE. Default value is FALSE
#'
#' @return an object of class "mars.control". a list containing three elements
#' @details this function is used to create an object of class "mars.control", which is later used in the control argument of function "mars".
#' This function perform certain checks to make sure that Mmax is an even integer. If it is not integer, it stops. If it is not even, it adds one
#' to the supplied value to make it even. It also stops if d is not a number and trace is not logical.
#' @export
#' @examples mars.control(Mmax = 10, d = 3, trace = FALSE)
#' @author Samir Arora and Gurpal Singh Tulli
#' @references Friedman, J.H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#' @seealso Multivariate Adaptive Regression Splines (MARS)

mars.control <- function(Mmax = 2, d = 3, trace = FALSE){
  mc <- list(Mmax = Mmax, d = d, trace = trace) #making a list
  mc <- validate_mars.control(mc) #vaidating
  new_mars.control(mc) #making a class
}



