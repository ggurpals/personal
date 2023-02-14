#' Test data set for Multivariate Adaptive Regression Splines (MARS) function
#'
#' A data set containing one response variable "y" and ten explanatory variables
#' "x1" to "x10" made to test the "mars" function. This data set is written to run the examples
#' shown in the documentation of MARS and method predict.mars.
#'
#' \itemize{
#' \item y. response variable.
#' \item x1:x10. ten explanatory variables
#' }
#'
#' @docType data
#' @keywords datasets
#' @name marstestdata
#' @usage data(marstestdata)
#' @format A data frame with 100 rows and 11 columns.
#' @author Dr. Brad McNeney \email{brad_mcneney@sfu.ca}


"marstestdata"


#' Test object of class "mars.control" to test "mars" function
#'
#' A list of length 3, which is an object of class "mars.control", entered in control
#' argument of mars function. This particular mars.control object is used in examples
#' shown in documentation of mars function.
#'
#' \itemize{
#' \item Mmax. An integer value representing maximum number of basis functions that a user can accept. In this example object it has a value of 10.
#' \item d. A double value representing an important component used in Generalized Cross Validation (GCV) criterion of mars function. In this example, the value is 3.
#' \item trace. A logical value entered by user. TRUE if they want to see what is happening as the mars function is progressing, FALSE if not. In this example, its FALSE.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name testmc
#' @usage data(testmc)
#' @format A list with three elements. One is a double, one is an integer and one is logical value.
#' @author Dr. Brad McNeney \email{brad_mcneney@sfu.ca}
#'
#'
"testmc"


#' Test object of class "mars" to test summary.mars, print.mars, and plot.mars
#'
#' A list of length 18 which is an object of class "mars". This particular object is created to
#' run the examples written for methods for an object of class "mars". These methods are" summary.mars, plot.mars,
#' print.mars.
#'
#' @docType data
#' @keywords datasets
#' @name testmars
#' @usage data(testmars)
#' @format A list of 18 elements.
#' @author Dr. Brad McNeney \email{brad_mcneney@sfu.ca}
"testmars"


#'Housing Prices in Ames, Iowa - Training data
#'
#'This is a data set with 24 explanatory variables, each describing different aspect of
#'residential homes in Ames, Iowa. The response variable in this dataset is selling price of the house.
#'
#'
#'\itemize{
#'\item LotFrontage. Linear feet of street connected to property.
#'\item LotArea. Lot size in square feet.
#'\item OverallQual. Rates the overall material and finish of the house. 10 - Very Excellent, 9 - Excellent, 8 - Very Good, 7 - Good, 6 - Above Average, 5 - Average, 4 - Below Average, 3 - Fair, 2 - Poor, 1 - Very Poor.
#'\item OverallCond. Rates the overall condition of the house. 10 - Very Excellent, 9 - Excellent, 8 - Very Good, 7 - Good, 6 - Above Average, 5 - Average, 4 - Below Average, 3 - Fair, 2 - Poor, 1 - Very Poor.
#'\item YearBuilt. Original construction date.
#'\item YearRemodAdd. Remodel date (same as construction date if no remodelling or additions)
#'\item MasVnrArea. Masonry veneer area in square feet.
#'\item BsmtUnfSF. Unfinished square feet of basement area.
#'\item TotalBsmtSF. Total square feet of basement area.
#'\item X1stFlrSF. First floor square feet.
#'\item X2ndFlrSF. Second Floor square feet.
#'\item GrLivArea. Above ground living area square feet.
#'\item FullBath. Full bathrooms above grade.
#'\item Halfbath. half baths above grade.
#'\item BedroomAbvGr. Bedrooms above grade.
#'\item KitchenAbvGr. Kitchens above grade.
#'\item TotRmsAbvGrd. Total rooms above grade.
#'\item Fireplaces. Number of fireplaces.
#'\item GarageYrBlt. Year garage was built.
#'\item GarageCars. Size of garage in car capacity.
#'\item GarageArea. Size of garage in square feet.
#'\item WoodDeckSF. Wood deck area in square feet.
#'\item OpenPorchSF. Open porch area in square feet.
#'\item YrSold. Year Sold (YYYY).
#'\item SalePrice. Selling Price.
#'}
#'
#'@docType data
#'@keywords datasets
#'@name housing_prices
#'@usage data(housing_prices)
#'@format A data frame of 1121 rows and 25 columns.
#'@author Dean De Cock
#'@references https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/overview
"housing_prices"


#' Housing prices in Ames, Iowa - test dataset
#'
#'
#' This is a dataset with 24 explanatory variables, each describing different aspect of
#' resedential homes in Ames, Iowa.
#'
#'\itemize{
#'\item LotFrontage. Linear feet of street connected to property.
#'\item LotArea. Lot size in square feet.
#'\item OverallQual. Rates the overall material and finish of the house. 10 - Very Excellent, 9 - Excellent, 8 - Very Good, 7 - Good, 6 - Above Average, 5 - Average, 4 - Below Average, 3 - Fair, 2 - Poor, 1 - Very Poor.
#'\item OverallCond. Rates the overall condition of the house. 10 - Very Excellent, 9 - Excellent, 8 - Very Good, 7 - Good, 6 - Above Average, 5 - Average, 4 - Below Average, 3 - Fair, 2 - Poor, 1 - Very Poor.
#'\item YearBuilt. Original construction date.
#'\item YearRemodAdd. Remodel date (same as construction date if no remodelling or additions)
#'\item MasVnrArea. Masonry veneer area in square feet.
#'\item BsmtUnfSF. Unfinished square feet of basement area.
#'\item TotalBsmtSF. Total square feet of basement area.
#'\item X1stFlrSF. First floor square feet.
#'\item X2ndFlrSF. Second Floor square feet.
#'\item GrLivArea. Above ground living area square feet.
#'\item FullBath. Full bathrooms above grade.
#'\item Halfbath. half baths above grade.
#'\item BedroomAbvGr. Bedrooms above grade.
#'\item KitchenAbvGr. Kitchens above grade.
#'\item TotRmsAbvGrd. Total rooms above grade.
#'\item Fireplaces. Number of fireplaces.
#'\item GarageYrBlt. Year garage was built.
#'\item GarageCars. Size of garage in car capacity.
#'\item GarageArea. Size of garage in square feet.
#'\item WoodDeckSF. Wood deck area in square feet.
#'\item OpenPorchSF. Open porch area in square feet.
#'\item YrSold. Year Sold (YYYY).
#'}
#'
#'@docType data
#'@keywords datasets
#'@name housing_prices_test
#'@usage data(housing_prices_test)
#'@format A data frame of 1147 rows and 24 columns.
#'@author Dean De Cock
#'@references https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/overview
"housing_prices_test"
