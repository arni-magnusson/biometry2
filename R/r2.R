#' Calculate R2 
#'
#' This function calculates R2, the coefficient of determination, from a linear
#' regression.
#'
#' @param x a numeric vector.
#' @param y a numeric vector.
#' 
#' @return
#' R2 as a number.
#'
#' @author
#' Hafrún Gunnarsdóttir
#'
#' @seealso
#' \code{\link{lm}}
#'
#' @examples
#' r2(cars$speed, cars$dist)
#' 
#' cor(cars$speed, cars$dist)^2
#'
#' summary(lm(dist~speed, data=cars))$r.squared
#'
#' @importFrom stats lm
#'
#' @export

r2 <- function(x,y)
{
  fm <- lm(y~x)
  r2 <- summary(fm)$r.squared
  r2
}
