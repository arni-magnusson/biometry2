#' Calculate R2 
#'
#'This function calculates R2 from a linear regression
#'
#' @param x a number.
#' @param y a numeric vector.
#' 
#' @return
#' A number
#'
#' @author
#' Hafrún Gunnarsdóttir
#'
#' @seealso
#' \link{Arithmetic}
#'
#' @examples
#' r2(10,5)
#' 
#' @importFrom stats lm
#'
#'
#' @export

r2 <- function(x,y)
{
  fm <- lm(y~x)
  r2 <- summary(fm)$r.squared
  r2
}
