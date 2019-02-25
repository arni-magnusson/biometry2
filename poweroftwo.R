#' The power of two
#' 
#' This function puts x in the power of two.
#' 
#' @param x a number.
#' 
#' @returns
#' A number, x in the power of two.
#' 
#' @seealso
#' \link{Aritmetric}
#' 
#' @examples 
#' poweroftwo(10)
#' 
#' poweroftwo(pi)
#' 
#' @author
#' Hulda Margrét Birkisdóttir

poweroftwo<- function(x)
{
  x <- x*x
  return(x)
}
