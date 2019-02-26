#' square
#' 
#' This function makes x squared.
#' 
#' @param x a number.
#' 
#' @return
#' A number, x squared.
#' 
#' @seealso
#' \link{Aritmetric}, \code{\link{sqrt}}.
#' 
#' @examples 
#' squared(10)
#' 
#' squared(pi)
#' 
#' @author
#' Hulda Margrét Birkisdóttir
#'
#' @export

squared<- function(x)
{
  x <- x*x
  return(x)
}
