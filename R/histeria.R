#' Make a histogram of x
#'
#' This function generates a histogram of x.
#'
#' @param x a numeric vector.
#'
#' @return
#' Plots a histogram of x. 
#'
#' @author
#' Einar Petur Jonsson
#'
#' @seealso
#' \link{hist}
#'
#' @examples
#' histeria(mtcars$mpg)
#'
#' histeria(mtcars$qsec)
#'
#' @importFrom graphics hist
#'
#' @export



histeria<-function(x)
{
  out<-hist(x) 
}
    


