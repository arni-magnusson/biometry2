
#' Make a histogram of x
#'
#' This function generates a histogram of x.
#'
#' @param x column name
#'
#' @return
#' A histogram of x. 
#'
#' @author
#' Einar Petur Jonsson
#'
#' @seealso
#' \link{hist}
#'
#' @examples
#' histeria(mpg)
#'
#' histeria(qsec)
#'
#' @export



histeria<-function(x)
  { out<-hist(x) 
    }
    


