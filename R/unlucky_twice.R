#' Unlucky Twice
#' 
#' The unlucky_twice function enables you to multiply by two but in 50% of the cases the output will be "you are unlucky"
#' 
#' @usage unlucky_twice(x)
#' 
#' @param x is a number
#' 
#' @details This function will only work for numerical arguments. This package neither brings luck nor alters it in any way. Either you are lucky or you aren't!
#' 
#' @return x multiplied by 2 (if you're lucky!)
#' 
#' @author Jóhannes Bjarki Urbancic Tómasson
#' 
#' @seealso /link{Arithmetic}
#' /link {ifelse}
#' 
#' @examples unlucky_twice(4)
#' unlucky_twice(-3)
#' unlucky_twice((14+9*8)+3)
#' unlucky_twice(pi)
#' 
#' @importFrom stats rnorm
#' 
#' @export

unlucky_twice <- function(x)
  {
  if(rnorm(1) > 0) { y = x*2 } else { y = "You are unlucky" }
    return(y)
}
