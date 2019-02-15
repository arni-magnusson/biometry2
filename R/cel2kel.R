#' Celsius to Kelvin
#' 
#' Converts temperatures from Celsius degrees to Kelvin scale.
#'
#' @param x Temperature in Celsius degrees.
#' 
#' @returns
#' Kelvin scale value, 273.15 added to Celsius degrees.
#' 
#' @author
#' Vigdís Freyja Helmutsdóttir, MSc. student at the University of Iceland.
#' 
#' @seealso
#' \link{Arithmetic}
#' 
#' @examples
#' cel2kel(70)
#' cel2kel(-4)
#'
#' @export

cel2kel <- function(x) {
    kel <- (x + 273.15)
    kel
}
