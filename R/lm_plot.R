#' LM Plot
#'
#' Plot scatterplot with linear regression line.
#'
#' @author Clara Jégousse
#'
#' @param m linear model
#' @return Scatterplot and lm regression line as well as the equation
#' 
#' @examples
#' model <- lm(dist~speed, data=cars)
#' lm_plot(model)
#' 
#' @importFrom graphics abline mtext par plot
#' @importFrom stats model.frame
#'
#' @export

lm_plot <- function(m) {
	par(pty="s") # ratio = 1
	plot(m$model[,1]~m$model[,2],
		xlab = names(model.frame(m))[1], 
 		ylab = names(model.frame(m))[2])
	abline(m) # show regression line

	# extract coefficients 
	coef <- round(coef(m), 3) 
	mtext(bquote(y == .(coef[2]) * x + .(coef[1])), 
	adj=1, padj=0) 
}
