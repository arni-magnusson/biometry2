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
#' plot.lm(model)
#' 
#' @importFrom graphics matplot points
#' @export

lm_plot <- function(m) {

	if(length(coef(m)) > 2)
    stop("model should have one x variable")
    
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
