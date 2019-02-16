#' Compare AIC
#'
#' This function computes and compares the AIC value between two models.
#'
#' @param m1 is a fitted model.
#' @param m2 is another fitted model.
#'
#' @return
#' Shows a message on the screen, indicating which model has the lower AIC.
#'
#' @author
#' Laura Aixalà Perelló
#'
#' @seealso
#' \code{\link{AIC}}
#'
#' @examples
#' model.wt <- lm(mpg~wt, data=mtcars)
#' model.wt.hp <- lm(mpg~wt+hp, data=mtcars)
#' bestAIC(model.wt, model.wt.hp)
#'
#' @importFrom stats AIC
#'
#' @export

bestAIC<-function(m1, m2)
{
  aic1<-AIC(m1)
  aic2<-AIC(m2)
  
  name1 <- deparse(substitute(m1))
  name2 <- deparse(substitute(m2))
  
  cat(name1, aic1, "", sep="\n")
  cat(name2, aic2, "", sep="\n")
  
  if(aic1<aic2)
  {
    cat("The model", name1, "is better using the AIC comparison method\n")
  }
  else if(aic2<aic1)
  {
    cat("The model", name2, "is better using the AIC comparison method\n")
  }
  else
  {
    cat("The models", name1, "and ", name2, " are equal using the AIC comparison method\n")
  }
}
