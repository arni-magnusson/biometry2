#' Multiply by Two
#'
#' This function computes and compates de AIC value between two models.
#'
#' @param  lm1 and lm2 are models.
#'
#' @return
#' A number, two times the value of x.
#'
#' @author
#' Laura Aixalà Perelló
#'
#' @examples
#' bestAIC(model1, model2)
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
