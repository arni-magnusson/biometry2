#' Population parameters from lifetable
#'
#' This function gives various point estimates of current population growth.
#'
#' @param x A 3 column table containing going from left or right age, abundance at each life stage whether relative or absolute and offspring produced at lifestage..
#'
#' @return
#' R0,Tc,r,birth-ex, current population growth, average generation time, intrinsic population growth and life expectancy at birth (if age 0 is at the top of a given table and life stages are seperated by a constant interval, e.g. 1 year) all expressed in the given time unit..
#'
#' @author
#' Ölvir Styrmisson
#'
#' @seealso
#' \href{https://cran.r-project.org/web/packages/MortalityTables/index.html}{MortailityTables}
#'  \href{https://cran.r-project.org/web/packages/LifeTables/index.html}{LifeTables}
#'
#' @examples
#' poppar(cbind((0:5),sort(runif(6,0,10),decreasing=TRUE),sort(c(0,0,0,runif(3,0,12)))))
#'
#' poppar(cbind((0:5),sort(runif(6,0,10),decreasing=TRUE),sort(c(0,0,0,runif(3,0,12)))),20)
#'
#' @export







poppar <- function(x,y){
  if(missing(y)) {
    R0 = sum(x[,3])/x[1,2]
    Tc = sum(x[,1]*x[,2]*x[,3])/sum(x[,2]*x[,3])
    r = (R0*sum(x[,1:2]))/sum(x[,1:2])
    
    lx = (x[,2]/(x[1,2]))
    Tx = sum(lx[1:length(lx)-1]+lx[2:length(lx)])/2
    
    ex = (x[ceiling((Tx/1))+1,1]*(((Tx/1))-floor((Tx/1))))+(x[floor((Tx/1))+1,1]*(ceiling((Tx/1))-((Tx/1))))
    
    out=(c(R0,Tc,r,ex))
    names(out)=c("R0","Tc","r","birth-ex")
    warning('Carrying capacity not supplied, density independence assumed.')
    return(out)
  } else {
    R0 = sum(x[,3])/x[1,2]
    Tc = sum(x[,1]*x[,2]*x[,3])/sum(x[,2]*x[,3])
    r = (R0*sum(x[,1:2])*y)/((y-sum(x[,1:2]))*sum(x[,1:2])) 
    
    
    lx = (x[,2]/(x[1,2]))
    Tx = sum(lx[1:length(lx)-1]+lx[2:length(lx)])/2
    ex = x[round((Tx/1))+1,1]
    
    out=(c(R0,Tc,r,ex))
    names(out)=c("R0","Tc","r","birth-ex")
    out
  }
}