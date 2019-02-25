#' Population parameters from lifetable
#'
#' This function gives various point estimates of current population growth.
#'
#' @param pop A 3 column table containing age, abundance at each life stage, whether relative or absolute, and offspring produced at lifestage.
#' 
#' @param K Estimated carrying capacity of the envirorment where the represented population lives.
#'
#' @details K, carrying capacity, is optional. If it is not provided it is assumed the observed growth is not contrained by the population density.
#'
#' @return
#' R0,Tc,r,birth-ex: Current population growth, average generation time, intrinsic population growth and life expectancy at birth (if age 0 is at the top of a given table and life stages are seperated by a constant interval, e.g. 1 year) all expressed in the given time unit.
#'
#' @note 
#' Calculation of estimated parameters:
#' \eqn{R0 = \sum(lx x bx)}{R0 = Σ(lx x bx)}
#' \eqn{Gc = \sum(lx x bx x age) / R0}{Gc = Σ(lx x bx x age) / R0}
#' \eqn{r(1) =  ln(R0) / Gc}
#' \eqn{r(2) = ((dN/dt) x K) / (N x (K - N))}
#' \eqn{ex = Tx / lx}
#' 
#' Where R0 is the basic reproduction rate, lx proportion of living individuals at age x compared to age 0, bx numbers of offspring produced by individual at age x, Gc the average generation length, age the age in a given time unit, r the instantaneous multiplication rate, defined without or with K, Tx is a sum of proportion of individuals surviving at the mindpoint of present life stage (here age 0) and future life stages and ex is the execpted life length at age 0.
#' 
#' Some other needed values:
#' \eqn{lx = Ni/N0}
#' \eqn{bx = Noi/Ni}
#' \eqn{Lx = (lxi + lxi+1)/2}
#' \eqn{Tx = \sum Lx}{Tx = Σ Lx}
#' 
#' Ni is the number of individuals at a particular live stage, Noi is the offspring produced at a particular live stage and Lx is the proportion of individual survivial for midpoints of all livestages. 
#'
#' @author
#' Ölvir Styrmisson
#' 
#' @references 
#' Elzinga, C. Laboratory Manual for Honors Organismal Biology, Michican State University. Retrieved from https://msu.edu/course/lbs/158h/manual.html, February 2019
#' Krebs, C. J. (2009). Ecology. San Francisco, Pearson.
#'
#' @seealso
#' \href{https://cran.r-project.org/package=MortalityTables/}{MortailityTables}
#'  \href{https://cran.r-project.org/package=LifeTables/}{LifeTables}
#'
#' @examples
#' pop = (cbind((0:5),sort(runif(6,0,10),decreasing=TRUE),sort(c(0,0,0,runif(3,0,12)))))
#' poppar(pop)
#' poppar(pop, 50)
#'
#' @export

poppar <- function(pop,K){
  if(missing(K)) {
    lx = (pop[,2]/(pop[1,2]))
    bx = (pop[,3]/pop[,2])
    R0 = sum(lx*bx)
    Gc = sum(lx*bx*pop[,1])/R0
    r = log(R0)/Gc
    
    Tx = sum(lx[1:length(lx)-1]+lx[2:length(lx)])/2
    ex = (pop[ceiling((Tx/1))+1,1]*(((Tx/1))-floor((Tx/1))))+(pop[floor((Tx/1))+1,1]*(ceiling((Tx/1))-((Tx/1))))
    
    out=(c(R0,Gc,r,ex))
    names(out)=c("R0","Gc","r","birth-ex")
    warning('Carrying capacity not supplied, density independence assumed.')
    return(out)
  } else {
    lx = (pop[,2]/(pop[1,2]))
    bx = (pop[,3]/pop[,2])
    R0 = sum(lx*bx)
    Gc = sum(lx*bx*pop[,1])/R0
    rtemp = log(R0)/Gc
    dNdt = sum(pop[,2])*rtemp
    r = (dNdt*K)/(sum(pop[,2])*(K-sum(pop[,2])))
    
    Tx = sum(lx[1:length(lx)-1]+lx[2:length(lx)])/2
    ex = (pop[ceiling((Tx/1))+1,1]*(((Tx/1))-floor((Tx/1))))+(pop[floor((Tx/1))+1,1]*(ceiling((Tx/1))-((Tx/1))))
    
    out=(c(R0,Gc,r,ex))
    names(out)=c("R0","Gc","r","birth-ex")
    out
  }
}
