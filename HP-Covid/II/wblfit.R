# The function wblfit is translated from the same function from matlab
# For details of this function, please reference to documentation in matlab
library("nleqslv")
wblfit <- function(t, p) {
  #prep
  censoring <- rep(0, length(t))
  t <- log(t)
  flt <- which(p==0)
  t <- t[-flt]
  censoring <- censoring[-flt]
  p <- p[-flt]
  n <- sum(p)
  ranget <- max(t) - min(t)
  maxt <- max(t)
  x0 <- (t - maxt)/ranget
  
  #MM for no censoring
  sigmahat <- (sqrt(6)*sd(x0))/pi
  wgtmean <- sum(x0*p)/n
  #Bracket the root of the scale parameter likelihood eqn
  if (lkeqn(sigmahat, x0, p, wgtmean) > 0) {
    upper <- sigmahat
    lower <- 0.5 * upper
    
    while (lkeqn(lower, x0, p, wgtmean)>0) {
      upper <- lower
      lower <- upper * 0.5
    }
  } else {
    lower <- sigmahat
    upper <- 2*lower
    while (lkeqn(upper, x0, p, wgtmean)<0) {
      lower <- lower
      upper <- 2*upper
    } 
  }
  r <- nleqslv(x = 0.5*(upper+lower), fn = lkeqn, jac=NULL, x0, p, wgtmean)
  muhat <- r$x * log(sum(p*exp(x0/r$x))/n)
  alpha <- exp(ranget*muhat+maxt)
  beta <- 1/(ranget*r$x)
  result <- list("alpha" = alpha, "beta" = beta)
  return(result)
}

lkeqn <- function(sigma, x, w, xbarWgtUnc) {
  w <- exp(x/sigma)*w
  v <- sigma + xbarWgtUnc - sum(x*w)/sum(w)
  return(v)
}