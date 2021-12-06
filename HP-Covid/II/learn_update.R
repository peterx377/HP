source("./HP-Covid/II/wblfit.R")
learn_update <- function(trueCase, emiter, N, bins, params, newdays) {
  extra <- 0
  p <- array(0,dim = c(N,N))
  newdays <- newdays%/%10
  if (newdays > 0) {
    for (i in 1:newdays) {
      newbin <- bins[length(bins)] 
      bins <- c(bins, newbin)
    }
  } 
  rList <- bins
  bins <- length(rList)
  alpha <- params$scale
  beta <- params$shape
  mu <- params$mu
  lam <- 0
  
  last_mu = mu
  last_alpha = alpha
  last_beta = beta
  last_K0 = rList
  for (k in 1:emiter) {
    newPlam <- updatep(trueCase, p, rList, alpha, beta, mu, N, bins, extra)
    p <- newPlam$p
    lam <- newPlam$lam
    newPara <- updatepar(rList, trueCase, p, N, bins, extra)
    rList <- newPara$rList
    alpha <- newPara$alpha
    beta <- newPara$beta
    mu <- newPara$mu
    bup <- 4
    if (beta > 4) {
      beta <- bup
    }
    # if(k%%5==0) {
    #   if (beta < bup) {
    #     last_mu = mu;
    #     last_alpha = alpha;
    #     last_beta = beta;
    #     last_K0 = rList;        
    #   } else {
    #     mu = last_mu;
    #     alpha = last_alpha;
    #     beta = last_beta;
    #     rList = last_K0;
    #     break       
    #   }
    #   
    # }

  }
  
  result <- list("rList" = rList, "alpha" = alpha, "beta" = beta, "mu"=mu, "p"=p, "lam"=lam)
  return(result)
}

updatep <- function(trueCase, p, rList, alpha, beta, mu, N, bins, extra) {
  lam <- rep(0, N)
  #
  p[1,1] <- mu
  lam[1] <- sum(p[1,1:N])
  p[1,1:N] <- p[1,1:N]/lam[1]
  
  
  for (i in 2:N) {
    for (j in 1:i-1) {
      r0 <- rList[ceiling(bins*j/N)]
      ref <- as.numeric(trueCase[j])
      p[i,j] <- r0*dweibull(x=(i-j), shape=beta, scale=alpha)*ref
    }
    p[i,i] <- mu
    lam[i] <- sum(p[i,1:N])
    p[i,1:N] <- p[i,1:N]/lam[i]      
    
  }
  #
  result <- list("p" = p, "lam" = lam)
  return(result)
}

updatepar <- function(rList, trueCase, p, N, bins, extra) {
  mu <- 0
  rList <- rep(0, bins)
  Nc <- rep(0, bins)
  time_sample <- c()
  p_sample <- c()
  
  mu <- mu + p[1,1]*as.numeric(trueCase[1])
  
  for (i in 2:N) {
    J <- min(i-1, N-extra)
    if (J!=0) {
      for (j in 1:min((i-1), (N-extra))) {
        time_sample <- append(time_sample, (i-j))
        p_sample <- append(p_sample, p[i,j]*as.numeric(trueCase[i]))        
      }
    }
    for (j in 1:i-1) {
      rList[ceiling(bins*j/N)] <- rList[ceiling(bins*j/N)] + p[i,j]*as.numeric(trueCase[i])
    }      
    
    mu <- mu + p[i,i]*as.numeric(trueCase[i])
    
  }
  
  fit_coeff <- wblfit(time_sample, p_sample)
  alpha <- fit_coeff$alpha
  beta <- fit_coeff$beta
  for (i in 1:N) {
    Nc[ceiling(bins*i/N)] <- Nc[ceiling(bins*i/N)] + as.numeric(trueCase[i])
  }
  rList <- rList/(Nc+0.000001)
  rList[bins] <- rList[bins-1]
  mu <- mu/N
  result <- list("rList"=rList, "alpha" = alpha, "beta" = beta, "mu" = mu)
  return(result)
}