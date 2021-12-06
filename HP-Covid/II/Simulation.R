library(dplyr)
library(matrixStats)
setwd("~/PP/covid/3.0")
Simulation <- function(covid, reproNum, param) {
  # The total number of days for simulation
  covid <- read.csv("./data/II/state_covid_confirmed-7ma.csv")
  T <- length(covid[1,])
  reproNum_ <- read.csv(file = './data/II/param/R0_state.csv')
  param_ <- read.csv(file = "./data/II/param/weibull_param.csv")
  # State 
  stateList = scan("./data/stateName.txt", what="", sep="\n")
  for (state in 1:2) {
    # reproduction number of each state
    
    reproNum <- as.numeric(c(reproNum_[state,]))
    # parameters of weibull for each state
    
    param <- param_[state,]
    # Simulation & result summary
    for (itr in 1:10) {
      P <- Hawkes(T, param, reproNum, stateList[state])
      record <- hist(P, breaks = 0:T)
      if (itr == 1) {
        dailyCount <- data.frame("1" = record$counts)
      } else {
        dailyCount$v <- record$counts
      }
      colnames(dailyCount)[itr] <- itr
    }
    dailyAverage <- rowMeans(dailyCount)
    dailySd <- rowSds(as.matrix(dailyCount))
    upper <- dailyAverage + 1.96*dailySd
    lower <- dailyAverage - 1.96*dailySd
    lower[lower<0] <- 0
    dailyCount$u <- dailyAverage
    dailyCount$upper <- upper
    dailyCount$lower <- lower
    write.csv(dailyCount, paste("./data/II/state/", stateList[state], ".csv", sep = ""), row.names = F)
  }
  
  return(P)
}

# Kernal Function - weibull
weibull <- function(x, param){
  density <- (param$shape/param$scale) * ((x/param$scale)**(param$shape-1)) * exp(-((x/param$scale)**param$shape))
  return (density)
}

# Intensity Function
lambdaFunc <- function(t, P, param, r0, T) {
  reproLen <- length(r0)
  result <- param$mu
  
  for (occ in P) {
    if (occ < t & occ > t - 25) {
      result <- result + weibull(t - occ, param) * r0[ceiling(occ/T*reproLen)]
    }
  }
  return (result)
}

# Hawkes Simulation Algorithm
Hawkes <- function(T, param, r0, state) {
  # For NY, we specify a caliberation as the boom in the case number was so large at the 
  # beginning of the pandemic. 
  if (state == "NY") {
    t <- 0
    P <- c()
    caliber <- TRUE
    while (t < T) {
      if (t >= 10 & caliber) {
        P_mod <- c()
        for (i in 1:length(P)) {
          for (j in 1:4) {
            P_mod <- c(P_mod, P[i])
          }
        }
        P <- P_mod
        caliber <- FALSE
      }
      u1 = runif(1)
      M = lambdaFunc(t, P, param, r0, T)
      t <- t - log(u1)/M
      N = lambdaFunc(t,P, param, r0, T)
      u2 <- runif(1)
      out <- tryCatch(
        {
          if ((t < T) & (M!=0)) {
            if (u2 <= N/M) {
              P <- append(P, t)
            }
          }
        }, 
        error = function(cond){
          message(t)
          message(M)
        }
      )

    }
    return(P)
  } else {
    t <- 0
    P <- c()

    while (t < T) {
      u1 = runif(1)
      M = lambdaFunc(t, P, param, r0, T)
      t <- t - log(u1)/M
      N = lambdaFunc(t,P, param, r0, T)
      u2 <- runif(1)
      out <- tryCatch(
        {
          if ((t < T) & (M!=0)) {
            if (u2 <= N/M) {
              P <- append(P, t)
            }
          }
        }, 
        error = function(cond){
          message(t)
          message(M)
        }
      )
    }
    return(P)
  } 
  
}
