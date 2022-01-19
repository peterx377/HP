library(dplyr)
library(matrixStats)
setwd("~/PP/covid/4.0")
# Variable
iteration = 1000 # Simulation sample size

# The total number of days for simulation
covid <- read.csv("./data/II/state_covid_confirmed-7ma.csv")
T <- length(covid[1,])
reproNum_ <- read.csv(file = './data/II/param/R0_state.csv')
param_ <- read.csv(file = "./data/II/param/weibull_param.csv")
# State 
stateList = scan("./data/stateName.txt", what="", sep="\n")
for (state in 1:length(stateList)) {
  # reproduction number of each state
  reproNum <- as.numeric(c(reproNum_[state,]))
  # parameters of weibull for each state
  
  param <- param_[state,]
  # Simulation & result summary
  for (itr in 1:iteration) {
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
  dailyCount$u <- dailyAverage
  write.csv(dailyCount, paste("./data/II/state/", stateList[state], ".csv", sep = ""), row.names = F)
}
  
# ---------------------Below are functions---------------------

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
    if (occ < t & occ > t - 10) {
      result <- result + weibull(t - occ, param) * r0[ceiling(occ/T*reproLen)]
    }
  }
  return (result)
}

# Hawkes Simulation Algorithm
Hawkes <- function(T, param, r0, state) {
  # For NY, we specify a caliberation as the boom in the case number was so large at the 
  # beginning of the pandemic. 

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
