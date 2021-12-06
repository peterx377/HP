source("./HP-Covid/II/learn_update.r")
# Update all the parameters needed for the simulation

updateParam <- function() {
  # State 
  stateList = scan("./data/stateName.txt", what="", sep="\n")
  # read the current parameters
  params <- read.csv("./data/I/param/weibull_param.csv")
  rList <- read.csv("./data/I/param/R0_state.csv")
  newdata <- read.csv("./data/II/state_covid_confirmed-7ma.csv")
  olddata <- read.csv("./data/I/state_covid_confirmed-7ma.csv")
  N <- length(newdata[1,])
  newdays <- N - length(olddata[1,])
  # initialize parameters for storage
  weib <- NULL
  r <- NULL
  for (state in 1:length(stateList)) {
    # Parameters of weibull for each state
    stateParam <- params[state,]
    # rList for each state
    r0 <- as.numeric(c(rList[state,]))
    # state rescaled data
    stateData <- as.numeric(c(newdata[state,]))
    # Learn the new parameters
    result <- learn_update(stateData, 20, N, r0, stateParam, newdays)
    # Fill the new parm into storage
    weib <- rbind(weib, c(result$alpha, result$beta, result$mu))
    r <- rbind(r, c(result$rList))
  }
  colnames(weib) <- c("scale", "shape", "mu")
  # Output the new 
  write.csv(weib, "./data/II/param/weibull_param.csv", row.names = F)
  write.csv(r, "./data/II/param/R0_state.csv", row.names = F)
  
}