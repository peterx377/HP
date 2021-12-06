# Calculate moving average for each state based on the choice of rescale factors 

library(TTR)
library(dplyr)
setwd("~/PP/covid/3.0")
dataCleanup <- function() {
  # read raw data
  data_state = read.csv("./data/II/state_covid_confirmed.csv", row.names=1)
  state = nrow(data_state)
  rescale <- read.csv("./data/rescale.csv")
  ma_state <- data.frame() 
  
  # compute 7 day average
  for (i in 1:state) {
    state = as.numeric(data_state[i,])  # Convert each state's data to numeric
    m <- SMA(state, 7)                  # run ma on data
    for (j in 1:6) {                    # for the first 6, run mean(1:i)
      m[j] = mean(state[1:j])
    }
    ma_state <- rbind(ma_state, m/as.integer(rescale[i,1]))
  }
  colnames(ma_state) <- colnames(data_state)
  write.csv(ma_state,"./data/II/state_covid_confirmed-7ma.csv", row.names = F)
}

