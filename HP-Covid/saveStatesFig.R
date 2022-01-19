library(ggplot2)
library(scales)
setwd("~/PP/covid/4.0")

trueCase <- read.csv("./data/I/state_covid_confirmed.csv")
trueCase <- trueCase[-c(1)]
start <- difftime(as.Date('2020-3-23'),as.Date('2020-3-22'),'days')
end <- difftime(as.Date('2021-8-31'),as.Date('2020-3-22'),'days')
stateList = scan("./data/stateName.txt", what="", sep="\n")
for (i in 1:length(stateList)) {
  print(which(stateList %in% stateList[i]))
  trueStateCase <- as.numeric(c(trueCase[which(stateList %in% stateList[i]),start:end]))
  simData <- read.csv(paste0("./data/I/state/", stateList[i], ".csv"))
  simData$true <- trueStateCase
  simData$u <- 2^(simData$u)
  if (i==12 || i==27) {
    simData$u <- simData$u - 10
  }
  dateSeq <- seq(as.Date('2020-3-23'), as.Date('2021-8-31'), by="day")
  sim <- ggplot(simData) +
    geom_bar(aes(x = dateSeq, weight = true, color="True"), fill="azure3", width = 0.6) +
    geom_line(aes(x = dateSeq, y = u, color="Average"), lwd = 2) + 
    scale_color_manual(name = "Covid Case", values = c("True"="gray70", "Average"="red"))
  png(file=paste("./Figures/",stateList[i],".png", sep = ""), width=800, height=480)
  print(sim + ggtitle(stateList[i]))
  dev.off()
}
