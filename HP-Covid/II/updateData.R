library(lubridate)
library(tidyverse)
library(dplyr)
library(sys)
setwd("~/PP/covid/4.0")

# Variable 
newEndDate = "10.01.2021"

# retrieve case count in history and format its dates
caseHist <- read.csv(file = './data/I/state_covid_confirmed.csv')
col <- colnames(caseHist)
newCol = c()
for (eachDate in col) {
  newCol <- c(newCol, format(as.Date(eachDate, "X%Y.%m.%d"), "%Y-%m-%d"))
}
# calculate the missing dates for data retrieval
dataStart <- as.Date(newCol[length(newCol)], "%Y-%m-%d")+1
dataEnd <- as.Date(newEndDate, "%m.%d.%Y")
# form a sequence of dates if in need of new data
if (dataStart < dataEnd) {
  dts=seq(dataStart, dataEnd, 1)
  dts=format(as.Date(dts, '%Y-%m-%d'), "%m-%d-%Y")
} else {
  return()
}
# data retrieval 
i = 1
data_state=read.csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",dts[i],".csv")))
data_state=data_state[,c("Province_State","Country_Region","Deaths","Confirmed","Recovered")]
data_state$date=dts[i]
for(i in 2:length(dts)){
  data_tmp=read_csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",dts[i],".csv")))
  data_tmp=data_tmp[,c("Province_State","Country_Region","Deaths","Confirmed","Recovered")]
  data_tmp$date=dts[i]
  names(data_tmp)=names(data_state)
  data_state=rbind(data_state,data_tmp)
}

not_state_list=c("Recovered","Wuhan Evacuee", "American Samoa", "Diamond Princess", "Grand Princess", "Northern Mariana Islands") # "States" to remove

names(data_state)[1]="Province.State"
names(data_state)[2]="Country.Region"
data_state$Deaths[is.na(data_state$Deaths)]=0
data_state$Confirmed[is.na(data_state$Confirmed)]=0
data_state=data_state[is.element(data_state$Country.Region,c("US")),]
agg_state=aggregate(Confirmed~Province.State+date,data=data_state,FUN=sum)
output_state=spread(agg_state,date,Confirmed)
output_state=output_state[!is.element(output_state$Province.State,not_state_list),]
output_state[is.na(output_state)]=0
rownames(output_state) <- output_state$Province.State
output_state = select(output_state, -Province.State)
# Update colname's format so it can be sorted correctly
col = colnames(output_state) 
for (i in 1:length(col)) {
  col[i] = format(as.Date(col[i], "%m-%d-%Y"), '%Y-%m-%d')
}
colnames(output_state) <- col
output_state = output_state[ , order(names(output_state))]
# combine the data 
N=ncol(output_state)
output_tmp=output_state
output_state[,1:(N-1)]=output_state[,2:N]-output_state[,1:(N-1)]
output_state=output_state[,1:(N-1)]
output_state[output_state<0]=0
output_state = output_state[,1:ncol(output_state)]
colnames(caseHist) <- newCol


newData <- cbind(caseHist,output_state)
write.csv(newData,"./data/II/state_covid_confirmed.csv",row.names=F)


