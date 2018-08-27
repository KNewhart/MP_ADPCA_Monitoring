### Preliminaries

# Clear global environment
rm(list=ls())
# Load libraries
library(ADPCA)
# Set working directory
remote <- TRUE
if (remote) {
  setwd("C:/Users/Kate Newhart/odrive/Mines/Code/MP_ADPCA_Monitoring")
} else {
  setwd("C:/Users/SB-MBR/Desktop/R Code/MP_ADPCA_Monitoring")
}
# Load variables
source("vars.R")

### Compile and clean data 
# loadandcleanDBF returns a dataframe with all days including column names
rawData <- loadandcleanDBF(dataLocation, testingDay, nDays = 1)
# convert to xts
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste(testingDay,"/",sep="")]
# Subset data into BR and and MT
dataBR <- rawData[,varsBR]
dataMT <- rawData[,varsMT]
# Create states
dataBR_ls <- stateGenerator(data = dataBR, stateVars = stateVarsBR, testingDay = testingDay, minObs = 1)
dataMT_ls <- stateGenerator(data = dataMT, stateVars = stateVarsMT, testingDay = testingDay, minObs = 1)

# Load training specs
load("trainingSpecs/trainingDataSS.R")
load("trainingSpecs/trainingDataBR.R")
load("trainingSpecs/trainingDataMT.R")

# Only include states with training data
states2keepBR <- numeric()
states2keepMT <- numeric()
for (i in 1:length(trainingDataBR[[1]])) {
  states2keepBR <- c(states2keepBR, as.integer(trainingDataBR[[1]][[i]]$labelCol[1]))
}
for (i in 1:length(trainingDataMT[[1]])) {
  states2keepMT <- c(states2keepMT, as.integer(trainingDataMT[[1]][[i]]$labelCol[1]))
}

filtered.dataBR_ls <- list()
for (j in 1:length(states2keepBR)) {
  for (i in 1:length(dataBR_ls)) {
    n <- dataBR_ls[[i]]$labelCol[1]
    if (n == states2keepBR[j]) {
      filtered.dataBR_ls <- c(filtered.dataBR_ls, list(dataBR_ls[[i]]))
    } else {}
  }
}

filtered.dataMT_ls <- list()
for (j in 1:length(states2keepMT)) {
  for (i in 1:length(dataMT_ls)) {
    n <- dataMT_ls[[i]]$labelCol[1]
    if (n == states2keepMT[j]) {
      filtered.dataMT_ls <- c(filtered.dataMT_ls, list(dataMT_ls[[i]]))
    } else {}
  }
}

# Test SS
alarmDataSS <- testNewObs(data = rawData,
                          trainingSpecs = trainingDataSS,
                          testingDay = testingDay,
                          faultsToTriggerAlarm = faultsToTriggerAlarm)

# Test multistate
alarmDataBR <- multistate_test(data = filtered.dataBR_ls,
                              trainingSpec_ls = trainingDataBR[[2]],
                              testingDay = trainingDataBR[[3]],
                              faultsToTriggerAlarm = trainingDataBR[[4]])

alarmDataMT <- multistate_test(data = filtered.dataMT_ls,
                               trainingSpec_ls = trainingDataMT[[2]],
                               testingDay = trainingDataMT[[3]],
                               faultsToTriggerAlarm = trainingDataMT[[4]])





