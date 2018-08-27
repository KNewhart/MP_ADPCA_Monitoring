### Preliminaries

# Clear global environment
rm(list=ls())
# Load libraries
library(ADPCA)
# Set working directory
setwd("MP_ADPCA_Monitoring")
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
for (i in 1:length(trainingDataBR[[1]][[1]])) {
  states2keepBR <- c(states2keepBR, as.integer(trainingDataBR[[1]][[1]][[i]]$labelCol[1]))
}
for (i in 1:length(trainingDataMT[[1]][[1]])) {
  states2keepMT <- c(states2keepMT, as.integer(trainingDataMT[[1]][[1]][[i]]$labelCol[1]))
}

filtered.dataBR_ls <- list()
for (j in 1:length(states2keepBR)) {
  for (i in 1:length(dataBR_ls)) {
    n <- dataBR_ls[[i]]$labelCol[1]
    if (n == states2keepBR[1]) {
      filtered.dataBR_ls <- c(filtered.dataBR_ls, list(dataBR_ls[[i]]))
    } else {}
  }
}

filtered.dataMT_ls <- list()
for (j in 1:length(states2keepMT)) {
  for (i in 1:length(dataMT_ls)) {
    n <- dataMT_ls[[i]]$labelCol[1]
    if (n == states2keepMT[1]) {
      filtered.dataMT_ls <- c(filtered.dataMT_ls, list(dataMT_ls[[i]]))
    } else {}
  }
}

# Test SS
# alarmDataHolder <- testNewObs(data = uniqueData,
#                               trainingSpecs = trainingSpecHolder,
#                               testingDay = testingDay,
#                               faultsToTriggerAlarm = faultsToTriggerAlarm)

# Test multistate
alarmDataBR_ls <- list()
for (i in 1:length(trainingDataBR[[1]][[1]])) {
  alarmDataBR_ls <- c(alarmDataBR_ls, list(multistate_test(data = filtered.dataBR_ls[[i]],
                                            trainingSpec_ls = trainingDataBR[[1]][[1]][[i]],
                                            testingDay = trainingDataBR[[3]],
                                           faultsToTriggerAlarm = trainingDataBR[[4]])))
}






# Test each state
alarmDataBR_ls <- list()
for (i in 1:length(filtered.dataBR_ls)) {
  alarmDataHolder <- testNewObs(data = filtered.dataBR_ls[[i]], trainingSpecs = trainingSpecBR[[i]], testingDay = testingDay, faultsToTriggerAlarm = faultsToTriggerAlarm)
  alarmDataBR_ls <- c(alarmDataBR_ls, list(alarmDataHolder))
}
alarmDataMT_ls <- list()
for (i in 1:length(dataMT_ls)) {
  alarmDataHolder <- testNewObs(data = dataMT_ls[[i]], 
                                trainingSpecs = trainingSpecMT[[i]], 
                                testingDay = testingDay, 
                                faultsToTriggerAlarm = faultsToTriggerAlarm)
  alarmDataMT_ls <- c(alarmDataMT_ls, list(alarmDataHolder))
}

# Compile all data
alarmDataBR <- data.frame()
for (i in 1:length(alarmDataBR_ls)) {
  if (i == 1) {
    alarmDataBR <- as.data.frame(alarmDataBR_ls[[i]])
  } else {
    alarmDataBR <- fastmerge(alarmDataBR, as.data.frame(alarmDataBR_ls[[i]]))
  }
}

alarmDataMT <- data.frame()
for (i in 1:length(alarmDataMT_ls)) {
  if (i == 1) {
    alarmDataMT <- as.data.frame(alarmDataMT_ls[[i]])
  } else {
    alarmDataMT <- fastmerge(alarmDataMT, as.data.frame(alarmDataMT_ls[[i]]))
  }
}







