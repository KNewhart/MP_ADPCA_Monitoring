### Preliminaries

# Clear global environment
rm(list=ls())

# Load libraries
library(ADPCA)
# Set working directory
setwd("MP_ADPCA_Monitoring")
# Load variables
source("vars.R")





# Compile and clean data 
# loadandcleanDBF returns a dataframe with all days including column names
rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)
# convert to xts
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",testingDay,sep="")]


## Train model: Single state
uniqueData <- uniquenessCheck(rawData)
# Generate 'labelCol'
uniqueData <- cbind(uniqueData, rep(1,nrow(uniqueData)))
colnames(uniqueData)[ncol(uniqueData)] <- "labelCol"
# Train
trainingDataSS <- createTrainingSpecs(data = uniqueData,
                                      testingDay = testingDay,
                                      rollingWindowDays = rollingWindowDays,
                                      alpha = alphaN,
                                      faultsToTriggerAlarm = faultsToTriggerAlarm)

# Run multistate function for BR and MT, return xts with test data
trainingDataBR <- multistate_train(rawData = rawData, 
                                  vars = varsBR, 
                                  stateVars = stateVarsBR, 
                                  testingDay = testingDay, 
                                  rollingWindowDays = rollingWindowDays, 
                                  alphaN = alphaN,
                                  faultsToTriggerAlarm = faultsToTriggerAlarm)

trainingDataMT <- multistate_train(rawData = rawData, 
                                  vars = varsMT, 
                                  stateVars = stateVarsMT, 
                                  testingDay = testingDay, 
                                  rollingWindowDays = rollingWindowDays, 
                                  alphaN = alphaN,
                                  faultsToTriggerAlarm = faultsToTriggerAlarm)

