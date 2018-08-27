### Preliminaries
#devtools::install_github("gabrielodom/mvMonitoring")
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

# TODO: Add check for previously compiled data

### Compile and clean data 
# loadandcleanDBF returns a dataframe with all days including column names
rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)
# convert to xts
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",testingDay,sep="")]
save(rawData, file = "trainingSpecs/rawData.R")

### Run multistate function for BR and MT, return xts with test data
trainingDataBR <- multistate_train(rawData = rawData, 
                                  vars = varsBR, 
                                  stateVars = stateVarsBR, 
                                  testingDay = testingDay, 
                                  rollingWindowDays = rollingWindowDays, 
                                  alphaN = alphaN,
                                  faultsToTriggerAlarm = faultsToTriggerAlarm)
save(trainingDataBR, 
     file = paste("trainingSpecs/trainingDataBR.R",sep=""))

trainingDataMT <- multistate_train(rawData = rawData, 
                                  vars = varsMT, 
                                  stateVars = stateVarsMT, 
                                  testingDay = testingDay, 
                                  rollingWindowDays = rollingWindowDays, 
                                  alphaN = alphaN,
                                  faultsToTriggerAlarm = faultsToTriggerAlarm)
save(trainingDataMT, 
     file = paste("trainingSpecs/trainingDataMT.R",sep=""))


## Train model: Single state
uniqueData <- uniquenessCheck(rawData)
# Generate 'labelCol'
uniqueData <- cbind(uniqueData, rep(1,nrow(uniqueData)))
colnames(uniqueData)[ncol(uniqueData)] <- "labelCol"

## SAVE MEMORY, clean shit up:
rm(rawData)
rm(trainingDataBR)
rm(trainingDataMT)

# Train
trainingDataSS <- createTrainingSpecs(data = uniqueData,
                                      testingDay = testingDay,
                                      rollingWindowDays = rollingWindowDays,
                                      alpha = alphaN,
                                      faultsToTriggerAlarm = faultsToTriggerAlarm)
# Save results
save(trainingDataSS, 
     file = paste("trainingSpecs/trainingDataSS.R",sep=""))