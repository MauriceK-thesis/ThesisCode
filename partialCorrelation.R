# shows a more basic overview of Multicollinearity  
#install.packages("psych")
library(psych)
library(tidyverse)
#Set working directory
setwd("C:/Users/mauri/OneDrive/Universiteit Master/Thesis/Data")

# add data to script: all buffer sizes are checked for multicollinearity 
# set persistence 
persistence <- 0
dataset <- '2021'
bufferSize <- c(50, 100, 1000, 10000, 100000)
columnNames <- cs()
options(scipen=999)

# now add all buffer sizes together for each specific variable 
#########################################################WaterArea 5m resolution#####################################################
buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(waterArea, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$waterArea)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(waterArea, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$waterArea)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(waterArea, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$waterArea)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(waterArea, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$waterArea)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(waterArea, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$waterArea)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))


variableType <- cs(waterArea, waterArea, waterArea, waterArea, waterArea)
combinedCorrelationFirstVar <- cbind(variableType, bufferSize,  combinedCorrelation)

#########################################################WaterArea 20m resolution#####################################################
buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(waterArea20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$waterArea20)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(waterArea20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$waterArea20)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(waterArea20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$waterArea20)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(waterArea20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$waterArea20)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(waterArea20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$waterArea20)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))

variableType <- cs(waterArea20, waterArea20, waterArea20, waterArea20, waterArea20)

combinedCorrelationwaterArea20 <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationFirstVar) <- names(combinedCorrelationwaterArea20) 

combinedCorrelationTotal <- rbind(combinedCorrelationFirstVar, combinedCorrelationwaterArea20)

#########################################################WaterArea 500m resolution#####################################################

buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(waterArea500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$waterArea500)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(waterArea500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$waterArea500)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(waterArea500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$waterArea500)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(waterArea500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$waterArea500)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(waterArea500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$waterArea500)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))


variableType <- cs(waterArea500, waterArea500, waterArea500, waterArea500, waterArea500)

combinedCorrelationwaterArea500 <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationwaterArea500) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationwaterArea500)

#########################################################WaterArea 1000m resolution#####################################################

buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(waterArea1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$waterArea1000)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(waterArea1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$waterArea1000)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(waterArea1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$waterArea1000)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(waterArea1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$waterArea1000)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(waterArea1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$waterArea1000)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))


variableType <- cs(waterArea1000, waterArea1000, waterArea1000, waterArea1000, waterArea1000)

combinedCorrelationwaterArea1000 <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationwaterArea1000) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationwaterArea1000)

#########################################################WaterArea 5000m resolution#####################################################

buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(waterArea5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$waterArea5000)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(waterArea5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$waterArea5000)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(waterArea5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$waterArea5000)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(waterArea5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$waterArea5000)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(waterArea5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$waterArea5000)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))


variableType <- cs(waterArea5000, waterArea5000, waterArea5000, waterArea5000, waterArea5000)

combinedCorrelationwaterArea5000 <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationwaterArea5000) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationwaterArea5000)

#########################################################WaterBodies 5-20m resolution#####################################################

buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(waterBodies, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$waterBodies)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(waterBodies, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$waterBodies)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(waterBodies, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$waterBodies)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(waterBodies, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$waterBodies)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[2,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(waterBodies, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$waterBodies)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[2,]))


variableType <- cs(waterBodies, waterBodies, waterBodies, waterBodies, waterBodies)

combinedCorrelationwaterBodies <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationwaterBodies) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationwaterBodies)
######################################################### Rainfall #####################################################

buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(total_precipitation, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$total_precipitation)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(total_precipitation, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$total_precipitation)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(total_precipitation, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$total_precipitation)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(total_precipitation, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$total_precipitation)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(total_precipitation, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$total_precipitation)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))


variableType <- cs(total_precipitation, total_precipitation, total_precipitation, total_precipitation, total_precipitation)

combinedCorrelationtotal_precipitation <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationtotal_precipitation) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationtotal_precipitation)


######################################################### Water distance 20m #####################################################

buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(distance20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$distance20)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(distance20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$distance20)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(distance20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$distance20)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(distance20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$distance20)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(distance20, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$distance20)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[2,]))

variableType <- cs(distance20, distance20, distance20, distance20, distance20)

combinedCorrelationdistance20 <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationdistance20) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationdistance20)

######################################################### Water distance 500m #####################################################

buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(distance500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$distance500)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(distance500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$distance500)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(distance500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$distance500)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(distance500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$distance500)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(distance500, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$distance500)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))

variableType <- cs(distance500, distance500, distance500, distance500, distance500)

combinedCorrelationdistance500 <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationdistance500) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationdistance500)

######################################################### Water distance 1000m #####################################################

buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(distance1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$distance1000)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(distance1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$distance1000)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(distance1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$distance1000)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(distance1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$distance1000)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(distance1000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$distance1000)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))

variableType <- cs(distance1000, distance1000, distance1000, distance1000, distance1000)

combinedCorrelationdistance1000 <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationdistance1000) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationdistance1000)

######################################################### Water distance 5000m #####################################################


buffer50 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer50_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer50.r <- partial.r(data = buffer50, x = cs(distance5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n50 <- length(which(!is.na(buffer50$distance5000)))
buffer50.p <- corr.p(buffer50.r, n50)
combinedCorrelation <- as_tibble_row(buffer50.p$ci[1,])

buffer100 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100.r <- partial.r(data = buffer100, x = cs(distance5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100 <- length(which(!is.na(buffer100$distance5000)))
buffer100.p <- corr.p(buffer100.r, n100)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100.p$ci[1,]))

buffer1000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer1000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer1000.r <- partial.r(data = buffer1000, x = cs(distance5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n1000 <- length(which(!is.na(buffer1000$distance5000)))
buffer1000.p <- corr.p(buffer1000.r, n1000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer1000.p$ci[1,]))

buffer10000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer10000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer10000.r <- partial.r(data = buffer10000, x = cs(distance5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n10000 <- length(which(!is.na(buffer10000$distance5000)))
buffer10000.p <- corr.p(buffer10000.r, n10000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer10000.p$ci[1,]))

buffer100000 <-  read.csv(sprintf("water_statistics_pf_%s_joined/persistence%dbuffer100000_%s_Pf.csv", dataset, persistence, dataset), sep=",")
buffer100000.r <- partial.r(data = buffer100000, x = cs(distance5000, pf_pr), y = cs(housingMean, meanITN, meanTemp, elevation))
n100000 <- length(which(!is.na(buffer100000$distance5000)))
buffer100000.p <- corr.p(buffer100000.r, n100000)
combinedCorrelation <- combinedCorrelation %>%
  add_row(as_tibble_row(buffer100000.p$ci[1,]))

variableType <- cs(distance5000, distance5000, distance5000, distance5000, distance5000)

combinedCorrelationdistance5000 <- cbind(variableType, bufferSize, combinedCorrelation)

# coerce the names to be the same 
names(combinedCorrelationTotal) <- names(combinedCorrelationdistance5000) 

combinedCorrelationTotal <- rbind(combinedCorrelationTotal, combinedCorrelationdistance5000)

write.csv(combinedCorrelationTotal, file=sprintf("correlation/%s_Pf/correlationPersist%d_%s_Pf.csv", dataset, persistence, dataset), row.names=FALSE)

