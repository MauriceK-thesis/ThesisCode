#loop with models
#install.packages("gbm")
library(gbm)
#install.packages("dismo")
library(dismo)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
setwd("C:/thesis/treeModel")

#these are 90% of the PfPR point observations with related variables  
allVariablesMalaria <-  read.csv("water_statistics_Pf_2021_joined/allVariablesCombined2021.csv")

data <- select(allVariablesMalaria, -c(X))
# remote variables that have only NA 
data <- Filter(function(x)!all(is.na(x)), data)

# remove variables that have near zero variance 
data <- select(data, -c(waterArea5000_b50p0, waterArea5000_b100p0, waterArea5000_b50p10, waterArea5000_b100p10, waterArea5000_b50p20, waterArea1000_b50p50, 
                        waterArea5000_b50p50, waterArea5000_b100p50, waterArea1000_b50p90, waterArea5000_b50p90, waterArea1000_b100p90, waterArea5000_b100p90, 
                        waterArea5000_b1000p90, waterArea5000_b100p20, waterArea1000_b50p20, waterArea500_b50p50, waterArea500_b50p90, waterArea1000_b50p10))

# creating empty dataframes to put the model performance statistics in, nrow is for how many different types of models are in this run 
#rows:10 models, Columns: 10 runs
df_deviance <- data.frame(matrix(ncol=0,nrow=6))

RMSE_combined = data.frame(matrix(ncol=0,nrow=6))
MAE_combined = data.frame(matrix(ncol=0,nrow=6))
IQR_combined = data.frame(matrix(ncol=0,nrow=6))
adj.R2_combined = data.frame(matrix(ncol=0,nrow=6))


for ( i in 1:50){
  set.seed(i)
  
  # splitting the dataset in 90% training and 10% predicting/testing
  training <- sample(1:nrow(data),6212)
  testing <- setdiff(rownames(data),training)
  training <- data[training,]
  testing <- data[testing,]
  
  # training the model with different combinations of variables using 90% of the data, If there were many variables, flore tended to use the same model settings, I will use those 
  #model 1: Only confounders 
  data1 <- select(training, c(pf_pr, starts_with("housingMean"), starts_with("travelTime"), starts_with("popDensity"), starts_with("meanITN"), starts_with("meanTemp"), starts_with("elevation")))
  BRT_malaria1 <- gbm.fixed(data=data1, gbm.x=2:31 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  #model 2: All confounders plus precipitation 
  data2 <- select(training, c(pf_pr, starts_with("housingMean"), starts_with("travelTime"), starts_with("popDensity"), starts_with("meanITN"), starts_with("meanTemp"), starts_with("elevation"), starts_with("total_precipitation")))
  BRT_malaria2 <- gbm.fixed(data=data2, gbm.x=2:36 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  #model 3: All confounders and all "best" performing variables of the first run 
  data3 <- select(training, c(pf_pr, starts_with("housingMean"), starts_with("travelTime"), starts_with("popDensity"), starts_with("meanITN"), starts_with("meanTemp"), starts_with("elevation"), distance5000_b100000p50, 
                              distance20_b100000p10, distance5000_b100000p90, waterArea5000_b100000p90, waterArea5000_b100000p10, distance5000_b100000p0, distance1000_b10000p50, waterBodies_b100000p10, waterBodies_b100000p0, distance1000_b100p20, 
                              distance20_b100000p20, distance1000_b100000p20, distance500_b100000p90, distance500_b100000p0, distance1000_b100000p50, distance5000_b100000p10, distance20_b100000p90, distance500_b100000p50, waterArea20_b100000p90, 
                              distance1000_b100000p90, distance5000_b1000p50 , distance20_b10000p10, distance1000_b1000p20, distance5000_b10000p50, distance20_b100000p0, distance5000_b10000p90, distance1000_b100000p10, distance1000_b100000p0, 
                              waterArea5000_b100000p50, distance500_b100000p10, distance20_b1000p90, waterBodies_b10000p10, distance20_b10000p50, distance20_b10000p20, distance5000_b10000p20, waterArea5000_b100000p20, waterBodies_b100000p20, 
                              distance5000_b100000p20, distance20_b100000p50, distance500_b10000p90, waterBodies_b100000p50, waterArea_b100000p90, distance20_b10000p90, waterArea1000_b100000p10, waterBodies_b10000p0, waterArea1000_b100000p90, 
                              distance500_b10000p0, waterArea20_b100000p10, distance20_b50p50, distance500_b100p50, distance5000_b1000p90, distance500_b100000p20, distance1000_b50p50, distance20_b10000p0, waterArea500_b100000p90, 
                              waterArea5000_b10000p90, distance500_b50p10, distance1000_b10000p20, distance5000_b1000p20, distance1000_b10000p10, distance5000_b100p20, distance5000_b10000p10, distance5000_b1000p10, distance1000_b100p10))
  BRT_malaria3 <- gbm.fixed(data=data3, gbm.x=2:95 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  #model 4: All confounders, precipitation and all "best" performing variables of the first run 
  data4 <- select(training, c(pf_pr, starts_with("housingMean"), starts_with("travelTime"), starts_with("popDensity"), starts_with("meanITN"), starts_with("meanTemp"), starts_with("elevation"), starts_with("total_precipitation"), distance5000_b100000p50, 
                              distance20_b100000p10, distance5000_b100000p90, waterArea5000_b100000p90, waterArea5000_b100000p10, distance5000_b100000p0, distance1000_b10000p50, waterBodies_b100000p10, waterBodies_b100000p0, distance1000_b100p20, 
                              distance20_b100000p20, distance1000_b100000p20, distance500_b100000p90, distance500_b100000p0, distance1000_b100000p50, distance5000_b100000p10, distance20_b100000p90, distance500_b100000p50, waterArea20_b100000p90, 
                              distance1000_b100000p90, distance5000_b1000p50 , distance20_b10000p10, distance1000_b1000p20, distance5000_b10000p50, distance20_b100000p0, distance5000_b10000p90, distance1000_b100000p10, distance1000_b100000p0, 
                              waterArea5000_b100000p50, distance500_b100000p10, distance20_b1000p90, waterBodies_b10000p10, distance20_b10000p50, distance20_b10000p20, distance5000_b10000p20, waterArea5000_b100000p20, waterBodies_b100000p20, 
                              distance5000_b100000p20, distance20_b100000p50, distance500_b10000p90, waterBodies_b100000p50, waterArea_b100000p90, distance20_b10000p90, waterArea1000_b100000p10, waterBodies_b10000p0, waterArea1000_b100000p90, 
                              distance500_b10000p0, waterArea20_b100000p10, distance20_b50p50, distance500_b100p50, distance5000_b1000p90, distance500_b100000p20, distance1000_b50p50, distance20_b10000p0, waterArea500_b100000p90, 
                              waterArea5000_b10000p90, distance500_b50p10, distance1000_b10000p20, distance5000_b1000p20, distance1000_b10000p10, distance5000_b100p20, distance5000_b10000p10, distance5000_b1000p10, distance1000_b100p10))
  BRT_malaria4 <- gbm.fixed(data=data4, gbm.x=2:100 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  #model 5: Only precipitation 
  data5 <- select(training, c(pf_pr, starts_with("total_precipitation")))
  BRT_malaria5 <- gbm.fixed(data=data5, gbm.x=2:6 ,gbm.y=1, family="gaussian", tree.complexity = 4, learning.rate = 0.004, n.trees = 2000,  bag.fraction = 0.6)
  
  #model 6: Only best performing water statistics 
  data6 <- select(training, c(pf_pr, distance5000_b100000p50, 
                              distance20_b100000p10, distance5000_b100000p90, waterArea5000_b100000p90, waterArea5000_b100000p10, distance5000_b100000p0, distance1000_b10000p50, waterBodies_b100000p10, waterBodies_b100000p0, distance1000_b100p20, 
                              distance20_b100000p20, distance1000_b100000p20, distance500_b100000p90, distance500_b100000p0, distance1000_b100000p50, distance5000_b100000p10, distance20_b100000p90, distance500_b100000p50, waterArea20_b100000p90, 
                              distance1000_b100000p90, distance5000_b1000p50 , distance20_b10000p10, distance1000_b1000p20, distance5000_b10000p50, distance20_b100000p0, distance5000_b10000p90, distance1000_b100000p10, distance1000_b100000p0, 
                              waterArea5000_b100000p50, distance500_b100000p10, distance20_b1000p90, waterBodies_b10000p10, distance20_b10000p50, distance20_b10000p20, distance5000_b10000p20, waterArea5000_b100000p20, waterBodies_b100000p20, 
                              distance5000_b100000p20, distance20_b100000p50, distance500_b10000p90, waterBodies_b100000p50, waterArea_b100000p90, distance20_b10000p90, waterArea1000_b100000p10, waterBodies_b10000p0, waterArea1000_b100000p90, 
                              distance500_b10000p0, waterArea20_b100000p10, distance20_b50p50, distance500_b100p50, distance5000_b1000p90, distance500_b100000p20, distance1000_b50p50, distance20_b10000p0, waterArea500_b100000p90, 
                              waterArea5000_b10000p90, distance500_b50p10, distance1000_b10000p20, distance5000_b1000p20, distance1000_b10000p10, distance5000_b100p20, distance5000_b10000p10, distance5000_b1000p10, distance1000_b100p10))
  BRT_malaria6 <- gbm.fixed(data=data6, gbm.x=2:65 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  # # #model 7: All confounders plus precipitation + 5000m resolution of water statistics
  # data7 <- select(training, c(pf_pr, starts_with("housingMean"), starts_with("travelTime"), starts_with("popDensity"), starts_with("meanITN"), starts_with("meanTemp"), starts_with("elevation"), starts_with("total_precipitation"), contains("waterArea5000_b"), contains("distance5000_b")))
  # BRT_malaria7 <- gbm.fixed(data=data7, gbm.x=2:70 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  # #model 8: model 5 + urban accessibility
  # data8 <- select(training, c(pf_pr, starts_with("tem"), starts_with("pre"), occurrence, seasonality, access))
  # BRT_malaria8 <- gbm.fixed(data=data8, gbm.x=2:28 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  # 
  # #model 9: model 2 + PCR-GLOBWB 2 + remote sensing + urban accessibility
  # data9 <- select(training, c(pf_pr, starts_with("tem"), starts_with("waTem"), starts_with("pre"), starts_with("chanSto"), starts_with("dis"), starts_with("dynFra"), starts_with("run"), starts_with("satLow"), starts_with("satUp"), starts_with("groWa"), starts_with("groFos"), starts_with("actThi"), starts_with("eva"), starts_with("totRun"), starts_with("totThi"), starts_with("netPre"), access, occurrence, seasonality))
  # BRT_malaria9 <- gbm.fixed(data=data9, gbm.x=2:196 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  # 
  # #model 10: PCR-GLOBWB 2
  # data10 <- select(training, c(pf_pr, starts_with("chanSto"), starts_with("waTem"), starts_with("dis"), starts_with("dynFra"), starts_with("run"), starts_with("satLow"), starts_with("satUp"), starts_with("groWa"), starts_with("groFos"), starts_with("actThi"), starts_with("eva"), starts_with("totRun"), starts_with("totThi"), starts_with("netPre")))
  # BRT_malaria10 <- gbm.fixed(data=data10, gbm.x=2:169 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  # Using the remaining 10% of the data to predict/test PfPr values 
  preds1 <- predict.gbm(BRT_malaria1, testing,
                        n.trees=BRT_malaria1$gbm.call$n.trees, type="response")
  preds2 <- predict.gbm(BRT_malaria2, testing,
                        n.trees=BRT_malaria2$gbm.call$n.trees, type="response")
  preds3 <- predict.gbm(BRT_malaria3, testing,
                        n.trees=BRT_malaria3$gbm.call$n.trees, type="response")
  preds4 <- predict.gbm(BRT_malaria4, testing,
                        n.trees=BRT_malaria4$gbm.call$n.trees, type="response")
  preds5 <- predict.gbm(BRT_malaria5, testing,
                        n.trees=BRT_malaria5$gbm.call$n.trees, type="response")
  preds6 <- predict.gbm(BRT_malaria6, testing,
                        n.trees=BRT_malaria6$gbm.call$n.trees, type="response")
  # preds7 <- predict.gbm(BRT_malaria7, testing,
  #                       n.trees=BRT_malaria7$gbm.call$n.trees, type="response")
  # preds8 <- predict.gbm(BRT_malaria8, testing,
  #                       n.trees=BRT_malaria8$gbm.call$n.trees, type="response")
  # preds9 <- predict.gbm(BRT_malaria9, testing,
  #                       n.trees=BRT_malaria9$gbm.call$n.trees, type="response")
  # preds10 <- predict.gbm(BRT_malaria10, testing,
  #                        n.trees=BRT_malaria10$gbm.call$n.trees, type="response")
  
  predictions <- list(preds1, preds2, preds3, preds4, preds5, preds6)
  
  # creating the prediction maps --> not needed for me 
  # prmap1 <- predict.gbm(BRT_malaria1, df_surwa_map,
  #                       n.trees=BRT_malaria1$gbm.call$n.trees, type="response") 
  # df_prmap1 <- cbind(df_prmap1, prmap1)
  # 
  # prmap2 <- predict.gbm(BRT_malaria2, df_surwa_map,
  #                       n.trees=BRT_malaria2$gbm.call$n.trees, type="response") 
  # df_prmap2 <- cbind(df_prmap2, prmap2)
  # 
  # prmap3 <- predict.gbm(BRT_malaria3, df_surwa_map,
  #                       n.trees=BRT_malaria3$gbm.call$n.trees, type="response") 
  # df_prmap3 <- cbind(df_prmap3, prmap3)
  # 
  # prmap4 <- predict.gbm(BRT_malaria4, df_surwa_map,
  #                       n.trees=BRT_malaria4$gbm.call$n.trees, type="response") 
  # df_prmap4 <- cbind(df_prmap4, prmap4)
  # 
  # prmap5 <- predict.gbm(BRT_malaria5, df_surwa_map,
  #                       n.trees=BRT_malaria5$gbm.call$n.trees, type="response") 
  # df_prmap5 <- cbind(df_prmap5, prmap5)
  # 
  # prmap6 <- predict.gbm(BRT_malaria6, df_surwa_map,
  #                       n.trees=BRT_malaria6$gbm.call$n.trees, type="response")
  # df_prmap6 <- cbind(df_prmap6, prmap6)
  # 
  # prmap7 <- predict.gbm(BRT_malaria7, df_surwa_map,
  #                       n.trees=BRT_malaria7$gbm.call$n.trees, type="response") 
  # df_prmap7 <- cbind(df_prmap7, prmap7)
  # 
  # prmap8 <- predict.gbm(BRT_malaria8, df_surwa_map,
  #                       n.trees=BRT_malaria8$gbm.call$n.trees, type="response") 
  # df_prmap8 <- cbind(df_prmap8, prmap8)
  # 
  # prmap9 <- predict.gbm(BRT_malaria9, df_surwa_map,
  #                       n.trees=BRT_malaria9$gbm.call$n.trees, type="response") 
  # df_prmap9 <- cbind(df_prmap9, prmap9)
  # 
  # prmap10 <- predict.gbm(BRT_malaria10, df_surwa_map,
  #                        n.trees=BRT_malaria10$gbm.call$n.trees, type="response") 
  # df_prmap10 <- cbind(df_prmap10, prmap10)
  
  deviance <- rbind(BRT_malaria1$self.statistics$resid.deviance, BRT_malaria2$self.statistics$resid.deviance, BRT_malaria3$self.statistics$resid.deviance, BRT_malaria4$self.statistics$resid.deviance, BRT_malaria5$self.statistics$resid.deviance, BRT_malaria6$self.statistics$resid.deviance)
  
  df_deviance <- cbind(df_deviance, deviance)
  
  f_RMSE <- function(x,y=testing$pf_pr) {sqrt(mean((x-y)^2))}
  RMSE <- sapply(predictions, f_RMSE)
  RMSE_combined <- cbind(RMSE_combined, RMSE)
  
  f_MAE <- function(x,y=testing$pf_pr){mean(abs(x-y))}
  MAE <- sapply(predictions, f_MAE)
  MAE_combined <- cbind(MAE_combined, MAE)
  
  f_IQR <- function(x,y=testing$pf_pr){IQR(abs(x-y))} 
  IQR <- sapply(predictions, f_IQR)
  IQR_combined <- cbind(IQR_combined, IQR)
  
  R1 <- summary(lm(preds1~testing$pf_pr))$adj.r.squared
  R2 <- summary(lm(preds2~testing$pf_pr))$adj.r.squared
  R3 <- summary(lm(preds3~testing$pf_pr))$adj.r.squared
  R4 <- summary(lm(preds4~testing$pf_pr))$adj.r.squared
  R5 <- summary(lm(preds5~testing$pf_pr))$adj.r.squared
  R6 <- summary(lm(preds6~testing$pf_pr))$adj.r.squared
  # R7 <- summary(lm(preds7~testing$pf_pr))$adj.r.squared
  # R8 <- summary(lm(preds8~testing$pf_pr))$adj.r.squared
  # R9 <- summary(lm(preds9~testing$pf_pr))$adj.r.squared
  # R10 <- summary(lm(preds10~testing$pf_pr))$adj.r.squared
  adj.R2 <- c(R1, R2, R3, R4, R5, R6)
  adj.R2_combined <- cbind(adj.R2_combined, adj.R2)
  
  gbm.plot(BRT_malaria1, n.plots = 10, common.scale= TRUE,  write.title = F)
  gbm.plot(BRT_malaria2, n.plots = 10, common.scale= TRUE,  write.title = F)
  gbm.plot(BRT_malaria3, n.plots = 10, common.scale= TRUE,  write.title = F)
  gbm.plot(BRT_malaria4, n.plots = 10, common.scale= TRUE,  write.title = F)
  gbm.plot(BRT_malaria5, n.plots = 10, common.scale= TRUE,  write.title = F)
  gbm.plot(BRT_malaria6, n.plots = 10, common.scale= TRUE,  write.title = F)
  # gbm.plot(BRT_malaria7, n.plots = 10, common.scale= TRUE,  write.title = F)
  # gbm.plot(BRT_malaria8, n.plots = 10, common.scale= TRUE,  write.title = F)
  # gbm.plot(BRT_malaria9, n.plots = 10, common.scale= TRUE,  write.title = F)
  # gbm.plot(BRT_malaria10, n.plots = 10, common.scale= TRUE,  write.title = F)
  
  #summary(BRT_malaria1, cBars = 2, method = relative.influence, las = 2)
  #summary(BRT_malaria2, cBars = 20, method = relative.influence, las = 2)
  #summary(BRT_malaria3, cBars = 20, method = relative.influence, las = 2)
  #summary(BRT_malaria4, cBars = 20, method = relative.influence, las = 2)
  #summary(BRT_malaria5, cBars = 20, method = relative.influence, las = 2)
  #summary(BRT_malaria6, cBars = 20, method = relative.influence, las = 2)
  #summary(BRT_malaria7, cBars = 20, method = relative.influence, las = 2)
  #summary(BRT_malaria8, cBars = 20, method = relative.influence, las = 2)
  #summary(BRT_malaria9, cBars = 20, method = relative.influence, las = 2)
  #summary(BRT_malaria10, cBars = 20, method = relative.influence, las = 2)
}

#model performances
df_deviance$mean  <- rowMeans(df_deviance)
df_deviance

RMSE_mean <- rowMeans(RMSE_combined)
MAE_mean <- rowMeans(MAE_combined)
IQR_mean <- rowMeans(IQR_combined)
R2_mean <- rowMeans(adj.R2_combined)
performance <- cbind(RMSE_mean, MAE_mean, IQR_mean, R2_mean, deviance)
colnames(performance)<-c("RMSE", "MAE", "IQR", "adj.R2", "Deviance")
performance

#save the dataset and write it to file 
write.csv(performance, file="Results/thirdRunPrecAndWaterComparison2021.csv", row.names = FALSE)

## plot all values 
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(gridExtra)

RMSE_combined <- t(RMSE_combined)
colnames(RMSE_combined) <- c(1,2,3,4,5,6)
rownames(RMSE_combined) <- NULL
RMSE_combined_stacked <- stack(as.data.frame(RMSE_combined))

boxplot(RMSE_combined, main="RMSE of 50 runs", xlab="BRT models", ylab="RMSE")

# plot
plotRMSE <- ggplot(RMSE_combined_stacked, aes(x=ind, y=values)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("RMSE") + xlab("Model") +
  labs(title = "RMSE of 50 runs") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none")
  
  

#MAE_combined <- t(MAE_combined)
#colnames(MAE_combined) <- c(1,2,3,4,5,6)
MAE_combined_stacked <- stack(as.data.frame(MAE_combined))
boxplot(MAE_combined, main="MAE of 50 runs", xlab="BRT models", ylab="MAE")
##plot with ggplot 
plotMAE <- ggplot(MAE_combined_stacked, aes(x=ind, y=values)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("MAE") + xlab("Model") +
  labs(title = "MAE of 50 runs") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none")


#IQR_combined <- t(IQR_combined)
#colnames(IQR_combined) <- c(1,2,3,4,5,6)
IQR_combined_stacked <- stack(as.data.frame(IQR_combined))
boxplot(IQR_combined, main="IQR of 50 runs", xlab="BRT models", ylab="IQR")
##plot with ggplot 
plotIQR <- ggplot(IQR_combined_stacked, aes(x=ind, y=values)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("IQR") + xlab("Model") +
  labs(title = "IQR of 50 runs") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none")

#adj.R2_combined <- t(adj.R2_combined)
#colnames(adj.R2_combined) <- c(1,2,3,4,5,6)
R2_combined_stacked <- stack(as.data.frame(adj.R2_combined))
boxplot(adj.R2_combined, main="Adjusted R2 of 50 runs", xlab="BRT models", ylab="adjusted R2")
##plot with ggplot 
plotR2 <- ggplot(R2_combined_stacked, aes(x=ind, y=values)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("R2") + xlab("Model") +
  labs(title = "R2 of 50 runs") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none")

#df_deviance <- t(df_deviance)
#colnames(df_deviance) <- c(1,2,3,4,5,6)
Deviance_combined_stacked <- stack(as.data.frame(df_deviance))
boxplot(df_deviance[1:10,], main="Residual deviance of 50 runs", xlab="BRT models", ylab="deviance")
##plot with ggplot 
plotDeviance <- ggplot(Deviance_combined_stacked, aes(x=ind, y=values)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("Residual deviance") + xlab("Model") +
  labs(title = "Residual deviance of 50 runs") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none")



grid.arrange(plotR2,plotDeviance, plotRMSE, plotIQR, ncol = 2, nrow =2)


#variable importance
summary(
  BRT_malaria7, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

#variable importance
summary(BRT_malaria1, cBars = 2, method = relative.influence, las = 2)
summary(BRT_malaria2, cBars = 20, method = relative.influence, las = 2)
summary(BRT_malaria3, cBars = 20, method = relative.influence, las = 2)
summary(BRT_malaria4, cBars = 20, method = relative.influence, las = 2)
summary(BRT_malaria5, cBars = 20, method = relative.influence, las = 2)
summary(BRT_malaria6, cBars = 20, method = relative.influence, las = 2)
# summary(BRT_malaria7, cBars = 20, method = relative.influence, las = 2)
# summary(BRT_malaria8, cBars = 20, method = relative.influence, las = 2)
# summary(BRT_malaria9, cBars = 20, method = relative.influence, las = 2)
# summary(BRT_malaria10, cBars = 20, method = relative.influence, las = 2)

#PDP plots
#gbm.plot(BRT_malaria9, variable.no =49, common.scale= TRUE,  write.title = F, x.label = "water temperature in January (K)")
