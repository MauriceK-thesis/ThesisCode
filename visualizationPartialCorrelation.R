library(tidyverse)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(ggpubr)

setwd("C:/Users/mauri/OneDrive/Universiteit Master/Thesis/linearRegression")

options(scipen=999)

#####################################################################################################################################################################################
####################################################################### scenario 1 ############################################################################################

## Partial correlations of all factors without precipitation as confounder -- 2021 dataset
fullTable2021 <- read.csv("correlation/allVariables/basicFullTable2021.csv")
meanVariables2021 <- read.csv("correlation/allVariables/meanVariablePerRes2021Separated.csv", sep = ";")
meanVariables2021 <- meanVariables2021 %>% na.omit()


## Partial correlations of all factors without precipitation as confounder -- Total dataset
fullTableTotal <- read.csv("correlation/allVariables/basicFullTableTotal.csv")
meanVariablesTotal <- read.csv("correlation/allVariables/meanVariablePerResTotalSeparated.csv", sep = ";")
meanVariablesTotal <- meanVariablesTotal %>% na.omit()


resolutionPlot2021 <- ggplot(meanVariables2021, aes(x= as.factor(variableType), y = r, fill = as.factor(resolution))) +  
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("waterArea", "distance")) +
  scale_y_continuous(limits=c(-0.15, 0.15)) +
  ylab("partial correlation r") + xlab("Variable type") +
  labs(title = "(a) Effect of resolution on water statistics (2021)") + 
  scale_fill_manual(values=c("#101010", "#404240", "#707371", "#a1a4a2", "#d3d4d3"), name = "Resolution (m)") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") 

resolutionPlotTotal <- ggplot(meanVariablesTotal, aes(x= as.factor(variableType), y = r, fill = as.factor(resolution))) +  
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("waterArea", "distance")) +
  scale_y_continuous(limits=c(-0.15, 0.15)) +
  ylab("partial correlation r") + xlab("Variable type") +
  labs(title = "(b) Effect of resolution on water statistics (Total)") + 
  scale_fill_manual(values=c("#101010", "#404240", "#707371", "#a1a4a2", "#d3d4d3"), name = "Resolution (m)") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") 


grid.arrange(resolutionPlot2021, resolutionPlotTotal, ncol = 2, nrow =1)

fullTable2021 <- read.csv("correlation/allVariables/basicFullTable2021.csv")

fullTable2021$p[fullTable2021$p >= 0.05] <- 2 
fullTable2021$p[fullTable2021$p <= 0.05] <- 1 


## remove all non significant or NA relations  
fullTable2021 <- fullTable2021 %>% na.omit()
fullTable2021 <- fullTable2021 %>% filter(p <= 0.05)

fullTableBuffer2021 <- fullTable2021 %>% filter(bufferSize >= 50)
variableList <- c("waterArea", "distance20", "total_precipitation", "waterBodies")
fullTableBuffer2021 <- fullTable2021 %>% filter(variableType %in% variableList)

ggp0 <- ggplot(filter(fullTableBuffer2021, persistence == 0), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000","10000","100000", labels = NULL)) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(a) Water > 0% persistence & Precipitation") + 
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Variable type", labels = c("Average distance to water", "Total precipitation", "Percentage water area", "Number of water bodies")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14),legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"), labels = c("p<=0.05", "p>0.05"), name = "Significance")

#legend.text = element_text(size=15), legend.title = element_text(size=15), legend.key.size = unit(1, 'cm')

ggp0
ggp1 <- ggplot(filter(fullTableBuffer2021, persistence == 10), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(b) Water > 10% persistence & Precipitation") +
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0")) + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp2 <- ggplot(filter(fullTableBuffer2021, persistence == 20), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(c) Water > 20% persistence & Precipitation") +
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0")) + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp3 <- ggplot(filter(fullTableBuffer2021, persistence == 50), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(d) Water > 50% persistence & Precipitation") +
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0")) + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp4 <- ggplot(filter(fullTableBuffer2021, persistence == 90), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(e) Water > 90% persistence & Precipitation") +
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0")) + theme_classic()  + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

grid.arrange(ggp0, ggp1, ggp2, ggp3, ggp4, as_ggplot(legend1), ncol = 2, nrow =3)

legend1 <- get_legend(ggp0)
## Partial correlations of all factors without precipitation as confounder -- 2021 dataset
fullTableTotal <- read.csv("correlation/allVariables/basicFullTableTotal.csv")
fullTableTotal <- fullTableTotal %>% na.omit()
fullTableTotal <- fullTableTotal %>% filter(p <= 0.05)

meanVariablesTotal <- read.csv("correlation/allVariables/meanVariablePerResTotal.csv")
meanVariablesTotal <- cbind(df, meanVariablesTotal)


fullTableBufferTotal <- fullTableTotal %>% filter(bufferSize >= 1000)
variableList <- c("waterArea", "distance20", "total_precipitation", "waterBodies")
fullTableBufferTotal <- fullTableTotal %>% filter(variableType %in% variableList)

# change significance into 1 or 2 
fullTableBufferTotal$p[fullTableBufferTotal$p >= 0.05] <- 2 
fullTableBufferTotal$p[fullTableBufferTotal$p <= 0.05] <- 1 

ggp0 <- ggplot(filter(fullTableBufferTotal, persistence == 0), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000","10000","100000", labels = NULL)) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(a) Water > 0% persistence & Precipitation") + 
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Variable type", labels = c("Average distance to water", "Total precipitation", "Percentage water area", "Number of water bodies")) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14),legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"), labels = c("p<=0.05", "p>0.05"), name = "Significance")

#legend.text = element_text(size=15), legend.title = element_text(size=15), legend.key.size = unit(1, 'cm')

ggp0
ggp1 <- ggplot(filter(fullTableBufferTotal, persistence == 10), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(b) Water > 10% persistence & Precipitation") +
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0")) + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp2 <- ggplot(filter(fullTableBufferTotal, persistence == 20), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(c) Water > 20% persistence & Precipitation") +
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0")) + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp3 <- ggplot(filter(fullTableBufferTotal, persistence == 50), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(d) Water at > 50% persistence & Precipitation") +
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0")) + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp4 <- ggplot(filter(fullTableBufferTotal, persistence == 90), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = variableType)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(e) Water > 90% persistence & Precipitation") +
  scale_fill_manual(values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0")) + theme_classic()  + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))


grid.arrange(ggp0, ggp1, ggp2, ggp3, ggp4, as_ggplot(legend1), ncol = 2, nrow =3)

#####################################################################################################################################################################################
####################################################################### scenario 2 ############################################################################################

## create the graphs for the second scenario where precipitation is a confounder of water 
scen2FullTable2021 <- read.csv("correlation/2021_Pf/scenario2All_2021_Pf.csv", sep = ";")
variableListScen2 <- c("distance20", "distance500", "distance1000", "distance5000")
scen2FullTable2021$p[scen2FullTable2021$p >= 0.05] <- 2 
scen2FullTable2021$p[scen2FullTable2021$p <= 0.05] <- 1 

#legend.text = element_text(size=15), legend.title = element_text(size=15), legend.key.size = unit(1, 'cm')

ggp0
ggp1 <- ggplot(filter(scen2FullTable2021, persistence == 10), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = factor(variableType, level = level_order))) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(a) Distance to water > 10% persistence & Precipitation") +
  scale_fill_manual(breaks=c('distance20', 'distance500', 'distance1000', 'distance5000'), values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Map resolution", labels = c("20 m", "500 m", "1000 m", "5000 m")) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"), labels = c("p<=0.05", "p>0.05"), name = "Significance")


ggp2 <- ggplot(filter(scen2FullTable2021, persistence == 20), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = factor(variableType, level = level_order))) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(b) Distance to water > 20% persistence & Precipitation") +
  scale_fill_manual(breaks=c('distance20', 'distance500', 'distance1000', 'distance5000'), values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Map resolution", labels = c("20 m", "500 m", "1000 m", "5000 m")) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp3 <- ggplot(filter(scen2FullTable2021, persistence == 50), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = factor(variableType, level = level_order))) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(c) Distance to water > 50% persistence & Precipitation") +
  scale_fill_manual(breaks=c('distance20', 'distance500', 'distance1000', 'distance5000'), values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Map resolution", labels = c("20 m", "500 m", "1000 m", "5000 m")) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp4 <- ggplot(filter(scen2FullTable2021, persistence == 90), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = factor(variableType, level = level_order))) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(d) Distance to water > 90% persistence & Precipitation") +
  scale_fill_manual(breaks=c('distance20', 'distance500', 'distance1000', 'distance5000'), values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Map resolution", labels = c("20 m", "500 m", "1000 m", "5000 m")) + 
  theme_classic() +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

grid.arrange(ggp1, ggp2, ggp3, ggp4, as_ggplot(legend), ncol = 2, nrow =3)

legend <- get_legend(ggp1)

################################################################################################################################################################################################################################
## create the graphs for the second scenario where precipitation is a confounder of water 
scen2FullTabletotal <- read.csv("correlation/total_Pf/scenario2All_total_Pf.csv", sep = ";")
variableListScen2 <- c("distance20", "distance500", "distance1000", "distance5000")
scen2FullTabletotal$p[scen2FullTabletotal$p >= 0.05] <- 2 
scen2FullTabletotal$p[scen2FullTabletotal$p <= 0.05] <- 1 
level_order <- c('distance20', 'distance500', 'distance1000', 'distance5000')
#legend.text = element_text(size=15), legend.title = element_text(size=15), legend.key.size = unit(1, 'cm')
#, labels = c("1000 m", "Total precipitation", "Percentage water area", "Number of water bodies")

ggp1 <- ggplot(filter(scen2FullTabletotal, persistence == 10), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = factor(variableType, level = level_order))) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(a) Distance to water > 10% persistence & Precipitation") +
  scale_fill_manual(breaks=c('distance20', 'distance500', 'distance1000', 'distance5000'), values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Map resolution", labels = c("20 m", "500 m", "1000 m", "5000 m")) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"), labels = c("p<=0.05", "p>0.05"), name = "Significance")
ggp1

ggp2 <- ggplot(filter(scen2FullTabletotal, persistence == 20), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = factor(variableType, level = level_order))) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(b) Distance to water > 20% persistence & Precipitation") +
  scale_fill_manual(breaks=c('distance20', 'distance500', 'distance1000', 'distance5000'), values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Map resolution", labels = c("20 m", "500 m", "1000 m", "5000 m")) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp3 <- ggplot(filter(scen2FullTabletotal, persistence == 50), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = factor(variableType, level = level_order))) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(c) Distance to water > 50% persistence & Precipitation") +
  scale_fill_manual(breaks=c('distance20', 'distance500', 'distance1000', 'distance5000'), values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Map resolution", labels = c("20 m", "500 m", "1000 m", "5000 m")) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

ggp4 <- ggplot(filter(scen2FullTabletotal, persistence == 90), aes(x= reorder(bufferSize, -r), y=r, fill=variableType, group = factor(variableType, level = level_order))) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("50","100","1000", "10000", "100000")) +
  ylab("Partial correlation r ") + xlab("Buffer size (m)") +
  labs(title = "(d) Distance to water > 90% persistence & Precipitation") +
  scale_fill_manual(breaks=c('distance20', 'distance500', 'distance1000', 'distance5000'), values=c("#301934", "#404240", "#a1a4a2", "#dfe0e0"), name = "Map resolution", labels = c("20 m", "500 m", "1000 m", "5000 m")) + 
  theme_classic() +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y = element_text(size=14), legend.position="none") + 
  geom_errorbar(aes(ymin=lowerR, ymax=upperR, color = factor(p)), width=.4, alpha=0.9, position=position_dodge(.9)) + scale_color_manual(breaks=c("1","2"),values=c("black","red"))

grid.arrange(ggp1, ggp2, ggp3, ggp4, as_ggplot(legend), ncol = 2, nrow =3)
