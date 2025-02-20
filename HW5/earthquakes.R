#The dataset is downloaded from: 
#https://www.kaggle.com/datasets/shreyasur965/recent-earthquakes
rm(list=ls()) #remove all the information stored in the current environment.

#Be sure to set the correct working directory by doing: 
#Session -> Set Working Directory -> Choose Directory.
data <- read.csv("earthquakes.csv")
x <- data$distanceKM[which(data$distanceKM > 0)] #zeros are removed.
x #use this to compute confidence intervals for the mean.
