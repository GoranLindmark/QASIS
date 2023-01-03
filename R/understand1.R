# every dataset needs to be understood. all capabilities revieled.
# This is a set of test that will help you ...
#
library(RCurl)
library(e1071)
library(tidyverse)
library(lubridate)


data <- qasis

ClassDistribution <- function(var){
    cbind(freq=table(var), percentage=prop.table(table(var))*100)
}



ClassDistribution(data$`Numeric Rating`)
ClassDistribution(data$Rating)
# ClassDistribution(data$`Project Manager`)
# ClassDistribution(data$Auditor)
ClassDistribution(data$`Audit Type`)
# ClassDistribution(data$ASP)
ClassDistribution(data$Category)
ClassDistribution(data$`Supplementary Item`)
ClassDistribution(data$Equipment)
ClassDistribution(data$'Project Manager')
ClassDistribution(data$'Auditor')
ClassDistribution(data$'Country')
ClassDistribution(data$'Audit Type')
ClassDistribution(data$'ASP')

data %>%
  group_by(year(`Audit Date`)) %>%
  summarize(antal = n())
summary(data)

# Data quality
# Country ok
# Audit ok
# Audit date ok
# Audit Type ok
# Equipment ok
# ASP needs split in country and asp
# Category ok
# Numeric Rating ok
# rating ok

# fix indata



#
# # calculate standard deviation for all attributes
# sapply(data[,1:17], sd)
#
#
# # calculate skewness for each variable
# skew <- apply(data[,12], 2, skewness)
# # display skewness, larger/smaller deviations from 0 show more skew
# print(skew)
#
# correlations <- cor(data[,1:17])
# # display the correlation matrix
#
# print("Correlation is high if larger than +/- 75%")
# print(correlations)
