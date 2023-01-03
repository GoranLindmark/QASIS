# every dataset needs to be understood. all capabilities reviled.
# This is a set of test that will help you ...
#
library(RCurl)
library(e1071)
library(tidyverse)
library(lubridate)

load("qasisData.RData")

data <- qasis

classDistribution <- function(var){
    cbind(freq=table(var), percentage=proportions(table(var))*100)
}



classDistribution(data$num_rating)
classDistribution(data$rating)
classDistribution(data$`project manager`)
classDistribution(data$auditor)
classDistribution(data$`audit type`)
classDistribution(data$aspName)
classDistribution(data$category)
classDistribution(data$`supplementary item`)
classDistribution(data$equipment)
classDistribution(data$'project manager')
classDistribution(data$'auditor')
classDistribution(data$'country')

# data i variablerna 'project manager', auditor,  'supplementary item' can not be used
# due to bad quality (different spellings) and so on)
#

data <- data %>%
  select(-'project manager',- auditor, -'supplementary item')




#
# # calculate standard deviation for all attributes
sapply(data[,6], sd)
#
#
# # calculate skewness for each variable
skew <- apply(data[,6], 2, skewness)
# # display skewness, larger/smaller deviations from 0 show more skew
print(skew)


equipmentLook <-
  data$equipment %>%
  unique() %>%
  as_tibble() %>%
  mutate(equipmentIndex = 1:nrow(.))
data <- left_join(data, equipmentLook, by= c("equipment" = "value"))

countryLook <-
  data$country %>%
  unique() %>%
  as_tibble() %>%
  mutate(countryIndex = 1:nrow(.))
data <- left_join(data, countryLook, by= c("country" = "value"))

aspLook <-
  data$aspName %>%
  unique() %>%
  as_tibble() %>%
  mutate(aspIndex = 1:nrow(.))
data <- left_join(data, aspLook, by= c("aspName" = "value"))

correlations <- cor(data[,5:9])*100
# # display the correlation matrix
#
print("Correlation is high if larger than +/- 75%")
print(correlations)

library(fpp3)

data %>%
  group_by(date, equipment) %>%
  summarize(averageFaults = mean(num_rating)
           ) %>%
  ungroup() -> data

data %>%
as_tsibble(index = date, key = equipment) %>%
  filter_index("2008-01-01" ~ "2010-12-30") %>%
  autoplot(averageFaults)


data %>%
  group_by(date, `audit type`) %>%
  summarize(averageFaults = mean(num_rating)
  ) %>%
  ungroup() -> data

data %>%
  as_tsibble(index = date, key = `audit type`) %>%
  filter_index("2008-01-01" ~ "2010-12-30") %>%
  autoplot(averageFaults)

