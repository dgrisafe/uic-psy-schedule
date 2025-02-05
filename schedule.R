library(tidyverse)

# load constraints
source('constraints_calendar.R')
source('constraints_hard.R')
source('constraints_soft.R')

# load csv file containing resident call requests
data <- read.csv('20240701_fall.csv', colClasses = c("character", "integer", rep("Date", 36), rep("factor", 2)))

glimpse(data)