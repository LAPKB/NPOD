rm(list = ls())
setwd("inst")
library(NPOD)
library(ospsuite) # PK-Sim R toolbox
library(purrr)
sim_file <- "data/Normal Clearance Individual.pkml"
data_file <- "data/iov_v2.csv"
params <- vector(mode = "list", length = 2)
params[[1]] <- c(0.1) # min
params[[2]] <- c(100) # max
raw_data <- parse_data_file(data_file)
demographics <- raw_data %>% get_demographics()
concentrations <- raw_data %>% get_concentrations()
