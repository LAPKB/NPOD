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
number_of_individuals <- 100
number_of_occassions <- raw_data$occasion %>% max()

individuals <- vector(mode = "list", length = number_of_individuals)

for (i in 1:number_of_individuals) {
  if (demographics$gender[i] == "Female") {
    their_gender <- Gender$Female
  } else if (demographics$gender[i] == "Male") {
    their_gender <- Gender$Male
  }
  individual_chars <- createIndividualCharacteristics(
    species = Species$Human, population = HumanPopulation$European_ICRP_2002,
    gender = their_gender,
    weight = demographics$"organism|weight"[i], weightUnit = "kg",
    height = demographics$"organism|height"[i], heightUnit = "dm",
    age = demographics$"organism|age"[i], ageUnit = "year(s)"
  )
  individuals[[i]] <- individual_chars
}


parameter_paths <- c("Liver Enzyme|Reference concentration")
population_functions <- get_population_functions(parameter_paths, number_of_individuals)

occasions <- vector(mode = "list", length = number_of_occassions)


for (i in 1:number_of_occassions) {
  pkdata_file <- paste0("conc_", i, ".csv")
  write.csv(raw_data %>% get_concentrations(occ = i), pkdata_file, row.names = FALSE)
  readLines(pkdata_file) %>%
    gsub("\"", "", .) %>%
    gsub("time", "Time", .) %>%
    writeLines(pkdata_file)
  occasions[[i]] <- NPOD(sim_file, pkdata_file, params, individuals, population_functions, c0 = 0.1, c1 = 0.05, cache_folder_name = "iov")
}

posteriors <- vector(mode = "list", length = number_of_occassions)

for (i in 1:number_of_occassions) {
  posteriors[[i]] <- occasions[[i]] %>% posterior()
}
