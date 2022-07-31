#' @export
get_population_functions <- function(parameter_paths,number_of_individuals) {
  function_vector <- NULL
  for (path_number in seq_along(parameter_paths)) {
    path <- parameter_paths[path_number]

    function_body_strings <- paste(
      "pop_theta <- c()",
      paste0("for (i in 1:length(.theta[1, ])) {pop_theta <- append(pop_theta, rep(.theta[", path_number, ", i], ", number_of_individuals, "))}"),
      paste0(".population$setParameterValues('", path, "', pop_theta)") , sep = ";")

    function_body <- paste0("function(){",function_body_strings,"}")
    new_function <- eval(parse(text = function_body))
    function_formals <- alist(".population" = , ".theta" = , ".index" = NULL)
    formals(new_function) <- function_formals
    function_vector <- c(function_vector, new_function)
  }
  return(function_vector)
}


#' @export
population_menu <- list(
  "European" = "European_ICRP_2002",
  "BlackAmerican" = "BlackAmerican_NHANES_1997",
  "MexicanAmerican" = "MexicanAmericanWhite_NHANES_1997",
  "Asian" = "Asian_Tanaka_1996"
)



fileExtension <- function(file) {
  ex <- strsplit(basename(file), split = "\\.")[[1]]
  return(utils::tail(ex, 1))
}


#' @export
readObservedDataFile <- function(fileName,header = TRUE,encoding = "UTF-8") {
  extension <- fileExtension(fileName)
  # For some cases where data was derived from Excel,
  # <U+FEFF> is included in first variable name and needs to be removed
  forbiddenCharacters <- "\ufeff"

  if (extension %in% "csv") {
    observedData <- read.csv(fileName,
                             header = header,
                             check.names = FALSE,
                             encoding = encoding
    )
    variableNames <- names(observedData)
    variableNames[1] <- gsub(forbiddenCharacters, "", variableNames[1])
    names(observedData) <- variableNames
    return(observedData)
  }

  observedData <- read.table(fileName,
                             header = header,
                             check.names = FALSE,
                             encoding = encoding
  )
  variableNames <- names(observedData)
  variableNames[1] <- gsub(forbiddenCharacters, "", variableNames[1])
  names(observedData) <- variableNames
  return(observedData)
}
