parse_data_file <- function(file_name, na = ".", ...) {
    if (!file.exists(file_name)) {
        stop("File not found: " + file_name)
    }
    raw_data <- readr::read_csv(file_name, na = na, ...)

    names(raw_data) <- tolower(names(raw_data))
    # stopifnot(names(raw_data)[1] == "#id", "file_name must have a column named '#id'")
    names(raw_data)[1] <- "id"
    # stopifnot(all(c("id", "time", "dose", "out", "occasion") %in% names(raw_data)), "must at least have id, time, dose, out and occasion columns to proceed with the check")
    return(raw_data)
}

get_demographics <- function(parsed_data, sub = NA, occ = 1) {
    parsed_data %>%
        {
            if (!is.na(sub)) filter(., id == sub) else .
        } %>%
        filter(occasion == occ) %>%
        select(-c(time, out, occasion)) %>%
        distinct() %>%
        return()
}

get_concentrations <- function(parsed_data, occ = 1) {
    parsed_data %>%
        # filter(is.na(dose)) %>%
        filter(occasion == occ) %>%
        select(c(id, time, out)) %>%
        mutate(out = replace(out, out == 999, NA)) %>%
        group_by(time, id) %>%
        spread(key = id, value = out)
}