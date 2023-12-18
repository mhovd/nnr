#' Determine correct birth date from a Norwegian person number
#'
#' @param fnr A valid person number.
#' @return A date-object with the correct birth date.
#' @importFrom lubridate ymd year
#' @importFrom dplyr case_when
#' @examples
#' fnr_to_birthdate(fnr = 19053826639)
#' @export
fnr_to_birthdate = function(fnr) {

  if (grepl("[^0-9]", fnr)) {
    stop("Person number should contain only numeric characters")
  }

  # Check length
  if (nchar(fnr) != 11) {
    stop("Person number must be 11 characters long")
  }

  # Extract the date and individual number parts
  date_part <- substr(fnr, 1, 6)
  individual_number <- as.numeric(substr(fnr, 7, 9))

  # Extract year, month, and day
  year <- substr(date_part, 5, 6)
  month <- substr(date_part, 3, 4)
  day <- substr(date_part, 1, 2)

  # Determine the century based on individual number
  # These rules may change over time, so this is a simplified version
  if (individual_number >= 0 && individual_number <= 499) {
    year_full <- paste0("19", year)
  } else if (individual_number >= 500 && individual_number <= 749 && year >= "54") {
    year_full <- paste0("18", year)
  } else if (individual_number >= 500 && individual_number <= 999 && year < "40") {
    year_full <- paste0("20", year)
  } else {
    return("Invalid individual number range")
  }

  date_string = paste(year_full, month, day, sep = "-")
  birth_date = ymd(date_string)
  return(birth_date)
}
