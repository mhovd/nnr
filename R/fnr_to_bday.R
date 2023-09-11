#' Determine correct birth date from a Norwegian person number
#'
#' @param fnr A correct person number.
#' @return A date-object with the correct birth date.
#' @importFrom lubridate dmy year
#' @importFrom dplyr case_when
#' @examples
#' fnr_to_bday(fnr = 19053826639)
fnr_to_bday = function(fnr) {

  if (grepl("[^0-9]", fnr)) {
    stop("Person number should contain only numeric characters")
  }

  # Check length
  if (nchar(fnr) != 11) {
    stop("Person number must be 11 characters long")
  }

  # Extract components
  birth_date <- dmy(substr(fnr, 1, 6))
  cipher <- as.numeric(substr(fnr, 7, 9))

  # Determine era from cipher
  era <- case_when(
    cipher %in% 500:749 ~ "18",
    cipher %in% 0:499 | cipher %in% 900:999 ~ "19",
    TRUE ~ "20"  # Assuming the default era is 20th century
  )

  # What is the guessed era?
  birth_year <- as.character(year(birth_date))

  if (substr(birth_year, 1, 2) != era) {
    birth_year <- paste0(era, substr(birth_year, 3, 4))
    year(birth_date) <- as.integer(birth_year)
  }

  return(birth_date)
}

fnr_to_bday(fnr = 19053826639)
