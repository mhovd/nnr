% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnr_to_birthdate.R
\name{fnr_to_birthdate}
\alias{fnr_to_birthdate}
\title{Determine correct birth date from a Norwegian person number}
\usage{
fnr_to_birthdate(fnr)
}
\arguments{
\item{fnr}{A valid person number.}
}
\value{
A date-object with the correct birth date.
}
\description{
Determine correct birth date from a Norwegian person number
}
\examples{
fnr_to_birthdate(fnr = 19053826639)
}
