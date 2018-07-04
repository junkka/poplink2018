#' Integer date to Date object
#' 
#' @param x integer date string
#' @export

as_date <- function(x) as.Date(as.character(x), format = "%Y%m%d")


#' Difference in fraction of years between dates
#' 
#' @param x integer date string
#' @param y integer date string
#' @export

diff_years <- function(x, y){ 
  # asd
  lubridate::as.duration(lubridate::interval(y, x)) / lubridate::dyears(1)
}

#' Difference in months between dates
#' 
#' @param x integer date string
#' @param y integer date string
#' @export

diff_months <- function(x,y){
  lubridate::as.duration(lubridate::interval(y, x)) / lubridate::ddays(30.43688)
}

#' Check if a date is between two other dates
#' 
#' @param start start date
#' @param end end date
#' @param x point date
#' @export

is_between <- function(start, end, x){
  x %within% lubridate::interval(start, end) | (x - lubridate::dyears(1))  %within% lubridate::interval(start, end)
}

#' Check if episodes overlapp
#' 
#' @param start of episode 1
#' @param end of eposide 2
#' @param start2 of episode 2
#' @param end2 of episode 2
#' @export

overlapping <- function(start, end, start2, end2) {
  int1 <- lubridate::interval(start, end)
  int2 <- lubridate::interval(start2, end2)
  lubridate::int_overlaps(int1, int2)
}