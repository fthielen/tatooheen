#' A set of functions to convert time
#' @description Functions to convert month to days, days, to month, years to days, days to years, years to months and months to years. Assuming that: one month contains 30.43 days on average, one year contains 365.25 days on average, and one year contains 52.17 weeks on average.
#' @param days The days to convert
#' @param months The monthts to convert
#' @param years The years to convert
#' @export

# Days to month
d2m <- function(days){
        days / 30.4
}

# Months to days
m2d <- function(months){
        months * 30.4
}

# Years to day
y2d <- function(years){
        years * 365.25
}

# Days to years
d2y <- function(days){
        days / 365.25
}

# Months to years
m2y <- function(months){
        months / 12
}

# Years to week
y2w <- function(years){
        years * 52.17
}

# Weeks to years
w2y <- function(weeks){
        weeks / 52.17
}
