#' A function to write pretty prices in `bookdown` reports
#' @param x A number to be printed
#' @param currency The name of the currency
#' @param digi Number of digits
#' @param ... Extra arguments for `formatC()`
#' @export p_price
#'

p_price <- function(x, digi = 2, currency = "EUR", ...){
        paste(formatC(x,
                  big.mark = ",",
                  format = "f",
                  digits = digi, ...), currency)
}
