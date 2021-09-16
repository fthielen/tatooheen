#' A function to write pretty prices in `bookdown` reports
#' @param x A number to be printed
#' @param currency The name of the currency
#' @export p_price
#'

p_price <- function(x, currency = "EUR"){
        paste(prettyNum(x,
                  big.mark = ","), currency)
}
