#' A wrapper function for kableExtra::kbl
#' @param Test foo
#' @keywords kable, kableExtra
#' fkbl()

fkbl <- function(df, digi = 2, big_mark = TRUE, ...){
        df %>%
                kableExtra::kbl(
                        digits = digi,
                        booktabs = TRUE, # this is for erasing the single lines in tables
                        linesep = "", # no distance per 5 lines
                        format.args = list(
                                big.mark = ifelse(
                                        big_mark,
                                        ",",
                                        "")),
                        ...)
}