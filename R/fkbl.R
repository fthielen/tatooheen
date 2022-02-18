#' A wrapper function for kableExtra::kbl()
#' @param df A data.frame to display
#' @param digi The number of digits to show in printed table.
#' @param big_mark Set to control if big marks should be used (e.g. 21,000). Be aware that this also applies for years (i.e. 2,021 instead of 2021). To avaoid the latter convert to text with `as.character()` first.
#' @param ... Further arguments for kableExtra::kbl()
#' @export fkbl

fkbl <- function(df, digi = 2, big_mark = TRUE,
                 col.names = names(df), ...){

        require("dplyr")
        require("kableExtra")

        if(knitr::pandoc_to("docx")) {
                df %>% flextable::flextable()
        } else {

        df %>%
                kableExtra::kbl(
                        col.names = col.names,
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
}
