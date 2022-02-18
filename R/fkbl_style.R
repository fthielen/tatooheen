#' A wrapper function for kableExtra::kable_styling() to be used after tatooheen::fkbl()
#' @param fkbl A piped object from tatooheen::kbl()
#' @param scale If `TRUE`, table will be scaled down
#' @param boots_opts Standard bootstrap options for HTML. Can be overwritten.
#' @param latx_opts Standard options for PDF. Can be overwritten.
#' @param ... Further arguments to `kableExtra::kable_styling()`
#' @export fkbl_style

fkbl_style <- function(fkbl, scale = FALSE,
                       boots_opts = c("striped", "condensed", "hover"),
                       latx_opts = c("striped", "HOLD_position"),
                       ...) {

        requie("kableExtra")

        if(knitr::pandoc_to("docx")) {
                fkbl
        } else {

        fkbl %>%
                kableExtra::kable_styling(
                        # HTML output
                        bootstrap_options = boots_opts,
                        # PDF output
                        latex_options = if (scale) {
                                c(latx_opts, "scale_down")

                        } else {
                                latx_opts
                        },
                        # Not full width
                        full_width = F,
                        ...
                ) %>%
                        kableExtra::scroll_box(., width = "100%", box_css = "border: 0px;")
                }
}
